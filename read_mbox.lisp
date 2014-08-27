(defparameter *maxlen* 1024)
(defparameter *encoding* :ascii)

(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
		     choices)))))

(defstruct message
  from
  header
  content-transfer-encoding
  content-type
  parent
  boundary
  body) ; a body is either a string or a list of message structures

(defvar *linebuf* (make-array *maxlen* :element-type '(unsigned-byte 8)))
(defun read-line-from-stream (str)
  (do ((o (read-byte str nil 'eof) (read-byte str nil 'eof))
       (i 0 (1+ i)))
      ((or (eql o 'eof) (eql o 10))
       (if (eql o 'eof)
	   'eof
	   (sb-ext:octets-to-string *linebuf* :external-format *encoding* :end i)))
    (setf (aref *linebuf* i) o)))

(defun trim-quot (str)
  (string-trim "\"" str))

(defun trim-blank (str)
  (string-trim '(#\Space #\Tab) str))

; tokenize:
; return list of strings that are separated by sep string in str
(defun tokenize-1 (str str-len sep sep-len acc)
  (let ((pos (search sep str))
	(pos-quot (search "\"" str)))
    (if (and pos (or (not pos-quot) (< pos pos-quot)))
	(tokenize-1 (subseq str (+ pos sep-len))
		    (- str-len pos sep-len)
		    sep
		    sep-len
		    (cons (trim-quot (subseq str 0 pos)) acc))
	(nreverse (cons (trim-quot str) acc)))))

(defun tokenize (str sep)
  (let ((str-len (length str))
	(sep-len (length sep)))
    (tokenize-1 str str-len sep sep-len nil)))

(defun get-content-type (msg)
  (let ((ct-str (or (cdr (car (member 'content-type (message-header msg) :key #'car)))
		    "text/plain; charset=us-ascii")))
    (mapcar #'(lambda (s)
		(tokenize (trim-blank s) "="))
	    (tokenize ct-str ";"))))

(defun get-field (msg getter-fun default)
  (intern (string-upcase (or (funcall getter-fun msg) default)) "KEYWORD"))

(defun get-encoding (msg)
  (get-field msg
	     #'(lambda (m)
		 (cadr (car (member "charset" (message-content-type m)
				    :test #'string-equal :key #'car))))
	     "us-ascii"))

(defun get-content-transfer-encoding (msg)
  (get-field msg
	     #'(lambda (m)
		 (cdr (car (member 'content-transfer-encoding (message-header m)
				   :key #'car))))
	     "8bit"))

; returns MIME boundary in case of a multipart/* message and NIL otherwise
(defun get-mime-boundary (msg)
  (let ((ct (get-content-type msg)))
    (if (eql 0 (search "multipart" (car (car ct)) :test #'char-equal))
	(trim-quot (cadr (car (member "boundary" ct :test #'string-equal :key #'car)))))))

(defun decode-hexdigit (val)
  (cond ((and (>= val (char-code #\0)) (<= val (char-code #\9)))
	 (- val (char-code #\0)))
	((and (>= val (char-code #\A)) (<= val (char-code #\F)))
	 (+ 10 (- val (char-code #\A))))
	((and (>= val (char-code #\a)) (<= val (char-code #\f)))
	 (+ 10 (- val (char-code #\a))))))

(defun qp-decode-octet (arr i)
  (+ (* 16 (decode-hexdigit (aref arr (1+ i))))
     (decode-hexdigit (aref arr (+ i 2)))))

(defvar *qp-buf* #())
(defun qp-decode-post (arr encoding nl)
  (setf *qp-buf* (concatenate 'vector *qp-buf* arr))
  (if nl
      (let ((s (sb-ext:octets-to-string
		(coerce *qp-buf* '(vector (unsigned-byte 8)))
		:external-format encoding)))
	(setf *qp-buf* #())
	(format nil "~A~%" s))
      ""))

(defun qp-decode-1 (arr i j len encoding)
  (cond ((= i len) (qp-decode-post (subseq arr 0 j) encoding t))
	((= i (1- len))
	 (if (= 61 (aref arr i))
	     (qp-decode-post (subseq arr 0 j) encoding nil)
	     (progn
	       (setf (aref arr j) (aref arr i))
	       (qp-decode-post (subseq arr 0 (1+ j)) encoding t))))
	((= 61 (aref arr i))
	 (setf (aref arr j) (qp-decode-octet arr i))
	 (qp-decode-1 arr (+ i 3) (1+ j) len encoding))
	(t
	 (setf (aref arr j) (aref arr i))
	 (qp-decode-1 arr (1+ i) (1+ j) len encoding))))


(defun qp-decode (line encoding)
  (let ((len (length line)))
    (if (= 0 len)
	(format nil "~%")
	(qp-decode-1 (sb-ext:string-to-octets line :external-format encoding)
		     0 0 (length line) encoding))))


(defun content-transfer-decode (line msg)
  (let ((encoding (get-encoding msg)))
    (case (message-content-transfer-encoding msg)
      ((:7bit :8bit :binary) (format nil "~A~%" line))
      (:quoted-printable (qp-decode line encoding))
      (otherwise (format nil "~A: ~A~%" (message-content-transfer-encoding msg) line)))))


(defvar *last-from* nil)
(defun from? (line)
  (string= (ignore-errors (subseq line 0 5)) "From "))

(defun msg-empty? (msg)
  (or (null msg)
      (and (null (message-from msg))
	   (null (message-header msg))
	   (null (message-body msg)))))

(defun msg-embedded-rfc822? (msg)
  (string-equal (car (car (message-content-type msg))) "message/rfc822"))

; find appropriate parent message when reaching end of given MIME boundary
(defun parent-msg-from-boundary (msg boundary)
  ;(format t "msg-boundary: ~A  boundary: ~A~%" (message-boundary msg) boundary)
  (if (null (message-parent msg))
      msg
      (if (or (string= boundary (message-boundary msg))
	      (msg-embedded-rfc822? msg))
	  (parent-msg-from-boundary (message-parent msg) boundary)
	  msg)))

(defun read-mail-from-stream (str)
  (do ((line (or *last-from* (read-line-from-stream str)) (read-line-from-stream str))
       (status 'init)
       (msg nil)
       (boundary nil))
      ((eql line 'eof) (if (msg-empty? msg) nil msg))

    (setf *last-from* nil)

    (cond
      ;; start of new mail
      ((and (eq status 'init) (from? line))
       (setf msg (make-message)
	     (message-from msg) line
	     *encoding* :ascii
	     status 'header))

      ;; end of header
      ((and (eq status 'header) (= (length line) 0))
       (setf (message-header msg) (nreverse (message-header msg))
	     (message-content-transfer-encoding msg) (get-content-transfer-encoding msg)
	     (message-content-type msg) (get-content-type msg)
	     (message-boundary msg) (get-mime-boundary msg)
	     *encoding* (get-encoding msg)
	     status 'body)
       (if (message-boundary msg)
	   (setf boundary (message-boundary msg)))
;       (if (not (eql (message-content-transfer-encoding msg) :quoted-printable))
;	   (setf *encoding* (get-encoding msg)))
       (format t "(get-mime-boundary msg): ~A~%" boundary)
       (if (msg-embedded-rfc822? msg)
	   ;; embedded message -- add new child msg
	   (let ((parent msg))
	     (setf (message-body parent) (list (make-message))
		   msg (car (message-body msg))
		   (message-parent msg) parent
		   status 'header))))

      ;; header continuation line
      ((and (eq status 'header) (in (char line 0) #\Space #\Tab))
       (format t "~A: ~A~%" status line)
       (let ((v+ (trim-blank line))
	     (tail (cdr (message-header msg)))
	     (k (car (car (message-header msg))))
	     (v (cdr (car (message-header msg)))))
	 (setf (message-header msg) (cons (cons k (concatenate 'string v " " v+)) tail))))

      ;; header line
      ((eq status 'header)
       (format t "~A: ~A~%" status line)
       (let* ((p (position #\: line))
	      (k (intern (string-upcase (subseq line 0 p))))
	      (v (or (ignore-errors (subseq line (+ p 2))) "")))
	 (setf (message-header msg) (cons (cons k v) (message-header msg)))))

      ;; body line
      ((eq status 'body)
       (cond
	 ;; start of new message
	 ((from? line)
	  (setf *last-from* line)
	  (return msg))

	 ;; end of MIME part
	 ((and boundary (eql 2 (search (concatenate 'string boundary "--") line)))
	  (format t "*** end of MIME parts with boundary: ~A~%" boundary)
	  (setf msg (message-parent msg))
	  (setf msg (parent-msg-from-boundary msg boundary)
		boundary (message-boundary msg))

	  (format t "boundary: ~A~%" boundary)

	  (if (null (message-parent msg))
	      (return msg)))

	 ;; start of new MIME part
	 ((and boundary (eql 2 (search boundary line)))
	  (format t "start of new MIME part, boundary: ~A~%" boundary)
	  ;; if current msg's boundary is this boundary, add child to msg
	  ;; if current msg's parent's boundary is this boundary, add sibling to msg
	  (cond ((string= (message-boundary msg) boundary)
		 (format t "Adding child to msg~%")
		 (let* ((parent msg)
			(new-msg (make-message))
			(new-body (if (listp (message-body parent))
				      (append (message-body parent) (list new-msg))
				      (list new-msg))))
		   (setf (message-body parent) new-body
			 msg new-msg
			 (message-parent msg) parent)))
		((string= (message-boundary (message-parent msg)) boundary)
		 (format t "Adding sibling to msg~%")
		 (let ((parent (message-parent msg))
		       (new-msg (make-message)))
		   (setf (message-body parent) (append (message-body parent) (list new-msg))
			 (message-parent new-msg) parent
			 msg new-msg))))
	  (setf status 'header))

	 ;; regular body line
	 ((and (not (message-p (message-body msg)))
	       (not (eql (message-content-transfer-encoding msg) :base64)))
	  (format t "~A: ~A~%" status line)
	  (setf (message-body msg) (concatenate 'string (message-body msg)
						(content-transfer-decode line msg)))))))
    ))

(defun read-mailbox (path mailbox)
  (with-open-file (str (format nil "~A/~A" path mailbox)
		       :direction :input
		       :element-type '(unsigned-byte 8))
    (do ((m (read-mail-from-stream str) (read-mail-from-stream str)))
	((null m))
      (format t "msg: ~A~%" m)      
      )))


;; We must be able to print circular structure without getting stoned
(setf *print-circle* t)

(read-mailbox "/xenon/tom/src/lisp/nospam/mail" "choke4")
;(read-mailbox "/xenon/tom/src/lisp/nospam/mail" "choke3")
;(read-mailbox "/xenon/tom/src/lisp/nospam/mail" "choke2")
;(read-mailbox "/xenon/tom/src/lisp/nospam/mail" "choke1")
;(read-mailbox "/xenon/tom/src/lisp/nospam/mail" "h1")
;(read-mailbox "/xenon/tom/src/lisp/nospam/mail" "many-atts")
;(read-mailbox "/xenon/tom/src/lisp/nospam/mail" "multipart-mixed-1")
;(read-mailbox "/xenon/tom/src/lisp/nospam/mail" "embedded-rfc822")
;(read-mailbox "/xenon/tom/src/lisp/nospam/mail" "multipart-alt-1")
;(read-mailbox "/xenon/tom/src/lisp/nospam/mail" "mbox1")
;(read-mailbox "/xenon/tom/src/lisp/nospam/mail" "mbox2")

;(read-mailbox "/xenon/tom/mail" "Robot.proj")

;    (do ((line (read-line-from-stream str) (read-line-from-stream str)))
;	((eql line 'eof))
;      (format t "line: ~A~%" line))))

; http://www.cliki.net/CloserLookAtCharacters
; http://en.wikipedia.org/wiki/MIME