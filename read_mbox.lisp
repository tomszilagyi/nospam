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
  body)

(defvar *linebuf* (make-array *maxlen* :element-type '(unsigned-byte 8)))
(defun read-line-from-stream (str)
  (do ((o (read-byte str nil 'eof) (read-byte str nil 'eof))
       (i 0 (1+ i)))
      ((or (eql o 'eof) (eql o 10))
       (if (eql o 'eof)
	   'eof
	   (sb-ext:octets-to-string *linebuf* :external-format *encoding* :end i)))
    (setf (aref *linebuf* i) o)))


; tokenize:
; return list of strings that are separated by sep string in str
(defun tokenize-1 (str str-len sep sep-len acc)
  (let ((pos (search sep str)))
    (if pos
	(tokenize-1 (subseq str (+ pos sep-len))
		    (- str-len pos sep-len)
		    sep
		    sep-len
		    (cons (subseq str 0 pos) acc))
	(nreverse (cons str acc)))))

(defun tokenize (str sep)
  (let ((str-len (length str))
	(sep-len (length sep)))
    (tokenize-1 str str-len sep  sep-len nil)))


(defun get-content-type (msg)
  (let ((ct-str (or (cdr (car (member 'content-type (message-header msg) :key #'car)))
		    "text/plain; charset=us-ascii")))
    (mapcar #'(lambda (s) (tokenize s "=")) (tokenize ct-str "; "))))

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
		 (cdr (car (member 'content-transfer-encoding (message-header m) :key #'car))))
	     "8bit"))


; receives two-character substring with hex code of character
(defun qp-decode-char (substr encoding)
  (let ((code (parse-integer substr :radix 16)))
    (sb-ext:octets-to-string (make-array '(1)
					 :element-type '(unsigned-byte 8)
					 :initial-contents (list code))
			     :external-format encoding)))

(defun qp-decode-1 (line i len encoding acc)
  (cond ((= i len) (format nil "~A~%" acc))
	((= i (1- len))
	 (if (char= #\= (char line i))
	     acc
	     (format nil "~A~C~%" acc (char line i))))
	((char= (char line i) #\=)
	 (qp-decode-1 line (+ i 3) len encoding
		      (concatenate 'string acc
				   (qp-decode-char (subseq line (1+ i) (+ i 3)) encoding))))
	(t (qp-decode-1 line (1+ i) len encoding
			(concatenate 'string acc (subseq line i (1+ i)))))))

(defun qp-decode (line msg)
  (let ((len (length line)))
    (if (= 0 len)
	(format nil "~%")
	(qp-decode-1 line 0 (length line) (get-encoding msg) ""))))

(defun content-transfer-decode (line msg)
  (case (message-content-transfer-encoding msg)
    ((:7bit :8bit :binary) (format nil "~A~%" line))
    (:quoted-printable (qp-decode line msg))
    (otherwise (format nil "~A: ~A~%" (message-content-transfer-encoding msg) line))))


(defvar *last-from* nil)
(defun from? (line)
  (string= (ignore-errors (subseq line 0 5)) "From "))

(defun msg-empty? (msg)
  (and (null (message-from msg))
       (null (message-header msg))
       (null (message-body msg))))

(defun read-mail-from-stream (str)
  (do ((line (or *last-from* (read-line-from-stream str)) (read-line-from-stream str))
       (status 'init)
       (msg (make-message)))
      ((eql line 'eof) (if (msg-empty? msg) nil msg))

    (setf *last-from* nil)

    (cond
      ;; start of new mail
      ((and (eq status 'init) (from? line))
       (setf (message-from msg) line
	     *encoding* :ascii
	     status 'header))

      ;; end of header
      ((and (eq status 'header) (= (length line) 0))
       (setf (message-header msg) (nreverse (message-header msg))
	     (message-content-transfer-encoding msg) (get-content-transfer-encoding msg)
	     (message-content-type msg) (get-content-type msg)
	     *encoding* (get-encoding msg)
	     status 'body))

      ;; header continuation line
      ((and (eq status 'header) (in (char line 0) #\Space #\Tab))
       (let ((v+ (string-trim '(#\Space #\Tab) line))
	     (tail (cdr (message-header msg)))
	     (k (car (car (message-header msg))))
	     (v (cdr (car (message-header msg)))))
	 (setf (message-header msg) (cons (cons k (concatenate 'string v " " v+)) tail))))

      ;; header line
      ((eq status 'header)
       (let* ((p (position #\: line))
	      (k (intern (string-upcase (subseq line 0 p))))
	      (v (or (ignore-errors (subseq line (+ p 2))) "")))
	 (setf (message-header msg) (cons (cons k v) (message-header msg)))))

      ;; body line
      ((eq status 'body)
       (if (from? line)
	   (progn (setf *last-from* line)
		  (return msg))
	   (setf (message-body msg) (concatenate 'string (message-body msg)
						 (content-transfer-decode line msg))))))

    (format t "~A: ~A~%" status line)))

(defun read-mailbox (path mailbox)
  (with-open-file (str (format nil "~A/~A" path mailbox)
		       :direction :input
		       :element-type '(unsigned-byte 8))
    (do ((m (read-mail-from-stream str) (read-mail-from-stream str)))
	((null m))
      (format t "msg: ~A~%" m)
      )))

(read-mailbox "/xenon/tom/src/lisp/nospam/mail" "mbox1")

;    (do ((line (read-line-from-stream str) (read-line-from-stream str)))
;	((eql line 'eof))
;      (format t "line: ~A~%" line))))

; http://www.cliki.net/CloserLookAtCharacters
; http://en.wikipedia.org/wiki/MIME