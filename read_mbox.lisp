;(load "macros.lisp")

(defparameter *maxlen* 1024)
(defparameter *encoding* :ascii)

(defstruct message
  from
  header
  content-transfer-encoding
  content-type
  parent
  boundary
  body) ; a body is either a string or a list of message structures

(defun octets-to-str (buf encoding &key (end nil))
  (or (ignore-errors
	(handler-bind ((sb-int:character-decoding-error
			#'(lambda (c)
			    (declare (ignore c))
			    (invoke-restart 'use-value #\?))))
	  (sb-ext:octets-to-string buf :external-format encoding :end end)))
      ""))

(defvar *linebuf* (make-array *maxlen* :element-type '(unsigned-byte 8)))
(defun read-line-from-stream (str)
  (do ((o (read-byte str nil 'eof) (read-byte str nil 'eof))
       (i 0 (1+ i)))
      ((or (eql o 'eof) (eql o 10) (= i *maxlen*))
       (if (eql o 'eof)
	   'eof
	   (octets-to-str *linebuf* *encoding* :end i)))
    (setf (aref *linebuf* i) o)))

(defun trim-quot (str)
  (string-trim "\"" str))

(defun trim-blank (str)
  (string-trim '(#\Space #\Tab #\Newline #\Return) str))

; split:
; return list of strings that are separated by sep string in str
(defun split-1 (str str-len sep sep-len acc)
  (let* ((pos (search sep str))
	 (pos-quot (search "\"" str))
	 (pos-quot2 (or (and pos-quot (search "\"" str :start2 (1+ pos-quot))) 0)))
    (declare (type (or fixnum null) pos-quot))
    (if (and pos (or (not pos-quot)
		     (< pos pos-quot)
		     (< pos-quot pos-quot2 pos)))
	(split-1 (subseq str (+ pos sep-len))
		 (- str-len pos sep-len)
		 sep
		 sep-len
		 (cons (subseq str 0 pos) acc))
	(nreverse (cons str acc)))))

(defun split (str sep)
  (let ((str-len (length str))
	(sep-len (length sep)))
    (split-1 str str-len sep sep-len nil)))

(defun get-content-type (msg)
  (let ((ct-str (or (cdr (car (member 'content-type (message-header msg) :key #'car)))
		    "text/plain; charset=us-ascii")))
    (mapcar #'(lambda (s)
		(split (trim-blank s) "="))
	    (split ct-str ";"))))

(defun get-field (msg getter-fun default)
  (intern (trim-quot (trim-blank (string-upcase (or (funcall getter-fun msg) default))))
	  "KEYWORD"))

(defun get-encoding-1 (msg)
  (get-field msg
	     #'(lambda (m)
		 (cadr (car (member "charset" (message-content-type m)
				    :test #'string-equal :key #'car))))
	     "us-ascii"))

(defun fix-encoding (enc)
  (cond ((in enc :big5 :gb2312 :default_charset :x-unknown) :us-ascii)
	((eq enc :windows-1254) :iso-8859-9)
	(t enc)))

(defun get-encoding (msg)
  (fix-encoding (get-encoding-1 msg)))

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
	 (+ 10 (- val (char-code #\a))))
	(t (char-code #\A))))

(defun qp-decode-octet (arr i)
  (logand 255 (+ (* 16 (decode-hexdigit (aref arr (1+ i))))
		 (decode-hexdigit (aref arr (+ i 2))))))

(defvar *qp-buf* #())
(defun qp-decode-post (arr encoding nl)
  (setf *qp-buf* (concatenate 'vector *qp-buf* arr))
  (if nl
      (let ((s (octets-to-str
		(coerce *qp-buf* '(vector (unsigned-byte 8)))
		encoding)))
	(setf *qp-buf* #())
	(format nil "~A~%" s))
      ""))

(defun qp-decode-1 (arr i j len encoding)
  (cond ((= i len) (qp-decode-post (subseq arr 0 j) encoding t))
	((= i (1- len))
	 (if (= 61 (aref arr i))
	     (qp-decode-post (subseq arr 0 j) encoding
			     (or (= i 0) (= 61 (aref arr (1- i)))))
	     (progn
	       (setf (aref arr j) (aref arr i))
	       (qp-decode-post (subseq arr 0 (1+ j)) encoding t))))
	((and (< i (- len 2)) (= 61 (aref arr i)) (not (= 61 (aref arr (1+ i)))))
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


(defparameter *base64-encode-table*
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=")

(defparameter *base64-decode-table*
  (let ((da (make-array (list 256)
			:element-type 'integer
			:initial-element 0)))
    (loop for character across *base64-encode-table*
       for index from 0 below 64
       do (setf (elt da (char-code character)) index))
    da))

(defun base64-decode-1 (string)
  (let ((result (make-array (+ 2 (* 3 (truncate (/ (length string) 4))))
			    :element-type '(unsigned-byte 8)))
	(ridx 0))
    (loop for schar across string
       for svalue = (elt *base64-decode-table* (char-code schar))
       with bitstore = 0
       with bitcount = 0
       do (unless (null svalue)
	    (setf bitstore (logior (ash bitstore 6) svalue))
	    (incf bitcount 6)
	    (when (>= bitcount 8)
	      (decf bitcount 8)
	      (setf (elt result ridx)
		    (logand (ash bitstore (- bitcount)) #xFF))
	      (incf ridx)
	      (setf bitstore (logand bitstore #xFF)))))
    (subseq result 0 ridx)))

(defun base64-decode (string encoding)
  (octets-to-str (coerce (base64-decode-1 string) '(vector (unsigned-byte 8))) encoding))


(defun content-transfer-decode (line msg)
  (let ((encoding (get-encoding msg)))
    (case (message-content-transfer-encoding msg)
      ((:7bit :8bit :binary) (format nil "~A~%" line))
      (:quoted-printable (qp-decode line encoding))
      (:base64 (base64-decode line encoding))
      (otherwise (format nil "~A: ~A~%" (message-content-transfer-encoding msg) line)))))


(defun mime-decode-word (encoding charset enctext)
  (let ((charset-sym (fix-encoding (intern (string-upcase charset) "KEYWORD"))))
    (cond ((string= encoding "Q")
	   (replace-all (remove #\Newline (qp-decode enctext charset-sym)) "_" " "))
	  ((string= encoding "B")
	   (base64-decode enctext charset-sym))
	  (t enctext))))

; decode MIME encoded words in a line, return decoded string
(defun mime-decode-line (line)
  (or (ignore-errors
	(let* ((start-pos (search "=?" line))
	       (encoding-pos (search "?" line :start2 (+ 2 start-pos)))
	       (enctext-pos (search "?" line :start2 (1+ encoding-pos)))
	       (end-pos (search "?=" line :start2 (1+ enctext-pos)))
	       (pre-str (subseq line 0 start-pos))
	       (charset (subseq line (+ 2 start-pos) encoding-pos))
	       (encoding (subseq line (1+ encoding-pos) enctext-pos))
	       (enctext (subseq line (1+ enctext-pos) end-pos))
	       (post-str (subseq line (+ 2 end-pos)))
	       (post-str1 (if (and (> (length post-str) 3)
				   (= 0 (or (search " =?" post-str) -1)))
			      (subseq post-str 1) ;; eat spaces between enc-words
			      post-str)))
	  (concatenate 'string
		       pre-str
		       (mime-decode-word encoding charset enctext)
		       (mime-decode-line post-str1))))
      line))


(defparameter *named-html-entities*
  '((|quot| #\u0022) (|amp| #\u0026) (|apos| #\u0027) (|lt| #\u003C) (|gt| #\u003E) (|nbsp| #\u00A0)
    (|iexcl| #\u00A1) (|cent| #\u00A2) (|pound| #\u00A3) (|curren| #\u00A4) (|yen| #\u00A5) (|brvbar| #\u00A6)
    (|sect| #\u00A7) (|uml| #\u00A8) (|copy| #\u00A9) (|ordf| #\u00AA) (|laquo| #\u00AB) (|not| #\u00AC)
    (|shy| #\u00AD) (|reg| #\u00AE) (|macr| #\u00AF) (|deg| #\u00B0) (|plusmn| #\u00B1) (|sup2| #\u00B2)
    (|sup3| #\u00B3) (|acute| #\u00B4) (|micro| #\u00B5) (|para| #\u00B6) (|middot| #\u00B7) (|cedil| #\u00B8)
    (|sup1| #\u00B9) (|ordm| #\u00BA) (|raquo| #\u00BB) (|frac14| #\u00BC) (|frac12| #\u00BD) (|frac34| #\u00BE)
    (|iquest| #\u00BF) (|Agrave| #\u00C0) (|Aacute| #\u00C1) (|Acirc| #\u00C2) (|Atilde| #\u00C3) (|Auml| #\u00C4)
    (|Aring| #\u00C5) (|AElig| #\u00C6) (|Ccedil| #\u00C7) (|Egrave| #\u00C8) (|Eacute| #\u00C9) (|Ecirc| #\u00CA)
    (|Euml| #\u00CB) (|Igrave| #\u00CC) (|Iacute| #\u00CD) (|Icirc| #\u00CE) (|Iuml| #\u00CF) (|ETH| #\u00D0)
    (|Ntilde| #\u00D1) (|Ograve| #\u00D2) (|Oacute| #\u00D3) (|Ocirc| #\u00D4) (|Otilde| #\u00D5) (|Ouml| #\u00D6)
    (|times| #\u00D7) (|Oslash| #\u00D8) (|Ugrave| #\u00D9) (|Uacute| #\u00DA) (|Ucirc| #\u00DB) (|Uuml| #\u00DC)
    (|Yacute| #\u00DD) (|THORN| #\u00DE) (|szlig| #\u00DF) (|agrave| #\u00E0) (|aacute| #\u00E1) (|acirc| #\u00E2)
    (|atilde| #\u00E3) (|auml| #\u00E4) (|aring| #\u00E5) (|aelig| #\u00E6) (|ccedil| #\u00E7) (|egrave| #\u00E8)
    (|eacute| #\u00E9) (|ecirc| #\u00EA) (|euml| #\u00EB) (|igrave| #\u00EC) (|iacute| #\u00ED) (|icirc| #\u00EE)
    (|iuml| #\u00EF) (|eth| #\u00F0) (|ntilde| #\u00F1) (|ograve| #\u00F2) (|oacute| #\u00F3) (|ocirc| #\u00F4)
    (|otilde| #\u00F5) (|ouml| #\u00F6) (|divide| #\u00F7) (|oslash| #\u00F8) (|ugrave| #\u00F9)
    (|uacute| #\u00FA) (|ucirc| #\u00FB) (|uuml| #\u00FC) (|yacute| #\u00FD) (|thorn| #\u00FE) (|yuml| #\u00FF)
    (|OElig| #\u0152) (|oelig| #\u0153) (|Scaron| #\u0160) (|scaron| #\u0161) (|Yuml| #\u0178) (|fnof| #\u0192)
    (|circ| #\u02C6) (|tilde| #\u02DC) (|Alpha| #\u0391) (|Beta| #\u0392) (|Gamma| #\u0393) (|Delta| #\u0394)
    (|Epsilon| #\u0395) (|Zeta| #\u0396) (|Eta| #\u0397) (|Theta| #\u0398) (|Iota| #\u0399) (|Kappa| #\u039A)
    (|Lambda| #\u039B) (|Mu| #\u039C) (|Nu| #\u039D) (|Xi| #\u039E) (|Omicron| #\u039F) (|Pi| #\u03A0)
    (|Rho| #\u03A1) (|Sigma| #\u03A3) (|Tau| #\u03A4) (|Upsilon| #\u03A5) (|Phi| #\u03A6) (|Chi| #\u03A7)
    (|Psi| #\u03A8) (|Omega| #\u03A9) (|alpha| #\u03B1) (|beta| #\u03B2) (|gamma| #\u03B3) (|delta| #\u03B4)
    (|epsilon| #\u03B5) (|zeta| #\u03B6) (|eta| #\u03B7) (|theta| #\u03B8) (|iota| #\u03B9) (|kappa| #\u03BA)
    (|lambda| #\u03BB) (|mu| #\u03BC) (|nu| #\u03BD) (|xi| #\u03BE) (|omicron| #\u03BF) (|pi| #\u03C0)
    (|rho| #\u03C1) (|sigmaf| #\u03C2) (|sigma| #\u03C3) (|tau| #\u03C4) (|upsilon| #\u03C5) (|phi| #\u03C6)
    (|chi| #\u03C7) (|psi| #\u03C8) (|omega| #\u03C9) (|thetasym| #\u03D1) (|upsih| #\u03D2) (|piv| #\u03D6)
    (|ensp| #\u2002) (|emsp| #\u2003) (|thinsp| #\u2009) (|zwnj| #\u200C) (|zwj| #\u200D) (|lrm| #\u200E)
    (|rlm| #\u200F) (|ndash| #\u2013) (|mdash| #\u2014) (|lsquo| #\u2018) (|rsquo| #\u2019) (|sbquo| #\u201A)
    (|ldquo| #\u201C) (|rdquo| #\u201D) (|bdquo| #\u201E) (|dagger| #\u2020) (|Dagger| #\u2021) (|bull| #\u2022)
    (|hellip| #\u2026) (|permil| #\u2030) (|prime| #\u2032) (|Prime| #\u2033) (|lsaquo| #\u2039)
    (|rsaquo| #\u203A) (|oline| #\u203E) (|frasl| #\u2044) (|euro| #\u20AC) (|image| #\u2111) (|weierp| #\u2118)
    (|real| #\u211C) (|trade| #\u2122) (|alefsym| #\u2135) (|larr| #\u2190) (|uarr| #\u2191) (|rarr| #\u2192)
    (|darr| #\u2193) (|harr| #\u2194) (|crarr| #\u21B5) (|lArr| #\u21D0) (|uArr| #\u21D1) (|rArr| #\u21D2)
    (|dArr| #\u21D3) (|hArr| #\u21D4) (|forall| #\u2200) (|part| #\u2202) (|exist| #\u2203) (|empty| #\u2205)
    (|nabla| #\u2207) (|isin| #\u2208) (|notin| #\u2209) (|ni| #\u220B) (|prod| #\u220F) (|sum| #\u2211)
    (|minus| #\u2212) (|lowast| #\u2217) (|radic| #\u221A) (|prop| #\u221D) (|infin| #\u221E) (|ang| #\u2220)
    (|and| #\u2227) (|or| #\u2228) (|cap| #\u2229) (|cup| #\u222A) (|int| #\u222B) (|there4| #\u2234)
    (|sim| #\u223C) (|cong| #\u2245) (|asymp| #\u2248) (|ne| #\u2260) (|equiv| #\u2261) (|le| #\u2264)
    (|ge| #\u2265) (|sub| #\u2282) (|sup| #\u2283) (|nsub| #\u2284) (|sube| #\u2286) (|supe| #\u2287)
    (|oplus| #\u2295) (|otimes| #\u2297) (|perp| #\u22A5) (|sdot| #\u22C5) (|lceil| #\u2308) (|rceil| #\u2309)
    (|lfloor| #\u230A) (|rfloor| #\u230B) (|lang| #\u2329) (|rang| #\u232A) (|loz| #\u25CA) (|spades| #\u2660)
    (|clubs| #\u2663) (|hearts| #\u2665) (|diams| #\u2666)))

(defvar *html-entities-hashtable*
  (let ((ht (make-hash-table)))
    (dolist (pair *named-html-entities*)
      (destructuring-bind (k v) pair
	(setf (gethash k ht) v)))
    ht))

(defun decode-html-entity (entity)
  (cond ((null entity) "&;")
	((char= (aref entity 0) #\#)
	 (if (char= (aref entity 1) #\x)
	     (string (code-char (parse-integer (subseq entity 2) :radix 16)))
	     (string (code-char (parse-integer (subseq entity 1) :radix 10)))))
	((gethash (intern entity) *html-entities-hashtable*)
	 (string (gethash (intern entity) *html-entities-hashtable*)))
	(t (format nil "&~A;" entity))))

; we need an accumulator for proper tail recursion to evade heap exhaustion
(defun decode-html-entities-1 (acc line)
  (let ((start-pos (search "&" line))
	(end-pos (search ";" line)))
    (cond ((or (null start-pos) (null end-pos))
	   (concatenate 'string acc line))
	  ((> start-pos end-pos)
	   (decode-html-entities-1 (concatenate 'string acc (subseq line 0 start-pos))
				   (subseq line start-pos)))
	  (t (let* ((pre-str (subseq line 0 start-pos))
		    (entity (subseq line (1+ start-pos) end-pos))
		    (post-str (subseq line (1+ end-pos))))
	       (decode-html-entities-1 (concatenate 'string acc pre-str (decode-html-entity entity))
				       post-str))))))

; decode html entities of the form &entity; and return decoded string
; supports named entities as well as base10 and hex encoded codes
(defun decode-html-entities (line)
  (decode-html-entities-1 "" line))


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
  (if (null (message-parent msg))
      msg
      (if (or (string= boundary (message-boundary msg))
	      (msg-embedded-rfc822? msg))
	  (parent-msg-from-boundary (message-parent msg) boundary)
	  msg)))

(defun read-mail-from-stream (str &key (init 'init))
  (do ((line (or *last-from* (read-line-from-stream str)) (read-line-from-stream str))
       (status init)
       (msg (make-message))
       (boundary nil)
       (skip-body nil)
       (decode-bodyline nil))
      ((eql line 'eof) (if (msg-empty? msg) nil msg))

    (setf *last-from* nil)

    (cond
      ;; start of new mail
      ((and (eq status 'init) (from? line))
       (pr-warn t "~A~%" line)
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
       (setf skip-body (not (or (string= "text/plain" (caar (message-content-type msg)))
				(string= "text/html" (caar (message-content-type msg))))))
       (setf decode-bodyline
	     (if (string= "text/html" (caar (message-content-type msg)))
		 #'(lambda (line msg) (decode-html-entities (content-transfer-decode line msg)))
		 #'(lambda (line msg) (content-transfer-decode line msg))))

       (pr-info t "(get-mime-boundary msg): ~A~%" boundary)
       (pr-info t "content-type: ~A~%" (message-content-type msg))
       (pr-info t "content-transfer-encoding: ~A~%" (message-content-transfer-encoding msg))
       (pr-info t "skip body: ~A~%" skip-body)

       (if (msg-embedded-rfc822? msg)
	   ;; embedded message -- add new child msg
	   (let ((parent msg))
	     (setf (message-body parent) (list (make-message))
		   msg (car (message-body msg))
		   (message-parent msg) parent
		   status 'header))))

      ;; header continuation line
      ((and (eq status 'header) (in (char line 0) #\Space #\Tab))
       (pr-dbg t "~A: ~A~%" status line)
       (let ((v+ (trim-blank line))
	     (tail (cdr (message-header msg)))
	     (k (car (car (message-header msg))))
	     (v (cdr (car (message-header msg)))))
	 (setf (message-header msg) (cons (cons k (concatenate 'string v " " v+)) tail))))

      ;; header line
      ((eq status 'header)
       (pr-dbg t "~A: ~A~%" status line)
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
	  (pr-info t "*** end of MIME parts with boundary: ~A~%" boundary)
	  (setf msg (message-parent msg))
	  (setf boundary (message-boundary msg))
	  (setf msg (parent-msg-from-boundary msg boundary)
		boundary (message-boundary msg))
	  (pr-info t "*** new MIME boundary in effect: ~A~%" boundary)

	  (if (null (message-parent msg))
	      (return msg)))

	 ;; start of new MIME part
	 ((and boundary (eql 2 (search boundary line)))
	  (pr-info t "start of new MIME part, boundary: ~A~%" boundary)
	  ;; if current msg's boundary is this boundary, add child to msg
	  ;; if current msg's parent's boundary is this boundary, add sibling to msg
	  (cond ((string= (message-boundary msg) boundary)
		 (pr-info t "Adding child to msg~%")
		 (let* ((parent msg)
			(new-msg (make-message))
			(new-body (if (listp (message-body parent))
				      (append (message-body parent) (list new-msg))
				      (list new-msg))))
		   (setf (message-body parent) new-body
			 msg new-msg
			 (message-parent msg) parent)))
		((string= (message-boundary (message-parent msg)) boundary)
		 (pr-info t "Adding sibling to msg~%")
		 (let ((parent (message-parent msg))
		       (new-msg (make-message)))
		   (setf (message-body parent) (append (message-body parent) (list new-msg))
			 (message-parent new-msg) parent
			 msg new-msg))))
	  (setf status 'header))

	 ;; regular body line
	 ((and (not skip-body) (not (message-p (message-body msg))))
	  (pr-info t "~A: ~A~%" status line)
	  (if (or (null (message-body msg)) (stringp (message-body msg)))
	      (setf (message-body msg) (concatenate 'string (message-body msg)
						    (funcall decode-bodyline line msg)))
	      (pr-info t "throwing away: ~A~%" line))))))))

(defun header-to-string (msg)
  (let ((from (message-from msg))
	(hdr-weed (mapcar (lambda (entry)
			    (if (in (car entry) 'from 'to 'subject 'return-path)
				(format nil "~A: ~A~%" (car entry) (mime-decode-line (cdr entry)))))
			  (message-header msg))))
    (if (null from)
	(format nil "~%~A~%" (apply #'concatenate 'string hdr-weed))
	(format nil "~A~%~A~%" from (apply #'concatenate 'string hdr-weed)))))

(defun message-to-strings (msg)
  (let ((hdr (header-to-string msg))
	(body (message-body msg)))
    (cond ((null body) (list hdr))
	  ((stringp body) (list hdr body))
	  (t (append (list hdr)
		     (mapcan #'message-to-strings body))))))

(defun read-mailbox (path mailbox)
  (with-open-file (str (format nil "~A/~A" path mailbox)
		       :direction :input
		       :element-type '(unsigned-byte 8))
    (do ((m (read-mail-from-stream str) (read-mail-from-stream str)))
	((null m))
      (dolist (s (message-to-strings m))
	(format t "~A" s)))))

;; We must be able to print circular structure without getting stoned
(setf *print-circle* t)

;(read-mailbox "/xenon/tom/src/lisp/nospam/mail" "parse-pdf-att")
;(read-mailbox "/xenon/tom/src/lisp/nospam/mail" "qpdec-choke")
;(read-mailbox "/xenon/tom/src/lisp/nospam/mail" "endless-loop")
;(read-mailbox "/xenon/tom/src/lisp/nospam/mail" "overlong-line")
;(read-mailbox "/xenon/tom/src/lisp/nospam/mail" "fuzzy-charset")
;(read-mailbox "/xenon/tom/src/lisp/nospam/mail" "illegal-char")
;(read-mailbox "/xenon/tom/src/lisp/nospam/mail" "choke4")
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
