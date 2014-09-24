(load "macros.lisp")

(defstruct corpus
  type         ; :ham or :spam
  mboxes       ; list structure of mailboxes
  (n-mails 0)  ; number of mails in corpus
  (hash-table (make-hash-table :test #'eq :size 10000)))  ; hash table counting token occurrences

(defmacro defcorpus (name type mboxes)
  `(defparameter ,name (make-corpus :type ,type :mboxes (quote ,mboxes))))

;; run-time global variables
(defvar *token-table*) (defvar *toks*) (defvar *ham*) (defvar *spam*)

;; run-time global variables referring to actively processed corpus
(defvar *n-mails* 0)
(defvar *hash-table* (make-hash-table :test #'eq :size 1000))

;; other (operational) global vars & params
(defparameter bufsize 1024)
(defvar *emit-tokens* t)
(defvar *silent* nil)

;;;; Buffer utilities

(defstruct buf vec (start -1) (end -1))

(defun bref (buf n)
  (char (buf-vec buf)
	(mod n (length (buf-vec buf)))))

(defun (setf bref) (val buf n)
  (setf (char (buf-vec buf)
	      (mod n (length (buf-vec buf))))
	val))

(defun new-buf (len)
  (make-buf :vec (make-string len)))

(defun buf-insert (x b)
  (setf (bref b (incf (buf-end b))) x)
  (if (> (- (buf-end b) (buf-start b)) (length (buf-vec b)))
      (setf (buf-start b) (- (buf-end b) (length (buf-vec b)))))
  x)

(defun buf-clear (b)
  (setf (buf-start b) -1
	(buf-end b) -1))

(defun buf-string (b)
  (let ((lst nil))
    (for x (1+ (buf-start b)) (buf-end b)
      (push (bref b x) lst))
    (coerce (nreverse lst) 'string)))


;;;; Tokenizer

(defun count-token (tok)
  (let ((toksym (intern tok)))
    (setf (gethash toksym *hash-table*)
	  (1+ (or (gethash toksym *hash-table*) 0)))))

;; Split potential tokens:
;; - split by any , and . surrounded by any non-digit char
;; - split by any - only surrounded by digits
;; In case of a split, the above separators are removed.
;; A list of final tokens to be emitted is returned.
(defun split-toks (str)
  (let* ((len (length str))
	 (str-prev (concatenate 'string "^" (subseq str 0 (1- len))))
	 (str-next (concatenate 'string (subseq str 1) "$"))
	 (start 0)
	 (toks nil))
    ;;(format t "~A ~A ~A~%" str-prev str str-next)
    (do ((i 0 (1+ i)))
	((>= i len)
	 (if (> i start)
	     (push (subseq str start i) toks)))
      (let ((p (char str-prev i))
	    (c (char str i))
	    (n (char str-next i)))
	;;(format t "~A ~A ~A~%" p c n)
	(if (or (and (char= c #\-)
		     (or (char= p #\^) (digit-char-p p))
		     (or (char= n #\$) (digit-char-p n)))
		(and (in c #\, #\.)
		     (or (or (char= p #\^) (not (digit-char-p p)))
			 (or (char= n #\$) (not (digit-char-p n))))))
	    (progn
	      (if (> i start)
		  (push (subseq str start i) toks))
	      (setf start (1+ i))
	      ;;(format t "start: ~A  toks: ~A~%" start toks)
	      ))))
    toks))

;; Emit tokens with some postprocessing:
;; split and ignore resulting outbound tokens longer than 32 characters.
(defun emit-tok (s &optional label)
  (if *emit-tokens*
      (let ((toks (split-toks s)))
	(dolist (tok toks)
	  (if (null label)
	      (if (< (length tok) 32)
		  (count-token tok))
	      (count-token (format nil "~A*~A" label tok)))))))

(defun emit-1 (tokstate tokbuf c)
  (let ((s (buf-string tokbuf)))
    (if (> (length s) 0)
	(progn
	  (cond ((eql tokstate 'tokstate-from)
		 (setf *emit-tokens* t)
		 (emit-tok s "From"))
		((eql tokstate 'tokstate-to) (emit-tok s "To"))
		((eql tokstate 'tokstate-subject) (emit-tok s "Subject"))
		((eql tokstate 'tokstate-rpath) (emit-tok s "ReturnPath"))
		((eql tokstate 'tokstate-url) (emit-tok s "Url"))
		(t (emit-tok s)))
	  (buf-clear tokbuf)
	  (cond ((eql tokstate 'tokstate-colzero)
		 (cond ((and (string= s "From") (not (char= c #\:)))
			(incf *n-mails*)
			(unless *silent*
			  (format t "~c[6D~6A" #\Esc *n-mails*) (finish-output nil))
			'tokstate-init)
		       ((and (string= s "From") (char= c #\:)) 'tokstate-from)
		       ((and (string= s "To") (char= c #\:)) 'tokstate-to)
		       ((and (string= s "Subject") (char= c #\:)) 'tokstate-subject)
		       ((and (string= s "Return-Path") (char= c #\:)) 'tokstate-rpath)
		       ((and (string-equal s "http") (char= c #\:)) 'tokstate-url)
		       ((and (string-equal s "Content-Transfer-Encoding") (char= c #\:))
			'tokstate-encoding)
		       (t 'tokstate-init)))
		((and (string-equal s "http") (char= c #\:)) 'tokstate-url)
		((and (eql tokstate 'tokstate-url)
		      (or (not (graphic-char-p c)) (in c #\" #\>)))
		 'tokstate-init)
		((and (eql tokstate 'tokstate-encoding) (string-equal s "base64"))
		 (setf *emit-tokens* nil) ; inhibit tokens until next From (next mail header)
		 'tokstate-init)
		(t tokstate)))
	tokstate)))

;; emit contents of tokbuf
;; NB. return value will be new tokstate
(defun emit (tokstate tokbuf c)
  (let ((new-tokstate (emit-1 tokstate tokbuf c)))
    (if (in c #\Newline #\Linefeed)
	'tokstate-colzero
	new-tokstate)))

;; state machine for tokenization
;; HTML comments are discarded, they are not even token separators.
;; NB. return value will be new tokstate
(defun tokenize (tokstate tokbuf c)
  (cond
    ;; HTML comment end
    ((and (characterp c) (char= c #\>) (eql tokstate 'tokstate-html-out2))
     ;;(format t "Leave HTML comment~%")
     'tokstate-init)
    ((and (characterp c) (char= c #\-) (eql tokstate 'tokstate-html-out1))
     'tokstate-html-out2)
    ((and (characterp c) (char= c #\-) (eql tokstate 'tokstate-html-comment))
     'tokstate-html-out1)
    ((in tokstate 'tokstate-html-comment 'tokstate-html-out1 'tokstate-html-out2)
     'tokstate-html-comment)

    ;; HTML comment start
    ((and (characterp c) (char= c #\<))
     'tokstate-html-in1)
    ((and (characterp c) (char= c #\!) (eql tokstate 'tokstate-html-in1))
     'tokstate-html-in2)
    ((and (characterp c) (char= c #\-) (eql tokstate 'tokstate-html-in2))
     'tokstate-html-in3)
    ((and (characterp c) (char= c #\-) (eql tokstate 'tokstate-html-in3))
     ;;(format t "Enter HTML comment~%")
     'tokstate-html-comment)
    ((in tokstate 'tokstate-html-in1 'tokstate-html-in2 'tokstate-html-in3)
     (tokenize 'tokstate-init tokbuf c))

    ;; constituent - add it to the token buffer:
    ((or (and (characterp c) (alphanumericp c)) (in c #\- #\' #\! #\$ #\, #\.))
     (buf-insert c tokbuf)
     tokstate) ; stay in same state

    ;; separator - emit buffer as token(s):
    (t (emit tokstate tokbuf c))))

(defun getc (str)
  (handler-bind ((sb-int:stream-decoding-error
		  (lambda (c)
		    (declare (ignore c))
		    (invoke-restart (car (compute-restarts))))))
    (read-char str nil 'eof)))

(defun read-mail-from-stream (str)
  (do ((c (getc str) (getc str))
       (tokstate 'tokstate-colzero (tokenize tokstate tokbuf c))
       (tokbuf (new-buf bufsize)))
      ((eql c 'eof) (tokenize tokstate tokbuf c))))

(defun read-mailfile (path mailbox)
  (format t "~c[6D~A       " #\Esc mailbox) (finish-output nil)
  (with-open-file (str (format nil "~A/~A" path mailbox)
		       :direction :input :external-format :utf-8)
    (read-mail-from-stream str)))

(defun read-corpus (corpus)
  (setf *n-mails* (corpus-n-mails corpus)
	*hash-table* (corpus-hash-table corpus))
  (fresh-line) (format t "~%Reading ~A mailboxes, please wait...~%        " (corpus-type corpus))
  (dolist (mbox (corpus-mboxes corpus))
    (let ((home (namestring (user-homedir-pathname)))
	  (mdir (car mbox))
	  (boxes (cadr mbox)))
      (let ((path (if (char= (char mdir 0) #\/)
		      mdir
		      (format nil "~A~A" home mdir))))
	(dolist (box boxes)
	  (read-mailfile path box)))))
  (setf (corpus-n-mails corpus) *n-mails*)
  corpus)


(defun token-spam-probability (token ht-ham ht-spam n-ham n-spam)
  (let ((g (* 2 (or (gethash token ht-ham) 0)))
	(b (or (gethash token ht-spam) 0)))
    (unless (< (+ g b) 5)
      (max .01
	   (min .99
		(float (/ (min 1 (/ b n-spam))
			  (+ (min 1 (/ g n-ham))
			     (min 1 (/ b n-spam))))))))))

(defun make-token-table (spam-corpus ham-corpus)
  (let* ((ht-spam (corpus-hash-table spam-corpus))
	 (n-spam (corpus-n-mails spam-corpus))
	 (ht-ham (corpus-hash-table ham-corpus))
	 (n-ham (corpus-n-mails ham-corpus))
	 (token-probs (make-hash-table :test #'eq :size 100000))
	 (update-prob #'(lambda (k v)
			  (declare (ignore v))
			  (let ((p (token-spam-probability k ht-ham ht-spam n-ham n-spam)))
			    (setf (gethash k token-probs) p)))))
    (maphash update-prob ht-spam) ; map over both hash tables
    (maphash update-prob ht-ham)  ; so P of all tokens are computed
    token-probs))


;;;; Thy application

(defun nospam-rebuild ()
  (let* ((home (namestring (user-homedir-pathname)))
	 (conf (format nil "~A~A" home "nospam.conf")))
    (format t "~%~%NOSPAM naive Bayesian spam classifier~%")
    (load conf)
    (read-corpus *ham*)
    (read-corpus *spam*)
    (setf *token-table* (make-token-table *spam* *ham*))

    (terpri) (terpri)
    (format t "Unique tokens from HAM :~9d~%" (hash-table-count (corpus-hash-table *ham*)))
    (format t "Unique tokens from SPAM:~9d~%" (hash-table-count (corpus-hash-table *spam*)))
    (format t "Total tokens recognized:~9d~%" (hash-table-count *token-table*))

    (format t "~%Creating executable image and exiting...~%~%")
    (sb-ext:save-lisp-and-die "nospam" :compression t :executable t :toplevel #'nospam)))

(defun nospam-classify ()
  (setf *n-mails* 0
	*hash-table* (make-hash-table :test #'eq :size 1000)
	*emit-tokens* t
	*silent* t
	*toks* nil)
  (read-mail-from-stream *standard-input*)

  (maphash #'(lambda (k v)
	       (declare (ignore v))
	       (let ((p (or (gethash k *token-table*) 0.4)))
		 (setf (gethash k *hash-table*) p)
		 (push (list k (abs (- p 0.5))) *toks*)))
	   *hash-table*)
  (let* ((sorted-toks (sort *toks* #'> :key #'cadr))
	 (interesting-toks (if (> (length sorted-toks) 15) (subseq sorted-toks 0 15) sorted-toks))
	 (*toks* (mapcar #'car interesting-toks))
	 (probs (mapcar #'(lambda (tok) (gethash tok *hash-table*)) *toks*))
	 (prod (apply #'* probs))
	 (spam-prob (/ prod (+ prod (apply #'* (mapcar #'(lambda (x) (- 1 x)) probs))))))
    (format t "~%Most interesting tokens (of ~d total):~%" (hash-table-count *hash-table*))
    (dolist (tok *toks*)
      (format t "  ~32A  ~7f  ~7d  ~7d~%"
	      tok (gethash tok *hash-table*)
	      (or (gethash tok (corpus-hash-table *ham*)) 0)
	      (or (gethash tok (corpus-hash-table *spam*)) 0)))
    (format t "~%Spam probability: ~A~%~%" spam-prob)
    (if (> spam-prob 0.9) (sb-ext:exit :code 1) (sb-ext:exit :code 0))))

(defun nospam ()
  (cond ((and (> (length *posix-argv*) 1)
	      (string= (cadr *posix-argv*) "rebuild"))
	 (nospam-rebuild))
	(t
	 (nospam-classify))))


;;;; Bootstrap in case of initial load (not via saved executable core)
(nospam-rebuild)
