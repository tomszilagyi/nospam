(in-package nospam)

;;;; Macro facilities

(defmacro for (var start stop &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
	  (,gstop ,stop))
	 ((> ,var ,gstop))
       ,@body)))

(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
		     choices)))))

;;;; Utilities

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of part are
replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
       for old-pos = 0 then (+ pos part-length)
       for pos = (search part string
			 :start2 old-pos
			 :test test)
       do (write-string string out
			:start old-pos
			:end (or pos (length string)))
       when pos do (write-string replacement out)
       while pos)))

; debug printouts on different verbosity levels
(defvar *verbosity* 0)

(defmacro pr-dbg (&body body)
  `(if (> *verbosity* 2)
       (format ,@body)))

(defmacro pr-info (&body body)
  `(if (> *verbosity* 1)
       (format ,@body)))

(defmacro pr-warn (&body body)
  `(if (> *verbosity* 0)
       (format ,@body)))

(defmacro pr-err (&body body)
  `(if (> *verbosity* -1)
       (format ,@body)))
