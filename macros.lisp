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
