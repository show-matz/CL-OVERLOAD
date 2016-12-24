(provide :cl-overload)

(defpackage		:cl-overload
  (:nicknames	:ol
				:overload)
  (:use			:common-lisp)
  (:export		#|--------------------------|#
				:make-overload-name
				:make-overload-setf-name
				:declare-method-overload
				:defmethod-overload
				:declare-function-overload
				:defun-overload
				:declare-macro-overload
				:defmacro-overload
				:declare-constructor
				:define-constructor
				:define-constructor-macro
				:dynamic-new
				:new
				#|--------------------------|#))

(in-package :cl-overload)

;;------------------------------------------------------------------------------
;;
;; utilities from 'On Lisp'
;;
;;------------------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun onlisp/mkstr (&rest args)
	(with-output-to-string (s)
	  (dolist (a args) (princ a s)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun onlisp/symb/r (package &rest args)
	(if (null package)
		(values (intern (apply #'onlisp/mkstr args)))
		(values (intern (apply #'onlisp/mkstr args) package)))))


;;------------------------------------------------------------------------------
;;
;; internal utilities ( not exported ).
;;
;;------------------------------------------------------------------------------
(defun argsym-to-arginfo (v)
  ;; ex : (argsym-to-arginfo     3) -> (3 3 nil)
  ;;      (argsym-to-arginfo   '5+) -> (5 5   t)
  ;;      (argsym-to-arginfo  '2-4) -> (2 4 nil)
  ;;      (argsym-to-arginfo '2-4+) -> (2 4   t)
  (cond ((integerp v)      (list v v nil))
		((not (symbolp v)) (error "Invalid arg-info : '~A'." v))
		(t (let* ((restp nil)
				  (str   (symbol-name v))
				  (len   (length str)))
			 (when (char= #\+ (char str (1- len)))
			   (decf len)
			   (setf restp t))
			 (let ((n1 nil)
				   (n2 nil)
				   (pt (do ((idx 0 (incf idx)))
						   ((= idx len) nil)
						 (when (char= #\- (aref str idx))
						   (return idx)))))
			   (if (null pt)
				   (progn
					 (setf n1 (ignore-errors (parse-integer str :end len)))
					 (setf n2 n1))
				   (progn
					 (setf n1 (ignore-errors (parse-integer str :end pt)))
					 (setf n2 (ignore-errors (parse-integer str :start (1+ pt) :end len)))))
			   (unless (and n1 n2 (<= n1 n2))
				 (error "Invalid arg-info : '~A'." v))
			   (list n1 n2 restp))))))

(defun arginfo-to-argsym (arg-info &optional (package nil))
  ;; this function is not exported & not used in this package, but used in CL-CLASS...
  (destructuring-bind (min max restp) arg-info
	(cond
	  ((and (= min max) (not restp))                        min           )
	  ((and (= min max) restp)       (onlisp/symb/r package min        '+))
	  ((and (< min max) (not restp)) (onlisp/symb/r package min '- max   ))
	  ((and (< min max) restp)       (onlisp/symb/r package min '- max '+)))))


(defun paramlst-to-arginfo (lst)
  ;; ex : (paramlst-to-arginfo '(v1 v2 v3))          -> (3 3 nil)
  ;;      (paramlst-to-arginfo '(v1 v2 &rest rest)   -> (2 2   t)
  ;;      (paramlst-to-arginfo '(v1 &optional v2 v3) -> (1 3 nil)
  ;;      (paramlst-to-arginfo '(&rest rest)         -> (0 0   t)
  (let* ((min 0)
		 (max 0)
		 (fnc (lambda () (incf min) (incf max))))
	(do ()
		((null lst) (list min max nil))
	  (let ((sym (car lst)))
		(cond
		  ((eq sym      '&key) (error "Can't use &key parameter in overload."))
		  ((eq sym '&optional) (setf fnc (lambda () (incf max))))
		  ((eq sym     '&rest) (progn
								 (unless (= (length lst) 2)
								   (error "Invalid parameter list."))
								 (return-from paramlst-to-arginfo (list min max t))))
		  (t                   (funcall fnc)))
		(setf lst (cdr lst))))))


(defun arginfo-to-paramlst (arg-info)
  (destructuring-bind (min max restp) arg-info
	(labels ((imp (&optional (idx 0) (acc nil))
			   (if (<= max idx)
				   (progn
					 (when restp
					   (setf acc (append `(,(gensym "REST") &rest) acc)))
					 (nreverse acc))
				   (progn
					 (when (and (= idx min) (< min max))
					   (cl:push '&optional acc))
					 (imp (1+ idx) (cl:push (gensym (format nil "V~A-" (1+ idx))) acc))))))
	  (imp))))







(eval-when (:compile-toplevel :load-toplevel :execute)
  (labels ((make-match-condition (arg-info g-cnt)
			 (destructuring-bind (min max restp) arg-info
			   (cond
				 ((and (= min max) (not restp)) `(=  ,min ,g-cnt))
				 ((and (= min max) restp)       `(<= ,min ,g-cnt))
				 ((and (< min max) (not restp)) `(<= ,min ,g-cnt ,max))
				 ((and (< min max) restp)       `(<= ,min ,g-cnt)))))
			   
		   (is-setf (name)
			 (and (listp name)
				  (= (length name) 2)
				  (eq (car name) 'setf)))

		   (make-defgeneric (name arg-info name-maker &optional (param-wrapper #'identity))
			 `(defgeneric ,(funcall name-maker name arg-info)
						  ,(funcall param-wrapper (arginfo-to-paramlst arg-info)))))

	(labels ((make-method-fncname (type arg-info &optional (sep '-))
			   (let ((name    (symbol-name    type))
					 (package (symbol-package type)))
				 (destructuring-bind (min max restp) arg-info
				   (cond
					 ((and (= min max) (not restp)) (onlisp/symb/r package '__ name sep min))
					 ((and (= min max) restp)       (onlisp/symb/r package '__ name sep min '+))
					 ((and (< min max) (not restp)) (onlisp/symb/r package '__ name sep min '- max))
					 ((and (< min max) restp)       (onlisp/symb/r package '__ name sep min '- max '+))))))
			 (make-method-setf-fncname (type arg-info &optional (sep '-))
			   `(setf ,(make-method-fncname type arg-info sep))))

	  ;;------------------------------------------------------------------------------
	  ;; macro make-overload-name
	  #|
	  #|EXPORT|#				:make-overload-name
	  |#
	  ;;------------------------------------------------------------------------------
	  (defun make-overload-name (method arg-cnt &key (separator '-))
		"
Syntax:
  (make-overload-name method arg-cnt &key (separator '-))

Arguments and Values:
  method    : a symbol. (method name)
  arg-cnt   : a number or symbol. (argument information)
  separator : a symbol.

Return value:
  symbol.

Description:
    Make internal overloaded method ( or function ) name from 'method' 
  and 'arg-cnt' parameter.

Examples:
  (make-overload-name 'foo 1)     => __FOO-1
  (make-overload-name 'foo '2+)   => __FOO-2+
  (make-overload-name 'foo '3-4)  => __FOO-3-4
  (make-overload-name 'foo '5-7+) => __FOO-5-7+

See Also:
  make-overload-setf-name, declare-method-overload, declare-function-overload
"
		(make-method-fncname method (argsym-to-arginfo arg-cnt) separator))

	  ;;------------------------------------------------------------------------------
	  ;; macro make-overload-setf-name
	  #|
	  #|EXPORT|#				:make-overload-setf-name
	  |#
	  ;;------------------------------------------------------------------------------
	  (defun make-overload-setf-name (method arg-cnt &key (separator '-))
		"
Syntax:
  (make-overload-setf-name method arg-cnt &key (separator '-))

Arguments and Values:
  method    : a symbol. (method name)
  arg-cnt   : a number or symbol. (argument information)
  separator : symbol.

Return value:
  list of symbols.

Description:
    Make internal overloaded setf method ( or function ) name from 'method' 
  and 'arg-cnt' parameter.

Examples:
  (make-overload-setf-name 'foo 1)     => (SETF __FOO-1)
  (make-overload-setf-name 'foo '2+)   => (SETF __FOO-2+)
  (make-overload-setf-name 'foo '3-4)  => (SETF __FOO-3-4)
  (make-overload-setf-name 'foo '5-7+) => (SETF __FOO-5-7+)

See Also:
  make-overload-name, declare-method-overload, declare-function-overload
"
		`(setf ,(make-method-fncname method (argsym-to-arginfo arg-cnt) separator)))


;;;;------------------------------------------------------------------------------------
;;;; method overloading
;;;;------------------------------------------------------------------------------------

	  ;;------------------------------------------------------------------------------
	  ;; macro declare-method-overload
	  #|
	  #|EXPORT|#				:declare-method-overload
	  |#
	  ;;------------------------------------------------------------------------------
	  (defmacro declare-method-overload (name (&rest arginfo-lst)
										 &key (make-setf nil) (make-top t) (documentation nil))
		"    
Syntax:
  (declare-method-overload name (&rest arginfo-lst)
                           &key (make-setf nil) (make-top t) (documentation nil))

Arguments and Values:
  name          : a symbol. (method name)
  arginfo-lst   : sequence of number or symbol.
  make-setf     : a boolean value.
  make-top      : a boolean value.
  documentation : documentation string.

Description:
  Declare overloading method ( defgenerics, entry function and compiler-macro ).
If make-setf is not null, declaration for setf generated too.

See Also:
  defmethod-overload, declare-function-overload, declare-macro-overload,
  make-overload-name, make-overload-setf-name
"
		(let ((g-args   (gensym "ARGS"))
			  (g-cnt    (gensym "CNT"))
			  (g-newval (gensym "NEWVAL")))
		  (setf arginfo-lst (mapcar #'argsym-to-arginfo arginfo-lst))
		  `(progn
			 ,@(mapcar (lambda (arg-info)
						 (make-defgeneric name arg-info
										  (lambda (type arg-info &key (sep '-))
											(make-method-fncname type arg-info sep)))) arginfo-lst)
			 ,@(when make-setf
					 (mapcar (lambda (arg-info)
							   (make-defgeneric name arg-info
												(lambda (type arg-info &key (sep '-))
												  `(setf ,(make-method-fncname type arg-info sep)))
												(lambda (prms) (cons g-newval prms)))) arginfo-lst))
			 ,(when make-top
					`(defun ,name (&rest ,g-args)
					   ,documentation
					   (let ((,g-cnt (length ,g-args)))
						 (cond
						   ,@(mapcar (lambda (arg-info)
									   `(,(make-match-condition arg-info g-cnt)
										  (apply #',(make-method-fncname name arg-info) ,g-args))) arginfo-lst)
						   (t (error "Can't resolve overload for ~A" ',name))))))
			 ,(when make-top
					`(define-compiler-macro ,name (&rest ,g-args)
					   (let ((,g-cnt (length ,g-args)))
						 (cond
						   ,@(mapcar (lambda (arg-info)
									   `(,(make-match-condition arg-info g-cnt)
										  `(,',(make-method-fncname name arg-info) ,@,g-args))) arginfo-lst)
						   (t (error "Can't resolve overload for ~A" ',name))))))
			 ,(when (and make-top make-setf)
					`(defun (setf ,name) (,g-newval &rest ,g-args)
					   ,documentation
					   (let ((,g-cnt (length ,g-args)))
						 (cond
						   ,@(mapcar (lambda (arg-info)
									   `(,(make-match-condition arg-info g-cnt)
										  (apply #',(make-method-setf-fncname name arg-info) ,g-newval ,g-args))) arginfo-lst)
						   (t (error "Can't resolve overload for ~A" ',name))))))
			 ,(when (and make-top make-setf)
					`(define-compiler-macro (setf ,name) (,g-newval &rest ,g-args)
					   (let ((,g-cnt (length ,g-args)))
						 (cond
						   ,@(mapcar (lambda (arg-info)
									   `(,(make-match-condition arg-info g-cnt)
										  `(setf (,',(make-method-fncname name arg-info) ,@,g-args) ,,g-newval))) arginfo-lst)
						   (t (error "Can't resolve overload for ~A" ',name)))))))))

	  ;;------------------------------------------------------------------------------
	  ;; macro defmethod-overload
	  #|
	  #|EXPORT|#				:defmethod-overload
	  |#
	  ;;------------------------------------------------------------------------------
	  (defmacro defmethod-overload (name (&rest params) &body body)
		"    
Syntax:
  (defmethod-overload name (&rest params) &body body)

Arguments and Values:
  name          : a symbol. (method name)
  params        : sequence of parameter symbols.
  body          : method body.

Description:
    Define overloading method.
  ToDo : not yet documented.

See Also:
  declare-method-overload
"
		(let ((arg-info (paramlst-to-arginfo params))
			  (wrapper1 (if (is-setf name) #'cadr #'identity))
			  (wrapper2 (if (is-setf name) (lambda (sym) `(setf ,sym)) #'identity)))
		  (when (is-setf name)
			(decf (first  arg-info))
			(decf (second arg-info)))
		  (let ((name (make-method-fncname (funcall wrapper1 name) arg-info)))
			`(defmethod ,(funcall wrapper2 name) ,params
			   ,@body))))

;;;;------------------------------------------------------------------------------------
;;;; function overloading
;;;;------------------------------------------------------------------------------------

	  ;;------------------------------------------------------------------------------
	  ;; macro declare-function-overload
	  #|
	  #|EXPORT|#				:declare-function-overload
	  |#
	  ;;------------------------------------------------------------------------------
	  (defmacro declare-function-overload (name (&rest arginfo-lst)
										   &key (make-setf nil) (documentation nil))
		"    
Syntax:
  (declare-function-overload name (&rest arginfo-lst)
                             &key (make-setf nil) (documentation nil))

Arguments and Values:
  name          : a symbol. (function name)
  arginfo-lst   : sequence of number or symbol.
  make-setf     : a boolean value.
  documentation : documentation string.

Description:
    Declare overloading function ( entry function and compiler-macro ).
  If make-setf is not null, declaration for setf generated too.

See Also:
  defun-overload, declare-method-overload, declare-macro-overload,
  make-overload-name, make-overload-setf-name
"
		(let ((g-args   (gensym "ARGS"))
			  (g-cnt    (gensym "CNT"))
			  (g-newval (gensym "NEWVAL")))
		  (setf arginfo-lst (mapcar #'argsym-to-arginfo arginfo-lst))
		  `(progn
			 (defun ,name (&rest ,g-args)
			   ,documentation
			   (let ((,g-cnt (length ,g-args)))
				 (cond
				   ,@(mapcar (lambda (arg-info)
							   `(,(make-match-condition arg-info g-cnt)
								  (apply #',(make-method-fncname name arg-info) ,g-args))) arginfo-lst)
				   (t (error "Can't resolve overload for ~A" ',name)))))
			 (define-compiler-macro ,name (&rest ,g-args)
			   (let ((,g-cnt (length ,g-args)))
				 (cond
				   ,@(mapcar (lambda (arg-info)
							   `(,(make-match-condition arg-info g-cnt)
								  `(,',(make-method-fncname name arg-info) ,@,g-args))) arginfo-lst)
				   (t (error "Can't resolve overload for ~A" ',name)))))
			 ,(when make-setf
					`(defun (setf ,name) (,g-newval &rest ,g-args)
					   ,documentation
					   (let ((,g-cnt (length ,g-args)))
						 (cond
						   ,@(mapcar (lambda (arg-info)
									   `(,(make-match-condition arg-info g-cnt)
										  (apply #',(make-method-setf-fncname name arg-info) ,g-newval ,g-args))) arginfo-lst)
						   (t (error "Can't resolve overload for ~A" ',name))))))
			 ,(when make-setf
					`(define-compiler-macro (setf ,name) (,g-newval &rest ,g-args)
					   (let ((,g-cnt (length ,g-args)))
						 (cond
						   ,@(mapcar (lambda (arg-info)
									   `(,(make-match-condition arg-info g-cnt)
										  `(setf (,',(make-method-fncname name arg-info) ,@,g-args) ,,g-newval))) arginfo-lst)
						   (t (error "Can't resolve overload for ~A" ',name)))))))))

	  ;;------------------------------------------------------------------------------
	  ;; macro defun-overload
	  #|
	  #|EXPORT|#				:defun-overload
	  |#
	  ;;------------------------------------------------------------------------------
	  (defmacro defun-overload (name (&rest params) &body body)
		"    
Syntax:
  (defun-overload name (&rest params) &body body)

Arguments and Values:
  name   : a symbol. (function name)
  params : sequence of parameter symbols.
  body   : function body.

Description:
    Define overloading function.
  ToDo : not yet documented.

See Also:
  declare-function-overload
"
		(let ((arg-info (paramlst-to-arginfo params))
			  (wrapper1 (if (is-setf name) #'cadr #'identity))
			  (wrapper2 (if (is-setf name) (lambda (sym) `(setf ,sym)) #'identity)))
		  (when (is-setf name)
			(decf (first  arg-info))
			(decf (second arg-info)))
		  (let ((name (make-method-fncname (funcall wrapper1 name) arg-info)))
			`(defun ,(funcall wrapper2 name) ,params
			   ,@body)))))


;;;;------------------------------------------------------------------------------------
;;;; macro overloading
;;;;------------------------------------------------------------------------------------
	(labels ((make-macro-impname (type arg-info)
			   (let ((name    (symbol-name    type))
					 (package (symbol-package type)))
				 (destructuring-bind (min max restp) arg-info
				   (cond
					 ((and (= min max) (not restp)) (onlisp/symb/r package '__ name '/ min))
					 ((and (= min max) restp)       (onlisp/symb/r package '__ name '/ min '+))
					 ((and (< min max) (not restp)) (onlisp/symb/r package '__ name '/ min '- max))
					 ((and (< min max) restp)       (onlisp/symb/r package '__ name '/ min '- max '+)))))))

	  ;;------------------------------------------------------------------------------
	  ;; macro declare-macro-overload
	  #|
	  #|EXPORT|#				:declare-macro-overload
	  |#
	  ;;------------------------------------------------------------------------------
	  (defmacro declare-macro-overload (name (&rest arginfo-lst) &key (documentation nil))
		"    
Syntax:
  (declare-macro-overload name (&rest arginfo-lst) &key (documentation nil))

Arguments and Values:
  name          : a symbol. (macro name)
  arginfo-lst   : sequence of number or symbol.
  documentation : documentation string.

Description:
    Declare overloading macro ( entry macro ).

See Also:
  defmacro-overload, declare-method-overload, declare-function-overload
"
		(let ((g-args (gensym "ARGS"))
			  (g-cnt  (gensym "CNT")))
		  (setf arginfo-lst (mapcar #'argsym-to-arginfo arginfo-lst))
		  `(defmacro ,name (&rest ,g-args)
			 ,documentation
			 (let ((,g-cnt (length ,g-args)))
			   (cond
				 ,@(mapcar (lambda (arg-info)
							 `(,(make-match-condition arg-info g-cnt)
								`(,',(make-macro-impname name arg-info) ,@,g-args))) arginfo-lst)
				 (t (error "Can't resolve overload for ~A" ',name)))))))

	  ;;------------------------------------------------------------------------------
	  ;; macro defmacro-overload
	  #|
	  #|EXPORT|#				:defmacro-overload
	  |#
	  ;;------------------------------------------------------------------------------
	  (defmacro defmacro-overload (name (&rest params) &body body)
		"    
Syntax:
  (defmacro-overload name (&rest params) &body body)

Arguments and Values:
  name          : a symbol. (macro name)
  params        : sequence of parameter symbols.
  body          : macro body.

Description:
    Define overloading macro.
  ToDo : not yet documented.

See Also:
  declare-macro-overload
"
		(let ((arg-info (paramlst-to-arginfo params)))
		  `(defmacro ,(make-macro-impname name arg-info) ,params
			 ,@body))))


;;;;------------------------------------------------------------------------------------
;;;; constructor overloading
;;;;------------------------------------------------------------------------------------
	(defun make-constructor-name (type)
	  (let ((name    (symbol-name    type))
			(package (symbol-package type)))
		(onlisp/symb/r package '__NEW- name)))

	(defun dynamic-new-imp (type)
	  (check-type type (and symbol (not keyword)))
	  (let ((ctor (ignore-errors
					(symbol-function
					 (onlisp/symb/r (symbol-package type)
						   "__NEW-" (symbol-name    type))))))
		(if (null ctor)
			(error "Constructor of ~A is not found." type)
			ctor)))

	(labels ((make-ctor-fncname (type arg-info)
			   (let ((name    (symbol-name    type))
					 (package (symbol-package type)))
				 (destructuring-bind (min max restp) arg-info
				   (cond
					 ((and (= min max) (not restp)) (onlisp/symb/r package '__NEW- name '- min))
					 ((and (= min max) restp)       (onlisp/symb/r package '__NEW- name '- min '+))
					 ((and (< min max) (not restp)) (onlisp/symb/r package '__NEW- name '- min '- max))
					 ((and (< min max) restp)       (onlisp/symb/r package '__NEW- name '- min '- max '+)))))))

	  ;;------------------------------------------------------------------------------
	  ;; macro declare-constructor
	  #|
	  #|EXPORT|#				:declare-constructor
	  |#
	  ;;------------------------------------------------------------------------------
	  (defmacro declare-constructor (name (&rest arginfo-lst))
		"    
Syntax:
  (declare-constructor name (&rest arginfo-lst))

Arguments and Values:
  name        : a symbol. (constructor name)
  arginfo-lst : sequence of number or symbol.

Description:
    Declare overloading constructor ( defgenerics, entry function and compiler-macro ).
  ToDo : not yet described.

See Also:
  define-constructor, new, dynamic-new
"
		(let ((g-args (gensym "ARGS"))
			  (g-cnt  (gensym "CNT")))
		  (setf arginfo-lst (mapcar #'argsym-to-arginfo arginfo-lst))
		  `(progn
			 ,@(mapcar (lambda (arg-info)
						 (make-defgeneric name arg-info
										  (lambda (type arg-info &key (sep '-))
											(declare (ignore sep))
											(make-ctor-fncname type arg-info)))) arginfo-lst)
			 (defun ,(make-constructor-name name) (&rest ,g-args)
			   (let ((,g-cnt (length ,g-args)))
				 (cond
				   ,@(mapcar (lambda (arg-info)
							   `(,(make-match-condition arg-info g-cnt)
								  (apply #',(make-ctor-fncname name arg-info) ,g-args))) arginfo-lst)
				   (t (error "Can't resolve overload for ~A" ',name)))))
			 (define-compiler-macro ,(make-constructor-name name) (&rest ,g-args)
			   (let ((,g-cnt (length ,g-args)))
				 (cond
				   ,@(mapcar (lambda (arg-info)
							   `(,(make-match-condition arg-info g-cnt)
								  `(,',(make-ctor-fncname name arg-info) ,@,g-args))) arginfo-lst)
				   (t (error "Can't resolve overload for ~A" ',name))))))))

	  ;;------------------------------------------------------------------------------
	  ;; macro define-constructor
	  #|
	  #|EXPORT|#				:define-constructor
	  |#
	  ;;------------------------------------------------------------------------------
	  (defmacro define-constructor (name (&rest params) &body body)
		"    
Syntax:
  (define-constructor name (&rest params) &body body)

Arguments and Values:
  name   : a symbol. (constructor name)
  params : sequence of parameter symbols.
  body   : constructor body.

Description:
    Define overloading constructor.
  ToDo : not yet described.

See Also:
  declare-constructor, new, dynamic-new
"
		(let ((arg-info (paramlst-to-arginfo params)))
		  `(defmethod ,(make-ctor-fncname name arg-info) ,params
			 ,@body)))

	  ;;------------------------------------------------------------------------------
	  ;; macro define-constructor-macro
	  #|
	  #|EXPORT|#				:define-constructor-macro
	  |#
	  ;;------------------------------------------------------------------------------
	  (defmacro define-constructor-macro (name (&rest params) &body body)
		"    
Syntax:
  (define-constructor-macro name (&rest params) &body body)

Arguments and Values:
  name   : a symbol. (constructor name)
  params : sequence of parameter symbols.
  body   : constructor macro body.

Description:
    Define compiler-macro for overloading constructor.
  ToDo : not yet described.

See Also:
  define-constructor
"
		(let ((arg-info (paramlst-to-arginfo params)))
		  `(define-compiler-macro ,(make-ctor-fncname name arg-info) ,params
			 ,@body)))

	  ;;------------------------------------------------------------------------------
	  ;; macro dynamic-new
	  #|
	  #|EXPORT|#				:dynamic-new
	  |#
	  ;;------------------------------------------------------------------------------
	  (defmacro dynamic-new (type &rest args)
		"    
Syntax:
  (dynamic-new type &rest args)

Arguments and Values:
  type   : variable binded to symbol of constructor name.
  args   : argument for constructor.

Description:
  ToDo : not yet described.

See Also:
  declare-constructor, define-constructor, new
"
		`(funcall (dynamic-new-imp ,type) ,@args))


	  ;;------------------------------------------------------------------------------
	  ;; macro new
	  #|
	  #|EXPORT|#				:new
	  |#
	  ;;------------------------------------------------------------------------------
	  (defmacro new (type &rest args)
		"    
Syntax:
  (new type &rest args)

Arguments and Values:
  type   : symbol of constructor name.
  args   : argument for constructor.

Description:
  ToDo : not yet described.

See Also:
  declare-constructor, define-constructor, dynamic-new
"
		(check-type type (and symbol (not keyword)))
		`(,(make-constructor-name type) ,@args))

)))


