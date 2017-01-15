(defpackage cl4l-iter
  (:export do-iter iter-next iter-result iter-yield with-iter)
  (:shadowing-import-from cl4l-utils symbol! with-symbols)
  (:use cl))

(in-package cl4l-iter)

(defmacro do-iter ((expr it) &body body)
  ;; Executes body with IT bound to items from EXPR
  `(with-iter nil ,expr
     (let ((,it (iter-result)))
       ,@body)
     (iter-next)))

(defmacro iter-yield (&optional result)
  ;; Signals ITER-yield with RESULT
  `(restart-case 
       (signal 'iter-yield :result ,result)
     (iter-next ())))

(defmacro with-iter (name expr &body body)
  ;; Executes BODY with EXPR bound to optional NAME,
  ;; ITER-aliases are provided for anonymous use.
  (let* ((_c (gensym))
         (_name (or name (gensym)))
         (_result (symbol! _name '-result)))
    `(macrolet ((,(symbol! _name '-next) ()
                  `(invoke-restart 'iter-next))
                (iter-next ()
                  `(,(symbol! ',_name '-next)))
                (,_result () `(result ,',_c))
                (iter-result () `(,',_result)))
       (handler-bind ((iter-yield
                        (lambda (,_c)
                          (declare (ignorable ,_c))
                          ,@body)))
         ,expr))))

(define-condition iter-yield (condition)
  ((result :initarg :result :reader result)))
