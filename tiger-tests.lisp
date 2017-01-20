(defpackage cl4l-tiger-tests
  (:use cl cl4l-tiger cl4l-test))

(in-package cl4l-tiger-tests)

(defparameter *test-max* 1000)
(defparameter *test-threads* 4)

(defmethod run-suite :around (run &key)
  (with-tiger-context ()
    (start-tigers *test-threads*)
    (call-next-method)
    (stop-tigers *test-threads*)
    (join-tigers)))

(define-test (:tiger :do-tiger)
  (let ((tgr (define-tiger (:max-stride *test-max*)
               (dotimes (i *test-max*)
                 (tiger-yield i))))
        (j 0))
    (do-tiger (tgr i)
      (assert (= j i))
      (incf j))
    (assert (= *test-max* j))))

