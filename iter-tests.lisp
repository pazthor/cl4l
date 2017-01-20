(defpackage cl4l-iter-tests
  (:shadowing-import-from bordeaux-threads
                          join-thread make-thread)
  (:use cl cl4l-iter cl4l-test cl4l-tiger cl4l-chan))

(in-package cl4l-iter-tests)

(defparameter *test-max* 100000)

(define-test (:iter :perf :cond)
  (flet ((foo (max)
           (dotimes (i max)
             (iter-yield i))))
    (let ((j 0))
      (with-iter ((foo *test-max*))
        (assert (= j (iter-result)))
        (incf j)
        (iter-next)))))

(define-test (:iter :perf :list)
  (flet ((foo ()
           (let ((res))
             (dotimes (i *test-max*)
               (push i res))
             (nreverse res))))
    (let ((res (foo)))
      (dotimes (j *test-max*)
        (assert (= (pop res) j))))))

(define-test (:iter :perf :tiger)
  (let ((tgr (define-tiger (*test-max*)
               (dotimes (i *test-max*)
                 (tiger-yield i)))))
    (dotimes (j *test-max*)
      (assert (= j (tiger-next tgr))))))
