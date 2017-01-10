(defpackage cl4l-utils-tests
  (:import-from cl4l-test define-test)
  (:use cl cl4l-utils))

(in-package cl4l-utils-tests)

(define-test (:defer :nested)
  (assert (string= "hello world"
                   (with-output-to-string (out)
                     (with-defer (outer)
                       (with-defer ()
                         (defer (format out "hello"))
                         (defer-outer (format out "world")))
                       (format out " "))))))