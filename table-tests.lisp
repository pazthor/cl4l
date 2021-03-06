(defpackage cl4l-table-tests
  (:use cl cl4l-event cl4l-table cl4l-test))

(in-package cl4l-table-tests)

(defparameter test-max 1000)

(define-test (:table :stream)
  (with-output-to-string (out)
    (let ((tbl (make-table :key #'first :stream out)))
      (table-upsert tbl '(1 2 3))
      (table-upsert tbl '(1 3 4))
      (let ((rec '(2 3 4)))
        (table-upsert tbl rec)
        (table-delete tbl rec))
      (table-clear tbl)
      (let ((data (get-output-stream-string out)))
        (with-input-from-string (in data)
          (table-slurp tbl :stream in))
        (assert (equal '(1 3 4) (table-find tbl 1)))
        (assert (null (table-find tbl 2)))))))

(define-test (:table :dump)
  (let ((tbl (make-table :key #'first)))
    (table-upsert tbl '(1 2 3))
    (table-upsert tbl '(1 3 4))
    (let ((rec '(2 3 4)))
      (table-upsert tbl rec)
      (table-delete tbl rec))
    (let ((data (with-output-to-string (out)
                  (table-dump tbl :stream out))))
      (table-clear tbl)
      (with-input-from-string (in data)
        (table-slurp tbl :stream in))
      (assert (equal '(1 3 4) (table-find tbl 1)))
      (assert (null (table-find tbl 2))))))

(define-test (:table :iter)
  (let ((tbl (make-table)))
    (dotimes (i test-max)
      (table-upsert tbl i))
    
    (let ((its))
      (do-table ((table-iter tbl) key rec prev)
        (setf its (adjoin key its :test #'=)))

      (dotimes (i test-max)
        (assert (member i its :test #'=))))))

(define-test (:table :upsert :event)
  (let ((calls 0)
        (tbl (make-table)))
    (flet ((fn (rec prev)
             (declare (ignore rec prev))
             (incf calls)))
      (event-subscribe (table-on-upsert tbl) #'fn))
    (dotimes (i test-max)
      (table-upsert tbl i))
    (assert (= test-max calls))))
