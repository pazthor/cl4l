(defpackage cl4l-table
  (:export clone-record do-table-iter
           make-table make-table-trans
           table table-clone table-commit table-delete
           table-diff table-dump
           table-find table-index table-iter table-join
           table-merge table-key
           table-length table-prev? table-read table-rollback
           table-slurp table-upsert table-write
           with-table-trans *table-trans*)
  (:shadowing-import-from cl4l-utils compare do-hash-table
                          key-gen when-let
                          with-symbols)
  (:use cl cl4l-event cl4l-iter cl4l-index cl4l-test))

(in-package cl4l-table)

;; Default trans
(defvar *table-trans* nil)

(defmacro do-table-iter ((expr key rec prev) &body body)
  (with-symbols (_it)
    `(with-iter nil ,expr
       (let* ((,_it iter-result)
              (,key (first ,_it))
              (,rec (second ,_it))
              (,prev (nthcdr 2 ,_it)))
         (declare (ignorable ,key ,rec ,prev))
         ,@body)
       (iter-next))))

(defmacro with-table-trans ((&key trans) &body body)
  ;; Executes BODY in transaction that is automatically
  ;; rolled back on early and committed on normal exit
  (with-symbols (_res)
    `(let ((*table-trans* (or ,trans (make-table-trans))))
       (unwind-protect
            (progn
              (let ((,_res (progn ,@body)))
                (table-commit)
                ,_res))
         (table-rollback)))))

(defstruct (tbl)
  idxs
  key-gen
  (on-delete (make-event))
  (on-upsert (make-event))
  (prev (make-hash-table :test #'eq))
  recs
  stream)

(defstruct (ch)
  op tbl rec prev)

(defun make-table (&key key key-gen (test #'equal) stream)
  (make-tbl :key-gen (or key-gen (key-gen key))
            :recs (make-hash-table :test test)
            :stream stream))

(defun table (key &rest recs)
  ;; Returns a new table with KEY, populated from RECS
  (let ((tbl (make-table :key key)))
    (dolist (rec recs tbl)
      (table-upsert tbl rec))))

(defun make-table-trans ()
  (list nil))

(defun table-trans-reset (self)
  (rplacd self nil))

(defun table-write (self op rec &key (stream (tbl-stream self)))
  (write (cons op rec) :stream stream)
  (terpri stream))

(defun table-clear (self)
  (clrhash (tbl-prev self))
  (clrhash (tbl-recs self)))

(defun table-clone (self)
  ;; Returns clone of SELF
  (let ((clone (make-table :key-gen (tbl-key-gen self)
                           :test (hash-table-test
                                  (tbl-recs self)))))
    (do-hash-table ((tbl-prev self) rec prev :result clone)
      (setf (gethash rec (tbl-prev self)) prev)
      (setf (gethash (table-key clone prev) (tbl-recs self))
            rec))))

(defun table-commit (&key (trans *table-trans*))
  ;; Clears changes made in TRANS
  (when trans
      (dolist (ch (nreverse (rest trans)))
        (when-let (stream (tbl-stream (ch-tbl ch)))
          (table-write (ch-tbl ch)
                       (ch-op ch)
                       (ch-prev ch)
                       :stream stream)))
      (table-trans-reset trans)))

(defun table-diff (self other)
  ;; Removes all records from SELF that are found in OTHER and
  ;; returns SELF.
  (do-hash-table ((tbl-prev other) rec prev :result self)
    (remhash rec (tbl-prev self))
    (remhash (table-key self prev) (tbl-recs self))))

(defun table-find (self key)
  ;; Returns record with KEY from SELF,
  ;; or NIL if not found.
  (gethash key (tbl-recs self)))

(defun table-index (self idx)
  (setf (tbl-idxs self) (adjoin idx (tbl-idxs self)))
  idx)

(defun table-iter (self)
  (do-hash-table ((tbl-recs self) key rec)
    (let ((prev (gethash rec (tbl-prev self))))
      (iter-yield (cons key (cons rec prev))))))

(defun table-join (self other)
  ;; Removes all records from SELF that are not found in OTHER and
  ;; returns SELF.
  (do-hash-table ((tbl-prev self) rec prev :result self)
    (unless (gethash rec (tbl-prev other))
      (remhash rec (tbl-prev self))
      (remhash (table-key self prev) (tbl-recs self)))))

(defun table-merge (self other)
  ;; Adds all records from OTHER that are not found in SELF and
  ;; returns SELF.
  (do-hash-table ((tbl-prev other) rec prev :result self)
    (setf (gethash rec (tbl-prev self)) prev)
    (setf (gethash (table-key self prev) (tbl-recs self)) rec)))

(defun table-key (self rec)
  (funcall (tbl-key-gen self) rec))

(defun table-length (self)
  (hash-table-count (tbl-recs self)))

(defun table-on-delete (self)
  (tbl-on-delete self))

(defun table-on-upsert (self)
  (tbl-on-upsert self))

(defun table-upsert (self rec &key (trans *table-trans*))
  (with-table-trans (:trans trans)
    (let ((key (table-key self rec))
          (prev (gethash rec (tbl-prev self))))
      (if trans
          (push (make-ch :op :upsert
                         :tbl self
                         :rec rec
                         :prev prev)
                (rest  trans))
          (when-let (stream (tbl-stream self))
            (table-write self :upsert (or prev rec)
                              :stream stream)))
    
      (when (tbl-idxs self)
        (let ((prev (gethash rec (tbl-prev self))))
          (dolist (idx (tbl-idxs self))
            (when prev
              (index-remove idx (index-key idx prev)))
            (index-add idx rec))))

      (event-publish (tbl-on-upsert self) self rec)
      (setf (gethash key (tbl-recs self)) rec)
      (setf (gethash rec (tbl-prev self)) (clone-record rec))))
  rec)

(defun table-delete (self rec &key (trans *table-trans*))
  (with-table-trans (:trans trans)
    (let ((prev (gethash rec (tbl-prev self))))
      (when prev
        (let ((key (table-key self rec)))
          (if trans
              (push (make-ch :op :delete
                             :tbl self
                             :rec rec
                             :prev prev)
                    (rest trans))
              (when-let (stream (tbl-stream self))
                (table-write self :delete prev :stream stream)))
        
          (when (tbl-idxs self)
            (dolist (idx (tbl-idxs self))
              (index-remove idx (index-key idx prev))))
          
          (event-publish (tbl-on-delete self) self rec)
          (remhash key (tbl-recs self))
          (remhash rec (tbl-prev self))))
      prev)))
  
(defun table-prev? (self rec)
  (gethash rec (tbl-prev self)))

(defun table-rollback (&key (trans *table-trans*))
  ;; Rolls back and clears changes made in TRANS
  (when trans
    (dolist (ch (nreverse (rest trans)))
      (ecase (ch-op ch)
        (:upsert
         (if (ch-prev ch)
             (table-upsert (ch-tbl ch) (ch-prev ch) :trans nil)
             (table-delete (ch-tbl ch) (ch-rec ch) :trans nil)))
        (:delete
         (table-upsert (ch-tbl ch) (ch-rec ch) :trans nil))))
      
    (table-trans-reset trans)))

(defun table-dump (self &key (stream (tbl-stream self)))
  (do-hash-table ((tbl-prev self) _ prev :result self)
    (table-write self :upsert prev :stream stream)))

(defun table-read (self &key (stream (tbl-stream self)))
  (when-let (ln (read-line stream nil))
    (let ((form (read-from-string ln)))
      (ecase (first form)
        (:upsert
         (table-upsert self (rest form)))
        (:delete
         (table-delete self
                       (table-find self
                                   (table-key self
                                              (rest form)))))))
    t))

(defun table-slurp (self &key (stream (tbl-stream self)))
  (tagbody
   next
     (when (table-read self :stream stream)
       (go next))))

(defgeneric clone-record (self)
  (:method (self)
    self)
  (:method ((self list))
    (copy-list self)))

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
      (do-table-iter ((table-iter tbl) key rec prev)
        (setf its (adjoin key its :test #'=)))

      (dotimes (i test-max)
        (assert (member i its :test #'=))))))

(define-test (:table :upsert :event)
  (let ((calls 0)
        (tbl (make-table)))
    (flet ((fn (tbl rec &key)
             (declare (ignore tbl rec))
             (incf calls)))
      (event-subscribe (table-on-upsert tbl) #'fn))
    (dotimes (i test-max)
      (table-upsert tbl i))
    (assert (= test-max calls))))
