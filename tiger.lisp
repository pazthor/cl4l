(defpackage cl4l-tiger
  (:export define-tiger do-tiger join-tigers
           start-tigers stop-tigers tiger-next tiger-yield)
  (:shadowing-import-from bordeaux-threads
                          destroy-thread make-thread thread-yield)
  (:shadowing-import-from cl4l-utils
                          when-let with-symbols)
  (:use cl cl4l-chan cl4l-semaphore cl4l-test))

(in-package cl4l-tiger)

(defvar *tiger-threads* nil)
(defparameter *tiger-max-queue* 16)
(defvar *queue* (make-chan :max-length *tiger-max-queue*))

(defstruct (tiger (:conc-name tgr-) (:constructor make-tgr))
  chan)

(defmacro define-tiger ((max-length) &body body)
  (with-symbols (_chan _tgr)
    `(let* ((,_chan (make-chan :max-length ,max-length))
            (,_tgr (make-tgr :chan ,_chan)))
       (chan-put *queue* (lambda ()
                           (macrolet ((tiger-yield (it)
                                        `(chan-put ,',_chan ,it)))
                             ,@body
                             (tiger-yield nil))))
       ,_tgr)))

(defmacro do-tiger ((tgr it) &body body)
  (with-symbols (_start)
    `(tagbody
        ,_start
        (when-let (,it (tiger-next ,tgr))
          ,@body
          (go ,_start)))))

(defun tiger-loop ()
  (tagbody 
   start
     (when-let (fn (chan-get *queue*))
       (funcall fn)
       (go start))))

(defun start-tigers (nthreads)
  (dotimes (_ nthreads)
    (push (make-thread #'tiger-loop) *tiger-threads*)))

(defun stop-tigers (nthreads)
  (dotimes (_ nthreads)
    (chan-put *queue* nil)))

(defun join-tigers ()
  (dolist (thread *tiger-threads*)
    (destroy-thread thread))
  (setf *tiger-threads* nil))

(defun tiger-next (self)
  (chan-get (tgr-chan self)))
