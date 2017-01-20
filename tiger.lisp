(defpackage cl4l-tiger
  (:export define-tiger do-tiger join-tigers
           make-tiger-context
           start-tigers stop-tigers tiger-next tiger-yield
           with-tiger-context *tiger-context*)
  (:shadowing-import-from bordeaux-threads
                          destroy-thread make-thread thread-yield)
  (:shadowing-import-from cl4l-utils
                          when-let with-symbols)
  (:use cl cl4l-chan cl4l-semaphore cl4l-test))

(in-package cl4l-tiger)

(defvar *tiger-context*)

(defstruct (tiger (:conc-name tgr-)
                  (:constructor make-tgr))
  chan)

(defstruct (tiger-context (:conc-name cx-)
                          (:constructor make-cx))
  threads queue)

(defun make-tiger-context (&key (max-queue 0))
  (make-cx :queue (make-chan :max-length max-queue)))

(defmacro define-tiger ((&key context
                              (max-stride 0))
                        &body body)
  (with-symbols (_chan _tgr)
    `(let* ((,_chan (make-chan :max-length ,max-stride))
            (,_tgr (make-tgr :chan ,_chan)))
       (chan-put (cx-queue (or ,context *tiger-context*))
                 (lambda ()
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

(defmacro with-tiger-context ((&key context (max-queue 0))
                              &body body)
  "Executes BODY in context" 
  `(let ((*tiger-context* (or ,context
                              (make-tiger-context :max-queue
                                                  ,max-queue))))
     ,@body))

(defun tiger-loop (context)
  (tagbody 
   start
     (when-let (fn (chan-get (cx-queue context)))
       (funcall fn)
       (go start))))

(defun start-tigers (nthreads &key (context *tiger-context*))
  (dotimes (_ nthreads)
    (push (make-thread (lambda () (tiger-loop context)))
          (cx-threads context))))

(defun stop-tigers (nthreads &key (context *tiger-context*))
  (dotimes (_ nthreads)
    (chan-put (cx-queue context) nil)))

(defun join-tigers (&key (context *tiger-context*))
  (dolist (thread (cx-threads context))
    (destroy-thread thread))
  (setf (cx-threads context) nil))

(defun tiger-next (self)
  (chan-get (tgr-chan self)))
