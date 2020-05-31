;;;; system-watcher.lisp

(in-package #:system-watcher)

(defclass watcher ()
  ((systems :initarg :systems
            :accessor systems
            :initform nil)
   (test-function :initarg :test-function
                  :accessor test-function
                  :initform nil)
   (prepare-done-p :initarg :prepare-done-p
                   :accessor prepare-done-p
                   :initform nil)))

(defmethod initialize-instance :after ((self watcher) &key test-function systems)
  (when systems
    (setf (systems self) (mapcar (lambda (x) (string-downcase (string x))) systems)))
  (when test-function
    (setf (test-function self) (string-upcase test-function))))

(defun watcher (systems &optional test-function)
  (make-instance 'watcher
                 :systems (ensure-list systems)
                 :test-function test-function))

(defgeneric files-to-watch (watcher)
  (:method ((w watcher))
    (let ((files (append (loop :for s :in (systems w)
                               :collect (asdf:system-source-file
                                         (asdf:find-system s)))
                         (loop :for s :in (systems w)
                               :append (asdf:input-files
                                        'asdf:concatenate-source-op s)))))
      (mapcar 'namestring files))))

;; Wait for change.
(defgeneric watch-files (watcher)
  (:method ((w watcher))
    (assert (files-to-watch w))
    (uiop:run-program (format nil "inotifywait -e modify ~{~S ~}"
                              (files-to-watch w)))))

;; Load dependencies normally (not from source).
(defgeneric prepare (watcher)
  (:method ((w watcher))
    (unless (prepare-done-p w)
      (let (systems-prepared-for)
        (dolist (s (systems w))
          (asdf:operate 'asdf:prepare-op s
                        :verbose t
                        :force-not (set-difference (systems w)
                                                   (list s)
                                                   :test #'string=))
          (push s systems-prepared-for)))
      (setf (prepare-done-p w) t))))

;; If we are ever able to loop perform-load within the same process in
;; such a way that memory usage stays constant, here is where cleanup
;; code should go (delete packages etc).

(defgeneric cleanup (watcher)
  (:method (watcher)
    nil))

(defun pick-next-system-to-load (systems)
  (find-if (lambda (x)
             (let ((deps (asdf:system-depends-on
                          (asdf:find-system x))))
               (null (intersection systems deps :test #'string=))))
           systems))

;; Load systems from source.
(defgeneric perform-load (watcher)
  (:method ((w watcher))
    (handler-bind ((style-warning #'muffle-warning))
      (let ((systems-to-load (systems w))
            loaded-systems)
        (loop :while systems-to-load
              :do (let ((next (pick-next-system-to-load systems-to-load)))
                    (assert next)
                    (setf systems-to-load (remove next systems-to-load
                                                  :test #'string=))
                    (asdf:operate 'asdf:load-source-op next
                                  :verbose t
                                  :force-not (asdf:already-loaded-systems))))))))

(defgeneric run-tests (watcher)
  (:method ((w watcher))
    (handler-bind ((style-warning #'muffle-warning))
      (when (test-function w)
        (funcall (read-from-string (test-function w)))))))

(defgeneric watch-systems (watcher)
  (:method ((w watcher))
    (prepare w)
    (with-simple-restart (out "Get out.")
      (handler-bind ((error (lambda (c)
                              (format t "~%--------------------------------------~%~%")
                              (format t "ERROR: ~A~%~%" c)
                              #+sbcl (sb-debug:print-backtrace :count 10)
                              (terpri)
                              (invoke-restart 'out))))
        (perform-load w)
        (run-tests w)))
    (cleanup w)
    (format t "Waiting.~%")
    (sleep 0.2)
    (watch-files w)))


;; Either run main-watcher/sbcl or save a core that does so.
#+sbcl
(defun main/sbcl (&key systems test-function core)
  (setf *load-verbose* t)
  (if core
      (let ((watcher (watcher systems test-function))
            (core-name (if (equal "t" core)
                           (format nil "~A-system-watcher.core"
                                   (first systems))
                           core)))
        (prepare watcher)
        (sb-ext:save-lisp-and-die
         core-name
         :toplevel (lambda () (main-watcher/sbcl watcher))
         :executable t
         :compression t))
      (let ((watcher (watcher systems test-function)))
        (prepare watcher)
        (main-watcher/sbcl watcher))))

;; Load the files, run the tests, then quit when a change is
;; detected. Interactive interrupt (^C) should result in non-zero exit
;; status.
#+sbcl
(defun main-watcher/sbcl (watcher)
  (sb-ext:disable-debugger)
  (handler-case
      (watch-systems watcher)
    (serious-condition (x)
      (format t "~%Received ~A~%" x)
      (uiop:quit 1)))
  (uiop:quit))

#+sbcl
(defun write-watcher-utility/sbcl ()
  (shell-utility:write-shell-utility :sbcl
    (:name "system-watcher-sbcl")
    (:load-system :system-watcher)
    (:launch main/sbcl :rest-keyword :systems)))

;; C-x C-e
;; (write-watcher-utility/sbcl)
