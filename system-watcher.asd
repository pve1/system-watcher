;;;; system-watcher.asd

(asdf:defsystem #:system-watcher
  :description "A tool that let's you continually run your tests in a terminal while
you code. Saving a file that belongs to the system you're working on
will automatically trigger a reload (from source) and test run."
  :author "Peter von Etter <your.name@example.com.invalid>"
  :license  "LLGPL"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "system-watcher"))
  :depends-on (:apply-argv
               :alexandria
               :shell-utility))
