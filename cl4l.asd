(asdf:defsystem cl4l
  :description "esoteric CL essentials"
  :author "Andreas <codr4life@gmail.com>"
  :license "MIT"
  :serial t
  :components ((:file "cl4l")
               (:file "macro-utils")
               (:file "utils")
               (:file "slist")
               (:file "index")
               (:file "memoize")))