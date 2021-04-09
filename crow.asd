(defsystem "crow"
  :version "0.1.0"
  :author "Walpurgisnatch"
  :license "MIT"
  :depends-on ("cl-ppcre")
  :components ((:module "src"
                :components
                ((:file "crow"))))
  :description "car car")
