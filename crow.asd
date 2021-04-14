(defsystem "crow"
  :version "0.1.5"
  :author "Walpurgisnatch"
  :license "MIT"
  :depends-on ("cl-ppcre")
  :components ((:module "src"
                :components
                ((:file "crow"))))
  :description "Creates unique wordlists from urls")
