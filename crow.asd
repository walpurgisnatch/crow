(defsystem "crow"
    :version "0.2.4"
    :author "Walpurgisnatch"
    :license "MIT"
    :depends-on ("cl-ppcre"
                 "quri")
    :components ((:module "src"
                  :components
                  ((:file "crow"))))
    :description "Creates unique wordlists from urls")
