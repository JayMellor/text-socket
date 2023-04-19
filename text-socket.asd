(defsystem "text-socket"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("usocket")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "text-socket/tests"))))

(defsystem "text-socket/tests"
  :author ""
  :license ""
  :depends-on ("text-socket"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for text-socket"
  :perform (test-op (op c) (symbol-call :rove :run c)))
