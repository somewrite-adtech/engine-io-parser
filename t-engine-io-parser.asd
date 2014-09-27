#|
  This file is a part of Engine.IO Parser project.
  Copyright (c) 2014 Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage t-engine-io-parser-asd
  (:use :cl :asdf))
(in-package :t-engine-io-parser-asd)

(defsystem t-engine-io-parser
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on (:engine-io-parser
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:test-file "engine-io-parser"))))

  :defsystem-depends-on (:cl-test-more)
  :perform (test-op :after (op c)
                    (funcall (intern #. (string :run-test-system) :cl-test-more)
                             c)
                    (asdf:clear-system c)))
