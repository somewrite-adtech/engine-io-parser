#|
  This file is a part of Engine.IO Parser project.
  Copyright (c) 2014 Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage engine-io-parser-asd
  (:use :cl :asdf))
(in-package :engine-io-parser-asd)

(defsystem engine-io-parser
  :name "Engine.IO Parser"
  :version "0.0.1"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on (:fast-io
               :babel
               :cl-base64
               :cl-utilities
               :alexandria)
  :components ((:module "src"
                :components
                ((:file "engine-io-parser" :depends-on ("error"))
                 (:file "error"))))
  :description "Parser for the engine.io protocol"
  :in-order-to ((test-op (test-op t-engine-io-parser))))
