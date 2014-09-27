(in-package :cl-user)
(defpackage engine-io-parser.error
  (:use :cl)
  (:export :engine-io-protocol-error
           :decoding-error
           :inexistent-type-error
           :number-too-long-error
           :decoding-eof-error
           :invalid-length-error))
(in-package :engine-io-parser.error)

(define-condition engine-io-protocol-error (simple-error) ())

(define-condition decoding-error (engine-io-protocol-error)
  ((data :initarg :data))
  (:report
   (lambda (condition stream)
     (format stream
             "Error while decoding ~S." (slot-value condition 'data)))))

(define-condition inexistent-type-error (decoding-error)
  ((type :type character
         :initarg :type))
  (:report
   (lambda (condition stream)
     (format stream
             "Error while decoding ~S.
Inexistent type: ~C"
             (slot-value condition 'data)
             (slot-value condition 'type)))))

(define-condition number-too-long-error (decoding-error) ()
  (:report
   (lambda (condition stream)
     (format stream
             "Error while decoding ~S.
Packet length is too long (less than 1e310 is allowed)."
             (slot-value condition 'data)))))

(define-condition decoding-eof-error (decoding-error) ()
  (:report
   (lambda (condition stream)
     (format stream
             "Error while decoding ~S.
End of file."
             (slot-value condition 'data)))))

(define-condition invalid-length-error (decoding-error)
  ((length :type :length))
  (:report
   (lambda (condition stream)
     (format stream
             "Error while decoding ~S.
Packet length is not an integer: ~S"
             (slot-value condition 'data)
             (slot-value condition 'length)))))
