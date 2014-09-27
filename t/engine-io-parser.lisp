(in-package :cl-user)
(defpackage t-engine-io-parser
  (:use :cl
        :engine-io-parser
        :cl-test-more))
(in-package :t-engine-io-parser)

(plan 38)

(diag "parser")
(diag "basic functionality")

(defvar *encoded*
  (encode-packet
   (make-packet :type :message :data "test")))

(is-type *encoded* 'string
         "should encode packets as strings")
(is-type (decode-packet *encoded*) 'packet
         "should decode packets as 'packet's")

(let ((data (make-array 5 :element-type '(unsigned-byte 8)
                          :initial-contents '(0 1 2 3 4))))
  (is (decode-packet (encode-packet (make-packet :type :message :data data)))
      (make-packet :type :message :data data)
      :test 'equalp
      "should encode a binary message")

  (destructuring-bind (packet1 packet2)
      (decode-payload (encode-payload (list (make-packet :type :message :data data)
                                            (make-packet :type :message :data "hello"))))
    (is packet1
        (make-packet :type :message :data data)
        :test #'equalp
        "should encode/decode mixed binary and string contents as b64")
    (is packet2
        (make-packet :type :message :data "hello")
        :test #'equalp
        "should encode/decode mixed binary and string contents as b64")))

(let ((data1 (make-array 5 :element-type '(unsigned-byte 8)
                         :initial-contents '(0 1 2 3 4)))
      (data2 (make-array 4 :element-type '(unsigned-byte 8)
                           :initial-contents '(5 6 7 8))))
  (destructuring-bind (packet1 packet2)
      (decode-payload-as-binary
       (encode-payload-as-binary (list (make-packet :type :message :data data1)
                                       (make-packet :type :message :data data2))))
    (is packet1 (make-packet :type :message :data data1)
        :test 'equalp
        "should encode binary contents as binary")
    (is packet2 (make-packet :type :message :data data2)
        :test 'equalp
        "should encode binary contents as binary")))

(let ((data (make-array 123 :element-type '(unsigned-byte 8)
                            :initial-contents (loop for i from 0 to 122
                                                    collect i))))
  (destructuring-bind (packet1 packet2 packet3)
      (decode-payload-as-binary
       (encode-payload-as-binary (list (make-packet :type :message :data data)
                                       (make-packet :type :message :data "hello")
                                       (make-packet :type :close))))
    (is packet1
        (make-packet :type :message :data data)
        :test #'equalp
        "should encode mixed binary and string contents as binary")
    (is packet2
        (make-packet :type :message :data "hello")
        :test #'equalp
        "should encode mixed binary and string contents as binary")
    (is packet3
        (make-packet :type :close)
        :test #'equalp
        "should encode mixed binary and string contents as binary")))

(diag "encoding and decoding")

(let ((empty-packet (make-packet :type :message)))
  (is (decode-packet (encode-packet empty-packet))
      empty-packet
      :test 'equalp
      "should allow no data"))

(let ((json-packet (make-packet :type :open :data "{\"some\":\"json\"}")))
  (is (decode-packet (encode-packet json-packet))
      json-packet
      :test 'equalp
      "should encode an open packet"))

(is (decode-packet (encode-packet (make-packet :type :close)))
    (make-packet :type :close)
    :test 'equalp
    "should encode a close packet")

(is (decode-packet (encode-packet (make-packet :type :ping :data "1")))
    (make-packet :type :ping :data "1")
    :test 'equalp
    "should encode a ping packet")

(is (decode-packet (encode-packet (make-packet :type :pong :data "1")))
    (make-packet :type :pong :data "1")
    :test 'equalp
    "should encode a pong packet")

(is (decode-packet (encode-packet (make-packet :type :message :data "aaa")))
    (make-packet :type :message :data "aaa")
    :test 'equalp
    "should encode a message packet")

(is (decode-packet (encode-packet (make-packet :type :message :data "utf8 — string")))
    (make-packet :type :message :data "utf8 — string")
    :test 'equalp
    "should encode a utf8 special chars message packet")

(is (encode-packet (make-packet :type :message :data "€€€"))
    "4€€€"
    :test #'string=
    "should not utf8 encode by default")

(is (encode-packet (make-packet :type :message :data "€€€") :utf8-encode t)
    "4â¬â¬â¬"
    :test #'string=
    "should not utf8 encode by default")

(is (decode-packet (encode-packet (make-packet :type :message :data 1)))
    (make-packet :type :message :data "1")
    :test #'equalp
    "should encode a message packet coercing to string")

(is (decode-packet (encode-packet (make-packet :type :upgrade)))
    (make-packet :type :upgrade)
    :test #'equalp
    "should encode an upgrade packet")

(like (encode-packet (make-packet :type :message :data "test"))
      "^[0-9]"
      "should match the encoding format")

(like (encode-packet (make-packet :type :message))
      "^[0-9]$"
      "should match the encoding format")

(diag "decoding error handling")

(is-error (decode-packet ":::")
          decoding-error
          "should disallow bad format")

(is-error (decode-packet "94103")
          inexistent-type-error
          "should disallow inexistent types")


(diag "payloads")
(diag "basic functionality")

(is-type (encode-payload (list (make-packet :type :ping)
                               (make-packet :type :close)))
         'string
         "should encode payloads as strings")

(ok (decode-payload (encode-payload (list (make-packet :type :message :data "a"))))
    "should encode/decode packets")

(destructuring-bind (message-packet
                     ping-packet)
    (decode-payload
     (encode-payload (list (make-packet :type :message :data "a")
                           (make-packet :type :ping))))
  (is (packet-type message-packet) :message
      "should encode/decode packets")
  (is (packet-type ping-packet) :ping
      "should encode/decode packets"))

(is (decode-payload (encode-payload '()))
    nil
    "should encode/decode empty payloads")

(is-error (decode-payload "1!")
          decoding-error
          "should error on bad payload format")

(is-error (decode-payload "")
          decoding-error
          "should error on bad payload format")

(is-error (decode-payload "))")
          decoding-error
          "should error on bad payload format")

(is-error (decode-payload (make-array 64 :element-type '(unsigned-byte 8)))
          decoding-error
          "should error on bad payload format")

(is-error (decode-payload "1:")
          decoding-error
          "should error on bad payload length")

(is-error (decode-payload "3:99:")
          decoding-error
          "should error on bad packet format")

(is-error (decode-payload "1:aa")
          decoding-error
          "should error on bad packet format")

(is-error (decode-payload "1:a2:b")
          decoding-error
          "should error on bad packet format")

(finalize)
