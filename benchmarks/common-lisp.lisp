(declaim (optimize (speed 3) (safety 0) (debug 0)))
(ql:quickload :engine-io-parser)

(use-package :engine-io-parser)

(defvar *binary-data* (make-array 5 :element-type '(unsigned-byte 8) :initial-contents '(0 1 2 3 4)))

(time
 (dotimes (i 10000)
   (encode-packet (make-packet :type :message :data "Hi, there") :supports-binary t)))

(time
 (dotimes (i 10000)
   (encode-packet (make-packet :type :message :data *binary-data*) :supports-binary t)))

(time
 (dotimes (i 10000)
   (decode-packet "2probe")))

(defvar *binary-packet* (make-array 6 :element-type '(unsigned-byte 8) :initial-contents '(4 0 1 2 3 4)))

(time
 (dotimes (i 10000)
   (decode-packet *binary-packet*)))

(time
 (dotimes (i 10000)
   (encode-payload (list (make-packet :type :message :data *binary-data*)
                         (make-packet :type :close)))))

(time
 (dotimes (i 10000)
   (decode-payload "10:b4AAECAwQ=1:1")))
