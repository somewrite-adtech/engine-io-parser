(in-package :cl-user)
(defpackage engine-io-parser
  (:use :cl)
  (:import-from :engine-io-parser.error
                :decoding-error
                :inexistent-type-error
                :number-too-long-error
                :decoding-eof-error
                :invalid-length-error)
  (:import-from :base64
                :string-to-base64-string
                :base64-string-to-string
                :usb8-array-to-base64-string
                :base64-string-to-usb8-array )
  (:import-from :babel
                :octets-to-string
                :string-to-octets)
  (:import-from :fast-io
                :with-fast-output
                :fast-write-byte
                :fast-write-sequence
                :with-fast-input
                :fast-read-byte
                :fast-read-sequence
                :make-output-buffer
                :finish-output-buffer)
  (:import-from :alexandria
                :plist-hash-table)
  (:import-from :cl-utilities
                :collecting
                :collect)
  (:export :packet
           :make-packet
           :packet-type
           :packet-data
           :encode-packet
           :encode-base64-packet
           :decode-packet
           :decode-base64-packet
           :encode-payload
           :encode-payload-as-binary
           :decode-payload
           :decode-payload-as-binary

           :decoding-error
           :inexistent-type-error
           :number-too-long-error
           :decoding-eof-error
           :invalid-length-error))
(in-package :engine-io-parser)

(defparameter *packet-codes*
  (plist-hash-table
   '(:open    #\0
     :close   #\1
     :ping    #\2
     :pong    #\3
     :message #\4
     :upgrade #\5
     :noop    #\6)
   :test 'eq))

(defparameter *packet-names*
  (plist-hash-table
   '(#\0 :open
     #\1 :close
     #\2 :ping
     #\3 :pong
     #\4 :message
     #\5 :upgrade
     #\6 :noop)
   :test 'eql))

(defstruct (packet (:constructor %make-packet (type data)))
  type
  data)

(defun make-packet (&key type data)
  (unless (gethash type *packet-codes*)
    (error "Invalid packet type ~S" type))
  (%make-packet type data))

(defun packet-name (code)
  (the symbol (gethash code *packet-names*)))

(defun packet-type-code (packet)
  (the character (gethash (packet-type packet) *packet-codes*)))

(defun code-to-int (char)
  (declare (type character char))
  (the fixnum
       #.`(ecase char
            ,@(loop for i from 0 to 6
                    collect (list (aref (write-to-string i) 0) i)))))

(defun int-to-code (int)
  (declare (type fixnum int))
  (the character
       #.`(ecase int
            ,@(loop for i from 0 to 6
                    collect (list i (aref (write-to-string i) 0))))))

(defun fast-write-32bits (32bits-vector buffer)
  (map nil
       (lambda (bit)
         (fast-write-byte (ldb (byte 8  0) bit) buffer)
         (fast-write-byte (ldb (byte 8  8) bit) buffer)
         (fast-write-byte (ldb (byte 8 16) bit) buffer)
         (fast-write-byte (ldb (byte 8 24) bit) buffer))
       32bits-vector))

(defun encode-packet (packet &key supports-binary utf8-encode)
  (flet ((encode-octets (packet supports-binary)
           (if supports-binary
               (let ((data (packet-data packet))
                     (type (packet-type-code packet)))
                 (with-fast-output (buffer :vector)
                   (fast-write-byte (code-to-int type)
                                    buffer)
                   (fast-write-sequence data buffer)))
               (encode-base64-packet packet)))
         (encode-32bits (packet supports-binary)
           (if supports-binary
               (let ((type (packet-type-code packet)))
                 (with-fast-output (buffer :vector)
                   (fast-write-byte (code-to-int type) buffer)
                   (fast-write-32bits (packet-data packet) buffer)))
               (encode-base64-packet packet)))
         (encode-string (packet data utf8-encode)
           (let ((type (packet-type-code packet)))
             (if utf8-encode
                 (map 'string #'code-char
                      (with-fast-output (buffer :vector)
                        (fast-write-byte (char-code type) buffer)
                        (fast-write-sequence (string-to-octets data :encoding :utf-8)
                                             buffer)))
                 (let ((result (make-array (1+ (length data))
                                           :element-type 'character)))
                   (setf (aref result 0) type)
                   (replace result data :start1 1)
                   result)))))
    (let ((data (packet-data packet)))
      (etypecase data
        (null (string (packet-type-code packet)))
        ((vector (unsigned-byte 32))
         (encode-32bits packet supports-binary))
        ((vector (unsigned-byte 8))
         (encode-octets packet supports-binary))
        (string
         (encode-string packet data utf8-encode))
        (atom
         (encode-string packet (princ-to-string data) utf8-encode))))))

(defun encode-base64-packet (packet)
  (let ((data (packet-data packet))
        (type (packet-type-code packet)))
    (declare (type character type))
    (concatenate 'string
                 "b"
                 (string type)
                 (etypecase data
                   ((vector (unsigned-byte 8))
                    (usb8-array-to-base64-string data))
                   ((vector (unsigned-byte 32))
                    (usb8-array-to-base64-string
                     (with-fast-output (buffer :vector)
                       (fast-write-32bits data buffer))))
                   (string (string-to-base64-string data))
                   (null "")))))

(defun decode-packet (data)
  (etypecase data
    ((vector (unsigned-byte 8))
     (let ((type (packet-name (int-to-code (aref data 0)))))
       (unless type
         (error 'inexistent-type-error :data data :type (aref data 0)))
       (make-packet
        :type type
        :data (subseq data 1))))
    (string
     (cond
       ((eql (aref data 0) #\b)
        (decode-base64-packet (subseq data 1)))
       ((not (packet-name (aref data 0)))
        (error 'inexistent-type-error :data data :type (aref data 0)))
       (T (let ((type (packet-name (aref data 0))))
            (if (= (length data) 1)
                (make-packet :type type)
                (make-packet :type type
                             :data (subseq data 1)))))))))

(defun decode-base64-packet (data)
  (check-type data string)
  (let ((type (packet-name (aref data 0))))
    (unless type
      (error 'inexistent-type-error :data data :type (aref data 0)))
    (replace data data :start2 1)
    (make-packet :type type
                 :data
                 (base64-string-to-usb8-array
                  (adjust-array data (1- (length data)))))))

(defun encode-payload (packets &key supports-binary)
  (cond
    ((or (null packets)
         (and (vectorp packets)
              (zerop (length packets))))
     (if supports-binary
         (make-array 0 :element-type '(unsigned-byte 8))
         "0:"))
    (supports-binary
     (encode-payload-as-binary packets))
    (T
     (with-output-to-string (s)
       (map nil
            (lambda (packet)
              (let ((encoded (encode-packet packet
                                            :supports-binary supports-binary
                                            :utf8-encode t)))
                (prin1 (length encoded) s)
                (write-char #\: s)
                (princ (etypecase encoded
                         (string encoded)
                         ((vector (unsigned-byte 8))
                          (map 'string #'code-char encoded)))
                       s)))
            packets)))))

(defun encode-payload-as-binary (packets)
  (with-fast-output (buffer :vector)
    (map nil
         (lambda (packet)
           (let* ((encoded (encode-packet packet
                                          :supports-binary t
                                          :utf8-encode t))
                  (encoding-length (string-to-octets (prin1-to-string (length encoded)))))
             (fast-write-byte (if (stringp encoded)
                                  0
                                  1)
                              buffer)
             (fast-write-sequence encoding-length buffer)
             (fast-write-byte 255 buffer)
             (fast-write-sequence (if (stringp encoded)
                                      (string-to-octets encoded)
                                      encoded)
                                  buffer)))
         packets)))

(defun decode-payload (data)
  (unless (stringp data)
    (return-from decode-payload (decode-payload-as-binary data)))

  (when (string= data "")
    (error 'decoding-eof-error :data data))

  (collecting
    (do ((i 0 (1+ i))
         (length-buffer (make-string-output-stream)))
        ((<= (length data) i)
         (unless (string= (get-output-stream-string length-buffer)
                          "")
           (error 'decoding-eof-error :data data)))
      (let ((char (aref data i)))
        (if (char= char #\:)
            (let* ((length (get-output-stream-string length-buffer))
                   (length (handler-case (parse-integer length)
                             (error ()
                               (error 'invalid-length-error :data data :length length))))
                   (message (handler-case (subseq data (1+ i) (+ i length 1))
                              (error ()
                                (error 'decoding-eof-error :data data)))))
              (when (< 0 length)
                (collect (decode-packet message)))
              (incf i length)
              (setq length-buffer (make-string-output-stream)))
            (write-char char length-buffer))))))

(defun decode-payload-as-binary (data)
  (let ((eof '#:eof))
    (collecting
      (with-fast-input (buffer data)
        (do ((byte (fast-read-byte buffer nil eof)
                   (fast-read-byte buffer nil eof))
             (length-buffer (make-string-output-stream)))
            ((eq byte eof))
          (do ((i 1 (1+ i))
               (length-byte (fast-read-byte buffer nil eof)
                            (fast-read-byte buffer nil eof)))
              ((eql length-byte 255))
            (when (eq length-byte eof)
              (error 'decoding-eof-error :data data))
            (when (< 310 i)
              (error 'number-too-long-error :data data))
            (write-char (code-char length-byte) length-buffer))
          (let* ((length (get-output-stream-string length-buffer))
                 (length (handler-case (parse-integer length)
                           (error ()
                             (error 'invalid-length-error :data data :length length))))
                 (message
                   (make-array length :element-type '(unsigned-byte 8)))
                 (read-count (fast-read-sequence message buffer)))
            (unless (= read-count length)
              (error 'decoding-eof-error :data data))
            (collect
                (decode-packet
                 (if (= byte 0)
                     (octets-to-string message)
                     message)))))))))
