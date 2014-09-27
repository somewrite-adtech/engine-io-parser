# Engine.IO Parser

This is a Common Lisp parser for [Engine.IO protocol](https://github.com/Automattic/engine.io-protocol).

## Usage

```common-lisp
;;
;; Encoding a packet

(encode-packet (make-packet :type :ping :data "probe"))
;=> "2probe"


;;
;; Encoding a binary message

(encode-packet (make-packet :type :message
                            :data (make-array 5
                                              :element-type '(unsigned-byte 8)
                                              :initial-contents '(0 1 2 3 4))))
;=> "b4AAECAwQ="

(encode-packet (make-packet :type :message
                            :data (make-array 5
                                              :element-type '(unsigned-byte 8)
                                              :initial-contents '(0 1 2 3 4)))
               :supports-binary t)
;=> #(4 0 1 2 3 4)


;;
;; Decoding a packet

(decode-packet "2probe")
;=> #S(PACKET :TYPE :PING :DATA "probe")

(decode-packet "b4AAECAwQ=")
;=> #S(PACKET :TYPE :MESSAGE :DATA #(0 1 2 3 4))

(decode-packet (make-array 6 :element-type '(unsigned-byte 8) :initial-contents '(4 0 1 2 3 4)))
;=> #S(PACKET :TYPE :MESSAGE :DATA #(0 1 2 3 4))
```

## Benchmark

### Node.js

Node.js ver 0.10.21

```
node benchmarks/node.js
```

```
text encoding: 3.8 milliseconds
binary encoding: 74.2 milliseconds
text decoding: 1.6 milliseconds
binary decoding: 70.6 milliseconds
```

### Common Lisp

SBCL ver 1.2.1

```
sbcl --load benchmarks/common-lisp.lisp --eval '(quit)'
```

```
Evaluation took:
  0.002 seconds of real time
  0.001976 seconds of total run time (0.001976 user, 0.000000 system)
  100.00% CPU
  5,907,963 processor cycles
  982,096 bytes consed
  
Evaluation took:
  0.002 seconds of real time
  0.001474 seconds of total run time (0.001474 user, 0.000000 system)
  50.00% CPU
  4,410,867 processor cycles
  1,605,248 bytes consed
  
Evaluation took:
  0.001 seconds of real time
  0.000824 seconds of total run time (0.000823 user, 0.000001 system)
  100.00% CPU
  2,461,671 processor cycles
  819,008 bytes consed
  
Evaluation took:
  0.000 seconds of real time
  0.000662 seconds of total run time (0.000662 user, 0.000000 system)
  100.00% CPU
  1,978,596 processor cycles
  622,576 bytes consed
```

### Conclusion

|                 | Node.js | Common Lisp |
| -------------   | -------:| -----------:|
| text encoding   |     3.8 |         2.0 |
| binary encoding |    74.2 |         1.5 |
| text decoding   |     1.6 |         0.8 |
| binary decoding |    70.6 |         0.7 |

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2014 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the BSD 2-Clause License.
