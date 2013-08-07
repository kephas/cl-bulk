(in-package :nothos.net/2013.08.bulk)

(defun %read-form-payload (stream)
  (let@ rec ((expressions)
	     (next (read-bulk stream)))
    (if (eq next :end)
	(reverse expressions)
	(rec (cons next expressions) (read-bulk stream)))))

(defun %read-array-payload (stream)
  (let* ((size (read-bulk stream))
	 (array (make-array size :element-type '(unsigned-byte 8))))
    (read-sequence array stream)
    array))

(defun %read-unsigned-payload (stream bytes)
  (let@ rec ((count bytes)
	     (value 0))
    (if (zerop count)
	value
	(progn
	  (setf (ldb (byte 8 (* 8 (1- count))) value) (read-byte stream))
	  (rec (1- count) value)))))

(defun %read-signed-payload (stream)
  (1+ (boole boole-c1 (read-bulk stream) 0)))

(define-condition parsing-error (error) ())

(defun read-bulk (stream)
  (let ((marker (read-byte stream)))
    (case marker
      (0 nil)
      (1 (%read-form-payload stream))
      (2 :end)
      (3 (%read-array-payload stream))
      (4 (%read-unsigned-payload stream 1))
      (5 (%read-unsigned-payload stream 2))
      (6 (%read-unsigned-payload stream 4))
      (7 (%read-unsigned-payload stream 8))
      (8 (%read-unsigned-payload stream 16))
      (9 (%read-signed-payload stream))
      ((10 11 12 13 14 15) (error 'parsing-error))
      (t (%read-ref-payload stream marker)))))
