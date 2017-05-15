;;; ffi-tests.el -- tests for ffi

;; emacs -batch -Q -L . -l ffi-tests.el -f ert-run-tests-batch

;;; Code:

(require 'ert)
(require 'ffi)

(defmacro ffi-test-harness (&rest body)
  (declare (indent 0))
  `(let ((ffi-context nil))
     (unwind-protect
         (progn ,@body)
       (ffi-destroy ffi-context))))

(ert-deftest ffi-cif ()
  (ffi-test-harness
    (ffi-ensure)
    (eq (ffi-cif ffi-context [:uint32 :double :uint8])
        (ffi-cif ffi-context [:uint32 :double :uint8]))))

(ert-deftest ffi-cos ()
  (ffi-test-harness
    (let* ((v 1.2)
           (result (ffi-call nil "cos" [:double :double] v)))
      (should (< (abs (- (cos v) result)) 0.000001)))))

(ert-deftest ffi-errno ()
  (ffi-test-harness
    (let* ((result (ffi-call-errno nil "sqrt" [:double :double] -1))
           (value (car result))
           (errno (cdr result)))
      (should (isnan value))
      (should (= errno 33))))) ; TODO Have some way to get macros...

(ert-deftest ffi-infinity ()
  (ffi-test-harness
    (let ((result (ffi-call nil "log" [:double :double] 0)))
      (should (= -1.0e+INF result)))))

(ert-deftest ffi-deref ()
  (ffi-test-harness
    (let* ((ptr (ffi-call nil "malloc" [:pointer :uint64] 4))
           (len (ffi-call nil "snprintf" [:sint32 :pointer :uint64 :pointer :sint32] ptr 4 "%d" 123)))
      (should (= 3 len))
	  (should (equal (ffi-get-string ptr) "123"))
      (let ((one   (ffi-deref ptr :uint8 0))
            (two   (ffi-deref ptr :uint8 1))
            (three (ffi-deref ptr :uint8 2))
            (null  (ffi-deref ptr :uint8 3)))
        (should (= one   49))
        (should (= two   50))
        (should (= three 51))
        (should (= null   0)))
	  (let ((vals (ffi-read-array ptr :uint8 4)))
		(should (equal vals '(49 50 51 0))))
	  (let ((vals (ffi-read-struct ptr '(:uint8 1 :uint8 :uint8))))
		(should (equal vals '(49 51 0))))
      (ffi-call nil "free" [:void :pointer] ptr))))

(provide 'ffi-tests)

;;; ffi-tests.el ends here
