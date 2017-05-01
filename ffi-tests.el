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

(provide 'ffi-tests)

;;; ffi-tests.el ends here
