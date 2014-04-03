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
    (eq (ffi-cif ffi-context '(:uint32 :double :uint8))
        (ffi-cif ffi-context '(:uint32 :double :uint8)))))

(ert-deftest ffi-cos ()
  (ffi-test-harness
    (let* ((v 1.2)
           (result (ffi-call nil "cos" '(:double :double) v)))
      (should (< (abs (- (cos v) result)) 0.000001)))))

(provide 'ffi-tests)

;;; ffi-tests.el ends here
