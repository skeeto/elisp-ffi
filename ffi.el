;;; ffi.el --- foreign function interface -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar ffi-types
  '(:uint8 :uint16 :uint32 :unit64
    :sint8 :sint16 :sint32 :sint64
    :float :double :pointer)
  "Data type designators.")

(progn
  (put :uint8   'ffi-code "u")
  (put :uint16  'ffi-code "v")
  (put :uint32  'ffi-code "w")
  (put :uint64  'ffi-code "x")
  (put :sint8   'ffi-code "i")
  (put :sint16  'ffi-code "j")
  (put :sint32  'ffi-code "k")
  (put :sint64  'ffi-code "l")
  (put :float   'ffi-code "f")
  (put :double  'ffi-code "d")
  (put :pointer 'ffi-code "p"))

(defvar ffi-data-root (file-name-directory load-file-name)
  "Location of FFI data files")

(defvar ffi-context nil
  "Context for FFI calls.")

(cl-defstruct (ffi (:constructor ffi--create))
  (process nil)
  (log nil)
  (cifs (make-hash-table :test 'equal))
  (libs (make-hash-table :test 'equal))
  (syms (make-hash-table :test 'equal)))

(cl-defun ffi-create (&optional (name "ffi"))
  "Create and return a new FFI context."
  (let* ((process-connection-type nil)  ; use a pipe
         (buffer (generate-new-buffer (format "*%s*" name)))
         (exec (expand-file-name "glue/ffi-glue" ffi-data-root))
         (process (start-process name buffer exec)))
    ;; (setf (process-sentinel process)
    ;;       (lambda (proc _) (kill-buffer (process-buffer proc))))
    (ffi--create :process process :log (generate-new-buffer "*out*"))))

(defun ffi-destroy (ffi)
  "Destroy an FFI context."
  (kill-process (ffi-process ffi)))

(defun ffi-ensure ()
  "Ensure that `ffi-context' is initialized."
  (when (or (null ffi-context) (not (process-live-p (ffi-process ffi-context))))
    (setf ffi-context (ffi-create))))

(defun ffi-write (ffi program)
  "Write stack PROGRAM to FFI."
  (let ((log (ffi-log ffi)))
    (when log
      (with-current-buffer log
        (insert program))))
  (process-send-string (ffi-process ffi) program))

(defun ffi-read (ffi)
  "Read a value from FFI."
  (let ((process (ffi-process ffi)))
    (with-current-buffer (process-buffer process)
      (while (not (eql ?$ (char-after (1- (point-max)))))
        (accept-process-output))
      (setf (point) (point-min))
      (prog1 (read (current-buffer))
        (erase-buffer)))))

(defun ffi-push (ffi type value)
  "Push VALUE onto FFI's stack."
  (cond ((null value) (ffi-write ffi "p0"))
        ((stringp value) (ffi-write ffi "w%dM%s" (length value) value))
        ((ffi-write ffi (concat (get type 'ffi-code)
                                (prin1-to-string value) " ")))))

(defun ffi-pop (ffi)
  "Return value from the top of FFI's stack."
  (ffi-write ffi "o")
  (ffi-read ffi))

(defun ffi-peek (ffi)
  "Return value from the top of FFI's stack."
  (ffi-write ffi "e")
  (ffi-read ffi))

(defun ffi-cif (ffi signature)
  "Create/fetch a call interface handle in FFI for SIGNATURE.
The signature must be a list of type designators (:unit8,
:double, etc.). The required first element is the return type and
the rest are the argument types."
  (or (gethash signature (ffi-cifs ffi))
      (with-temp-buffer
        (let ((standard-output (current-buffer)))
          (dolist (arg (reverse signature))
            (princ (get arg 'ffi-code))
            (princ "0"))
          (princ "w")
          (princ (1- (length signature)))
          (princ "Co")
          (ffi-write ffi (buffer-string))
          (setf (gethash signature (ffi-cifs ffi)) (ffi-read ffi))))))

(defun ffi-dlopen (ffi name)
  "Fetch a handle (pointer) by library NAME."
  (or (gethash name (ffi-libs ffi))
      (progn
        (ffi-write ffi (format "w%dM%sOo" (length name) name))
        (setf (gethash name (ffi-libs ffi)) (ffi-read ffi)))))

(defun ffi-dlsym (ffi library name)
  "Fetch a handle for NAME from LIRBARY in FFI."
  (or (gethash name (ffi-syms ffi))
      (progn
        (ffi-write ffi (format "p%sw%dM%sSo" (or library 0) (length name) name))
        (setf (gethash name (ffi-syms ffi)) (ffi-read ffi)))))

(defun ffi-call (library symbol signature &rest args)
  (ffi-ensure)
  (let* ((cif (ffi-cif ffi-context signature))
         (ptr-library (if library (ffi-dlopen ffi-context library) nil))
         (ptr-symbol  (ffi-dlsym ffi-context ptr-library symbol)))
    (cl-loop for arg in (reverse args)
             for type in (reverse (cdr signature))
             do (ffi-push ffi-context type arg))
    (ffi-push ffi-context :pointer cif)
    (ffi-push ffi-context :pointer ptr-symbol)
    (ffi-write ffi-context "c")
    (ffi-pop ffi-context)))

;; test area

(let ((ffi (ffi-create)))
  (prog1
      (list (ffi-cif ffi '(:uint32 :double :uint8))
            (ffi-cif ffi '(:uint32 :double :uint8))
            (ffi-dlopen ffi "libm.so")
            (ffi-dlsym ffi nil "cos"))
    (ffi-destroy ffi)))

(let ((ffi (ffi-create)))
  (ffi-write ffi "d1.2d0d0w1Cp0w3McosSco")
  (prog1 (ffi-read ffi)
    (ffi-destroy ffi)))

(ffi-call nil "cos" '(:double :double) 1.2)
(ffi-call nil "srand" '(:sint32 :uint32) 0)
(cl-loop repeat 10 collect (ffi-call nil "rand" '(:sint32)))

(provide 'ffi)

;;; ffi.el ends here
