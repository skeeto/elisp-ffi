;;; ffi.el --- foreign function interface -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; The main function to be concerned about is `ffi-call'. It takes a
;; library name, a symbol name, a function signature (a vector of
;; types from `ffi-types'), and a list of arguments.

;;     (ffi-call nil "cos" [:double :double] 1.2)
;;     (ffi-call nil "rand" [:sint32])

;; If you get the function signature wrong, the FFI context may crash.

;;; Code:

(require 'cl-lib)

(defvar ffi-types
  '(:uint8 :uint16 :uint32 :unit64
    :sint8 :sint16 :sint32 :sint64
    :float :double :pointer :void)
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
  (put :pointer 'ffi-code "p")
  (put :void    'ffi-code "V"))

(defvar ffi-data-root (file-name-directory load-file-name)
  "Location of FFI data files")

(defvar ffi-context nil
  "Context for FFI calls.")

(cl-defstruct (ffi (:constructor ffi--create))
  (process nil)
  (input (generate-new-buffer " *ffi-input*"))
  (cifs (make-hash-table :test 'equal))
  (libs (make-hash-table :test 'equal))
  (syms (make-hash-table :test 'equal)))

(cl-defun ffi-create (&optional (name "ffi"))
  "Create and return a new FFI context."
  (let* ((process-connection-type nil)  ; use a pipe
         (buffer (generate-new-buffer (format "*%s*" name)))
         (exec (expand-file-name "ffi-glue" ffi-data-root))
         (process (start-process name buffer exec))
         (ffi (ffi--create :process process)))
    (prog1 ffi
      (setf (process-sentinel process)
            (lambda (_proc _status)
              (kill-buffer buffer)
              (kill-buffer (ffi-input ffi)))))))

(defun ffi-destroy (ffi)
  "Destroy an FFI context."
  (unless (null ffi)
    (let ((process (ffi-process ffi)))
      (unwind-protect
          (when (process-live-p process)
            (ffi-flush ffi))
        (kill-process process)))))

(defun ffi-ensure ()
  "Ensure that `ffi-context' is initialized."
  (when (or (null ffi-context) (not (process-live-p (ffi-process ffi-context))))
    (setf ffi-context (ffi-create))))

(defun ffi-write (ffi &rest strings)
  "Write STRINGS to FFI."
  (with-current-buffer (ffi-input ffi)
    (apply #'insert strings)))

(defun ffi-flush (ffi)
  "Send all prepared FFI input to the subprocess."
  (with-current-buffer (ffi-input ffi)
    (unless (zerop (buffer-size))
      (process-send-region (ffi-process ffi) (point-min) (point-max))
      (erase-buffer))))

(defun ffi-read (ffi)
  "Blocking read a value from FFI."
  (ffi-flush ffi)
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
        ((eq type :void) (ffi-write ffi "V"))
        ((stringp value) (ffi-write ffi (format "w%dM%s" (length value) value)))
        ((ffi-write ffi (get type 'ffi-code) (prin1-to-string value) " "))))

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
See `ffi-call' docstring for the signature specification."
  (or (gethash signature (ffi-cifs ffi))
      (with-temp-buffer
        (let ((standard-output (current-buffer)))
          (cl-loop for i from (1- (length signature)) downto 0
                   for arg = (aref signature i)
                   do (princ (get arg 'ffi-code))
                   do (unless (eq arg :void) (princ "0")))
          (princ "w")
          (princ (1- (length signature)))
          (princ "C")
          (ffi-write ffi (buffer-string))
          (setf (gethash signature (ffi-cifs ffi)) (ffi-pop ffi))))))

(defun ffi-dlopen (ffi name)
  "Fetch a handle (pointer) by library NAME."
  (or (gethash name (ffi-libs ffi))
      (progn
        (ffi-push ffi :pointer name)
        (ffi-write ffi "O")
        (setf (gethash name (ffi-libs ffi)) (ffi-pop ffi)))))

(defun ffi-dlsym (ffi library name)
  "Fetch a handle for NAME from LIRBARY in FFI."
  (or (gethash name (ffi-syms ffi))
      (progn
        (ffi-push ffi :pointer (or library 0))
        (ffi-push ffi :pointer name)
        (ffi-write ffi "S")
        (setf (gethash name (ffi-syms ffi)) (ffi-pop ffi)))))

(defun ffi-call (library symbol signature &rest args)
  "Call SYMBOL from LIBRARY with ARGS using SIGNATURE.
The signature must be a vector of type designators (:unit8,
:double, etc.). The required first element is the return type and
the rest are the argument types."
  (ffi-ensure)
  (let* ((cif (ffi-cif ffi-context signature))
         (ptr-library (if library (ffi-dlopen ffi-context library) nil))
         (ptr-symbol  (ffi-dlsym ffi-context ptr-library symbol)))
    (cl-loop for arg in (reverse args)
             for i from (1- (length signature)) downto 1
             for type = (aref signature i)
             do (ffi-push ffi-context type arg))
    (ffi-push ffi-context :pointer cif)
    (ffi-push ffi-context :pointer ptr-symbol)
    (ffi-write ffi-context "c")
    (ffi-pop ffi-context)))

(provide 'ffi)

;;; ffi.el ends here
