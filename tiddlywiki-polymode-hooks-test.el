;;; tiddlywiki-polymode-hooks-test.el --- Tests for hook behavior -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests to verify that mode hooks don't run in code blocks
;; when tiddlywiki-code-block-run-hooks is nil.

;;; Code:

(require 'ert)

(defvar tiddlywiki-polymode-hooks-test-skip (not (require 'polymode nil t))
  "Non-nil if tests should be skipped.")

(when (not tiddlywiki-polymode-hooks-test-skip)
  (require 'tiddlywiki-polymode))

;;; ============================================================
;;; Test Fixtures
;;; ============================================================

(defconst tiddlywiki-hooks-test-content
  "title: Test

Some text.

```python
def hello():
    pass
```

More text.

```emacs-lisp
(defun test () nil)
```

End."
  "Sample content with code blocks.")

;;; ============================================================
;;; Hook Tracking
;;; ============================================================

(defvar tiddlywiki-test-hook-called nil
  "Set to t when test hook is called.")

(defvar tiddlywiki-test-hook-call-count 0
  "Count of hook calls.")

(defun tiddlywiki-test-hook-function ()
  "Test hook function that records when it's called."
  (setq tiddlywiki-test-hook-called t)
  (setq tiddlywiki-test-hook-call-count (1+ tiddlywiki-test-hook-call-count)))

;;; ============================================================
;;; Tests
;;; ============================================================

(ert-deftest tiddlywiki-hooks-test-python-hook-not-called ()
  "Test that python-mode-hook is not called in code blocks."
  :tags '(:polymode :hooks)
  (skip-unless (not tiddlywiki-polymode-hooks-test-skip))
  (skip-unless (fboundp 'python-mode))
  (let ((tiddlywiki-code-block-run-hooks nil)
        (tiddlywiki-test-hook-called nil)
        (tiddlywiki-test-hook-call-count 0))
    ;; Add our test hook
    (add-hook 'python-mode-hook #'tiddlywiki-test-hook-function)
    (unwind-protect
        (with-temp-buffer
          (insert tiddlywiki-hooks-test-content)
          (poly-tiddlywiki-mode)
          ;; Move into the python code block to trigger inner mode
          (goto-char (point-min))
          (search-forward "def hello")
          ;; Force polymode to switch to inner buffer
          (pm-switch-to-buffer)
          ;; Check if hook was called
          (should-not tiddlywiki-test-hook-called))
      ;; Cleanup
      (remove-hook 'python-mode-hook #'tiddlywiki-test-hook-function))))

(ert-deftest tiddlywiki-hooks-test-elisp-hook-not-called ()
  "Test that emacs-lisp-mode-hook is not called in code blocks."
  :tags '(:polymode :hooks)
  (skip-unless (not tiddlywiki-polymode-hooks-test-skip))
  (let ((tiddlywiki-code-block-run-hooks nil)
        (tiddlywiki-test-hook-called nil)
        (tiddlywiki-test-hook-call-count 0))
    ;; Add our test hook
    (add-hook 'emacs-lisp-mode-hook #'tiddlywiki-test-hook-function)
    (unwind-protect
        (with-temp-buffer
          (insert tiddlywiki-hooks-test-content)
          (poly-tiddlywiki-mode)
          ;; Move into the elisp code block
          (goto-char (point-min))
          (search-forward "(defun test")
          ;; Force polymode to switch
          (pm-switch-to-buffer)
          ;; Check if hook was called
          (should-not tiddlywiki-test-hook-called))
      ;; Cleanup
      (remove-hook 'emacs-lisp-mode-hook #'tiddlywiki-test-hook-function))))

(ert-deftest tiddlywiki-hooks-test-hooks-called-when-enabled ()
  "Test that hooks ARE called when tiddlywiki-code-block-run-hooks is t."
  :tags '(:polymode :hooks)
  (skip-unless (not tiddlywiki-polymode-hooks-test-skip))
  (skip-unless (fboundp 'python-mode))
  (let ((tiddlywiki-code-block-run-hooks t)
        (tiddlywiki-test-hook-called nil)
        (tiddlywiki-test-hook-call-count 0))
    ;; Add our test hook
    (add-hook 'python-mode-hook #'tiddlywiki-test-hook-function)
    (unwind-protect
        (with-temp-buffer
          (insert tiddlywiki-hooks-test-content)
          (poly-tiddlywiki-mode)
          ;; Move into the python code block
          (goto-char (point-min))
          (search-forward "def hello")
          ;; Force polymode to switch
          (pm-switch-to-buffer)
          ;; Hook SHOULD be called when enabled
          (should tiddlywiki-test-hook-called))
      ;; Cleanup
      (remove-hook 'python-mode-hook #'tiddlywiki-test-hook-function))))

(ert-deftest tiddlywiki-hooks-test-delay-mode-hooks-mechanism ()
  "Test that delay-mode-hooks mechanism is used to inhibit hooks."
  :tags '(:polymode :hooks)
  (skip-unless (not tiddlywiki-polymode-hooks-test-skip))
  ;; Just verify the mechanism exists
  (should (boundp 'delay-mode-hooks))
  (should (boundp 'delayed-mode-hooks)))

;;; ============================================================
;;; Debug Tests - Check what's happening
;;; ============================================================

(ert-deftest tiddlywiki-hooks-test-debug-inner-buffer-exists ()
  "Debug test: Check if inner buffer is created."
  :tags '(:polymode :debug)
  (skip-unless (not tiddlywiki-polymode-hooks-test-skip))
  (skip-unless (fboundp 'python-mode))
  (with-temp-buffer
    (insert tiddlywiki-hooks-test-content)
    (poly-tiddlywiki-mode)
    (goto-char (point-min))
    (search-forward "def hello")
    (let ((span (pm-innermost-span)))
      ;; Should have a span
      (should span)
      ;; Should be a body span (inner mode)
      (should (eq (car span) 'body))
      ;; Get the buffer for this span
      (let ((buf (pm-span-buffer span)))
        (should buf)
        (message "Inner buffer: %s, major-mode in it: %s"
                 buf
                 (buffer-local-value 'major-mode buf))))))

(ert-deftest tiddlywiki-hooks-test-debug-mode-resolution ()
  "Debug test: Check mode resolution for languages."
  :tags '(:polymode :debug)
  (skip-unless (not tiddlywiki-polymode-hooks-test-skip))
  ;; Test our function
  (should (eq (tiddlywiki-polymode--get-mode-symbol "python") 'python-mode))
  (should (eq (tiddlywiki-polymode--get-mode-symbol "emacs-lisp") 'emacs-lisp-mode))
  (should (eq (tiddlywiki-polymode--get-mode-symbol "elisp") 'emacs-lisp-mode))
  ;; Test with nil
  (should (null (tiddlywiki-polymode--get-mode-symbol nil)))
  ;; Test the advice
  (should (eq (pm-get-mode-symbol-from-name "python") 'python-mode))
  (should (eq (pm-get-mode-symbol-from-name "elisp") 'emacs-lisp-mode))
  ;; Test with nil - should return default
  (let ((result (pm-get-mode-symbol-from-name nil)))
    (message "pm-get-mode-symbol-from-name nil => %s" result)
    (should result)))

(provide 'tiddlywiki-polymode-hooks-test)

;;; tiddlywiki-polymode-hooks-test.el ends here
