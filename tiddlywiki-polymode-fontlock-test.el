;;; tiddlywiki-polymode-fontlock-test.el --- Tests for syntax highlighting -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests to verify that syntax highlighting works correctly in code blocks.

;;; Code:

(require 'ert)

(defvar tiddlywiki-fontlock-test-skip (not (require 'polymode nil t))
  "Non-nil if tests should be skipped.")

(when (not tiddlywiki-fontlock-test-skip)
  (require 'tiddlywiki-polymode))

;;; ============================================================
;;; Test Fixtures
;;; ============================================================

(defconst tiddlywiki-fontlock-test-python-content
  "title: Test

```python
def hello():
    return \"world\"
```
"
  "Sample content with Python code block.")

(defconst tiddlywiki-fontlock-test-elisp-content
  "title: Test

```elisp
(defun hello ()
  \"Docstring.\"
  (message \"world\"))
```
"
  "Sample content with Elisp code block.")

;;; ============================================================
;;; Helper Functions
;;; ============================================================

(defun tiddlywiki-fontlock-test--get-face-at (pos)
  "Get the face at POS in current buffer."
  (get-text-property pos 'face))

(defun tiddlywiki-fontlock-test--face-at-text (text)
  "Find TEXT in buffer and return the face at its start position."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward text nil t)
      (tiddlywiki-fontlock-test--get-face-at (match-beginning 0)))))

(defun tiddlywiki-fontlock-test--has-face-p (pos face)
  "Return non-nil if POS has FACE (or FACE is in the list of faces)."
  (let ((face-at-pos (get-text-property pos 'face)))
    (cond
     ((null face-at-pos) nil)
     ((symbolp face-at-pos) (eq face-at-pos face))
     ((listp face-at-pos) (memq face face-at-pos))
     (t nil))))

(defun tiddlywiki-fontlock-test--text-has-face-p (text face)
  "Return non-nil if TEXT in buffer has FACE."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward text nil t)
      (tiddlywiki-fontlock-test--has-face-p (match-beginning 0) face))))

;;; ============================================================
;;; Debug Tests
;;; ============================================================

(ert-deftest tiddlywiki-fontlock-test-debug-buffer-setup ()
  "Debug test: Check buffer setup and fontification."
  :tags '(:polymode :fontlock :debug)
  (skip-unless (not tiddlywiki-fontlock-test-skip))
  (skip-unless (fboundp 'python-mode))
  (with-temp-buffer
    (insert tiddlywiki-fontlock-test-python-content)
    (poly-tiddlywiki-mode)
    ;; Move into the python code block
    (goto-char (point-min))
    (search-forward "def hello")
    (let ((span (pm-innermost-span)))
      (message "Span: %S" span)
      (message "Major mode: %s" major-mode)
      (message "Buffer: %s" (current-buffer))
      ;; Check we're in an inner span
      (should span)
      (should (car span)))))

(ert-deftest tiddlywiki-fontlock-test-debug-faces ()
  "Debug test: Show what faces are applied."
  :tags '(:polymode :fontlock :debug)
  (skip-unless (not tiddlywiki-fontlock-test-skip))
  (skip-unless (fboundp 'python-mode))
  (with-temp-buffer
    (insert tiddlywiki-fontlock-test-python-content)
    (poly-tiddlywiki-mode)
    ;; Force fontification
    (font-lock-ensure)
    ;; Move into the python code block and check faces
    (goto-char (point-min))
    (search-forward "def")
    (let* ((def-pos (match-beginning 0))
           (def-face (get-text-property def-pos 'face)))
      (message "Position of 'def': %d" def-pos)
      (message "Face at 'def': %S" def-face)
      (message "All text properties at 'def': %S"
               (text-properties-at def-pos)))
    ;; Check "hello" function name
    (goto-char (point-min))
    (search-forward "hello")
    (let* ((hello-pos (match-beginning 0))
           (hello-face (get-text-property hello-pos 'face)))
      (message "Position of 'hello': %d" hello-pos)
      (message "Face at 'hello': %S" hello-face))
    ;; Check string
    (goto-char (point-min))
    (search-forward "\"world\"")
    (let* ((string-pos (match-beginning 0))
           (string-face (get-text-property string-pos 'face)))
      (message "Position of string: %d" string-pos)
      (message "Face at string: %S" string-face))))

(ert-deftest tiddlywiki-fontlock-test-debug-inner-buffer-fontlock ()
  "Debug test: Check fontification in inner buffer directly."
  :tags '(:polymode :fontlock :debug)
  (skip-unless (not tiddlywiki-fontlock-test-skip))
  (skip-unless (fboundp 'python-mode))
  (with-temp-buffer
    (insert tiddlywiki-fontlock-test-python-content)
    (poly-tiddlywiki-mode)
    ;; Move into the python code block
    (goto-char (point-min))
    (search-forward "def hello")
    ;; Get the inner buffer
    (let* ((span (pm-innermost-span))
           (inner-buf (when span (pm-span-buffer span))))
      (message "Inner buffer: %S" inner-buf)
      (when inner-buf
        (with-current-buffer inner-buf
          (message "Inner buffer major-mode: %s" major-mode)
          (message "font-lock-mode: %s" font-lock-mode)
          (message "font-lock-keywords: %S" (and (boundp 'font-lock-keywords)
                                                  (length font-lock-keywords)))
          ;; Force fontification in inner buffer
          (font-lock-ensure)
          ;; Check faces
          (goto-char (point-min))
          (when (search-forward "def" nil t)
            (message "Face at 'def' in inner buffer: %S"
                     (get-text-property (match-beginning 0) 'face))))))))

;;; ============================================================
;;; Actual Tests
;;; ============================================================

(ert-deftest tiddlywiki-fontlock-test-python-keyword ()
  "Test that Python keywords are fontified."
  :tags '(:polymode :fontlock)
  (skip-unless (not tiddlywiki-fontlock-test-skip))
  (skip-unless (fboundp 'python-mode))
  (with-temp-buffer
    (insert tiddlywiki-fontlock-test-python-content)
    (poly-tiddlywiki-mode)
    (font-lock-ensure)
    ;; Move into code block and get inner buffer
    (goto-char (point-min))
    (search-forward "def hello")
    (let* ((span (pm-innermost-span))
           (inner-buf (when span (pm-span-buffer span))))
      (should inner-buf)
      (with-current-buffer inner-buf
        (font-lock-ensure)
        (goto-char (point-min))
        (should (search-forward "def" nil t))
        (let ((face (get-text-property (match-beginning 0) 'face)))
          ;; 'def' should have font-lock-keyword-face
          (should face)
          (message "Face for 'def': %S" face))))))

(ert-deftest tiddlywiki-fontlock-test-python-string ()
  "Test that Python strings are fontified."
  :tags '(:polymode :fontlock)
  (skip-unless (not tiddlywiki-fontlock-test-skip))
  (skip-unless (fboundp 'python-mode))
  (with-temp-buffer
    (insert tiddlywiki-fontlock-test-python-content)
    (poly-tiddlywiki-mode)
    (font-lock-ensure)
    ;; Move into code block and get inner buffer
    (goto-char (point-min))
    (search-forward "\"world\"")
    (let* ((span (pm-innermost-span))
           (inner-buf (when span (pm-span-buffer span))))
      (should inner-buf)
      (with-current-buffer inner-buf
        (font-lock-ensure)
        (goto-char (point-min))
        (should (search-forward "\"world\"" nil t))
        (let ((face (get-text-property (match-beginning 0) 'face)))
          ;; String should have font-lock-string-face
          (should face)
          (message "Face for string: %S" face))))))

(ert-deftest tiddlywiki-fontlock-test-elisp-keyword ()
  "Test that Elisp keywords are fontified."
  :tags '(:polymode :fontlock)
  (skip-unless (not tiddlywiki-fontlock-test-skip))
  (with-temp-buffer
    (insert tiddlywiki-fontlock-test-elisp-content)
    (poly-tiddlywiki-mode)
    (font-lock-ensure)
    ;; Move into code block and get inner buffer
    (goto-char (point-min))
    (search-forward "defun")
    (let* ((span (pm-innermost-span))
           (inner-buf (when span (pm-span-buffer span))))
      (should inner-buf)
      (with-current-buffer inner-buf
        (font-lock-ensure)
        (goto-char (point-min))
        (should (search-forward "defun" nil t))
        (let ((face (get-text-property (match-beginning 0) 'face)))
          ;; 'defun' should have a face
          (should face)
          (message "Face for 'defun': %S" face))))))

(provide 'tiddlywiki-polymode-fontlock-test)

;;; tiddlywiki-polymode-fontlock-test.el ends here
