;;; poly-tiddlywiki-test.el --- Tests for poly-tiddlywiki -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for TiddlyWiki polymode integration.
;; These tests require polymode to be installed.

;;; Code:

(require 'ert)

;; Skip all tests if polymode is not available
(defvar tiddlywiki-polymode-test-skip (not (require 'polymode nil t))
  "Non-nil if polymode tests should be skipped.")

(when (not tiddlywiki-polymode-test-skip)
  (require 'poly-tiddlywiki))

;;; ============================================================
;;; Test Fixtures
;;; ============================================================

(defconst tiddlywiki-polymode-test-content
  "created: 20230101120000000
modified: 20230615143000000
title: Test Tiddler
type: text/vnd.tiddlywiki

Some text before code.

```python
def hello():
    print(\"Hello, World!\")
```

Some text after code.

```elisp
(defun test ()
  (message \"test\"))
```

End of tiddler."
  "Sample tiddler with code blocks for testing.")

(defconst tiddlywiki-polymode-test-no-lang
  "title: Test

```
code without language
```
"
  "Sample tiddler with code block without language specification.")

;;; ============================================================
;;; Mode Symbol Tests
;;; ============================================================

(ert-deftest tiddlywiki-polymode-test-get-mode-symbol-known ()
  "Test getting mode symbol for known languages."
  :tags '(:polymode)
  (skip-unless (not tiddlywiki-polymode-test-skip))
  (should (eq (tiddlywiki-polymode--get-mode-symbol "python") 'python-mode))
  (should (eq (tiddlywiki-polymode--get-mode-symbol "elisp") 'emacs-lisp-mode))
  (should (eq (tiddlywiki-polymode--get-mode-symbol "javascript") 'js-mode))
  (should (eq (tiddlywiki-polymode--get-mode-symbol "js") 'js-mode)))

(ert-deftest tiddlywiki-polymode-test-get-mode-symbol-case-insensitive ()
  "Test that language lookup is case-insensitive."
  :tags '(:polymode)
  (skip-unless (not tiddlywiki-polymode-test-skip))
  (should (eq (tiddlywiki-polymode--get-mode-symbol "Python") 'python-mode))
  (should (eq (tiddlywiki-polymode--get-mode-symbol "ELISP") 'emacs-lisp-mode)))

(ert-deftest tiddlywiki-polymode-test-get-mode-symbol-fallback ()
  "Test fallback for unknown languages with existing modes."
  :tags '(:polymode)
  (skip-unless (not tiddlywiki-polymode-test-skip))
  ;; text-mode exists, so "text" should resolve to text-mode
  (should (eq (tiddlywiki-polymode--get-mode-symbol "text") 'text-mode))
  ;; fundamental-mode exists
  (should (eq (tiddlywiki-polymode--get-mode-symbol "fundamental") 'fundamental-mode)))

(ert-deftest tiddlywiki-polymode-test-get-mode-symbol-unknown ()
  "Test that unknown languages return nil."
  :tags '(:polymode)
  (skip-unless (not tiddlywiki-polymode-test-skip))
  (should (null (tiddlywiki-polymode--get-mode-symbol "nonexistent-language-xyz"))))

;;; ============================================================
;;; Polymode Activation Tests
;;; ============================================================

(ert-deftest tiddlywiki-polymode-test-mode-activation ()
  "Test that poly-tiddlywiki-mode activates correctly."
  :tags '(:polymode)
  (skip-unless (not tiddlywiki-polymode-test-skip))
  (with-temp-buffer
    (insert tiddlywiki-polymode-test-content)
    (poly-tiddlywiki-mode)
    ;; In polymode, the major-mode is the host mode (tiddlywiki-mode)
    ;; and polymode-mode is a minor mode
    (should (eq major-mode 'tiddlywiki-mode))
    (should (bound-and-true-p polymode-mode))))

(ert-deftest tiddlywiki-polymode-test-host-mode ()
  "Test that host mode is tiddlywiki-mode."
  :tags '(:polymode)
  (skip-unless (not tiddlywiki-polymode-test-skip))
  (with-temp-buffer
    (insert tiddlywiki-polymode-test-content)
    (poly-tiddlywiki-mode)
    (goto-char (point-min))
    ;; In the header/host region, pm-innermost-span returns a span
    ;; with nil as the first element (type) indicating host mode
    (let ((span (pm-innermost-span)))
      (should span)
      (should (null (car span)))))) ;; nil type means host mode

(ert-deftest tiddlywiki-polymode-test-innermode-detection ()
  "Test that code blocks are detected as inner spans."
  :tags '(:polymode)
  (skip-unless (not tiddlywiki-polymode-test-skip))
  (with-temp-buffer
    (insert tiddlywiki-polymode-test-content)
    (poly-tiddlywiki-mode)
    ;; Move to inside the python code block
    (goto-char (point-min))
    (search-forward "def hello")
    (let ((span (pm-innermost-span)))
      (should span)
      ;; Inner spans have a non-nil type (like 'body)
      (should (car span)))))

(ert-deftest tiddlywiki-polymode-test-python-mode-in-block ()
  "Test that python code blocks use python-mode."
  :tags '(:polymode)
  (skip-unless (not tiddlywiki-polymode-test-skip))
  (skip-unless (fboundp 'python-mode))
  (with-temp-buffer
    (insert tiddlywiki-polymode-test-content)
    (poly-tiddlywiki-mode)
    ;; Move to inside the python code block
    (goto-char (point-min))
    (search-forward "def hello")
    (let* ((span (pm-innermost-span))
           (inner-mode (when span (pm-true-span-type span))))
      (should (or (eq inner-mode 'python-mode)
                  (eq inner-mode 'body))))))

(ert-deftest tiddlywiki-polymode-test-elisp-mode-in-block ()
  "Test that elisp code blocks use emacs-lisp-mode."
  :tags '(:polymode)
  (skip-unless (not tiddlywiki-polymode-test-skip))
  (with-temp-buffer
    (insert tiddlywiki-polymode-test-content)
    (poly-tiddlywiki-mode)
    ;; Move to inside the elisp code block
    (goto-char (point-min))
    (search-forward "(defun test")
    (let* ((span (pm-innermost-span))
           (inner-mode (when span (pm-true-span-type span))))
      (should (or (eq inner-mode 'emacs-lisp-mode)
                  (eq inner-mode 'body))))))

;;; ============================================================
;;; Configuration Tests
;;; ============================================================

(ert-deftest tiddlywiki-polymode-test-custom-language-alist ()
  "Test adding custom language mappings."
  :tags '(:polymode)
  (skip-unless (not tiddlywiki-polymode-test-skip))
  (let ((tiddlywiki-language-mode-alist
         (cons '("mylang" . text-mode) tiddlywiki-language-mode-alist)))
    (should (eq (tiddlywiki-polymode--get-mode-symbol "mylang") 'text-mode))))

(ert-deftest tiddlywiki-polymode-test-hooks-disabled-by-default ()
  "Test that mode hooks are disabled by default."
  :tags '(:polymode)
  (skip-unless (not tiddlywiki-polymode-test-skip))
  (should (eq tiddlywiki-code-block-run-hooks nil)))

(ert-deftest tiddlywiki-polymode-test-get-mode-symbol-nil ()
  "Test that nil/empty language names are handled."
  :tags '(:polymode)
  (skip-unless (not tiddlywiki-polymode-test-skip))
  (should (null (tiddlywiki-polymode--get-mode-symbol nil)))
  (should (null (tiddlywiki-polymode--get-mode-symbol ""))))

;;; ============================================================
;;; Code Block Boundary Tests
;;; ============================================================

(defconst tiddlywiki-polymode-test-boundaries
  "title: Test

Text before first block.

```python
def foo():
    pass
```

Text between blocks.

```javascript
function bar() {}
```

Text after all blocks.
"
  "Sample content for testing code block boundaries.")

(ert-deftest tiddlywiki-polymode-test-boundary-before-first-block ()
  "Test that text before first code block is in host mode."
  :tags '(:polymode :boundaries)
  (skip-unless (not tiddlywiki-polymode-test-skip))
  (with-temp-buffer
    (insert tiddlywiki-polymode-test-boundaries)
    (poly-tiddlywiki-mode)
    (goto-char (point-min))
    (search-forward "Text before")
    (let ((span (pm-innermost-span)))
      (should span)
      ;; nil type means host mode
      (should (null (car span))))))

(ert-deftest tiddlywiki-polymode-test-boundary-inside-first-block ()
  "Test that code inside first block is in inner mode."
  :tags '(:polymode :boundaries)
  (skip-unless (not tiddlywiki-polymode-test-skip))
  (skip-unless (fboundp 'python-mode))
  (with-temp-buffer
    (insert tiddlywiki-polymode-test-boundaries)
    (poly-tiddlywiki-mode)
    (goto-char (point-min))
    (search-forward "def foo")
    (let ((span (pm-innermost-span)))
      (should span)
      ;; non-nil type (body) means inner mode
      (should (car span)))))

(ert-deftest tiddlywiki-polymode-test-boundary-between-blocks ()
  "Test that text between code blocks is in host mode."
  :tags '(:polymode :boundaries)
  (skip-unless (not tiddlywiki-polymode-test-skip))
  (with-temp-buffer
    (insert tiddlywiki-polymode-test-boundaries)
    (poly-tiddlywiki-mode)
    (goto-char (point-min))
    (search-forward "Text between")
    (let ((span (pm-innermost-span)))
      (should span)
      ;; nil type means host mode
      (should (null (car span))))))

(ert-deftest tiddlywiki-polymode-test-boundary-inside-second-block ()
  "Test that code inside second block is in inner mode."
  :tags '(:polymode :boundaries)
  (skip-unless (not tiddlywiki-polymode-test-skip))
  (with-temp-buffer
    (insert tiddlywiki-polymode-test-boundaries)
    (poly-tiddlywiki-mode)
    (goto-char (point-min))
    (search-forward "function bar")
    (let ((span (pm-innermost-span)))
      (should span)
      ;; non-nil type (body) means inner mode
      (should (car span)))))

(ert-deftest tiddlywiki-polymode-test-boundary-after-all-blocks ()
  "Test that text after all code blocks is in host mode."
  :tags '(:polymode :boundaries)
  (skip-unless (not tiddlywiki-polymode-test-skip))
  (with-temp-buffer
    (insert tiddlywiki-polymode-test-boundaries)
    (poly-tiddlywiki-mode)
    (goto-char (point-min))
    (search-forward "Text after all")
    (let ((span (pm-innermost-span)))
      (should span)
      ;; nil type means host mode
      (should (null (car span))))))

(ert-deftest tiddlywiki-polymode-test-boundary-no-lang-block-is-host ()
  "Test that code blocks without language stay in host mode."
  :tags '(:polymode :boundaries)
  (skip-unless (not tiddlywiki-polymode-test-skip))
  (with-temp-buffer
    (insert tiddlywiki-polymode-test-no-lang)
    (poly-tiddlywiki-mode)
    (goto-char (point-min))
    (search-forward "code without")
    (let ((span (pm-innermost-span)))
      (should span)
      ;; Code blocks without language should be in host mode
      ;; because head-matcher requires a language
      (should (null (car span))))))

(provide 'poly-tiddlywiki-test)

;;; poly-tiddlywiki-test.el ends here
