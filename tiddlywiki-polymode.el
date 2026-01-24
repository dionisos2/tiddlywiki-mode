;;; tiddlywiki-polymode.el --- Polymode support for TiddlyWiki -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Your Name
;; Keywords: languages, tiddlywiki, polymode

;;; Commentary:

;; This file provides polymode support for TiddlyWiki mode, enabling
;; proper syntax highlighting and indentation for code blocks.
;;
;; To use, add to your config:
;;   (require 'tiddlywiki-polymode)
;;
;; Then use `poly-tiddlywiki-mode' instead of `tiddlywiki-mode'.
;; You can set this automatically:
;;   (add-to-list 'auto-mode-alist '("\\.tid\\'" . poly-tiddlywiki-mode))

;;; Code:

(require 'polymode)
(require 'tiddlywiki-mode)

;;; ============================================================
;;; Configuration
;;; ============================================================

(defcustom tiddlywiki-code-block-run-hooks nil
  "If non-nil, run mode hooks in code blocks.
When nil (the default), code blocks use lightweight initialization
that provides syntax highlighting and indentation without running
mode hooks. This prevents heavy operations like LSP from starting
for each code block.

Set to non-nil if you want full mode functionality in code blocks."
  :type 'boolean
  :group 'tiddlywiki)

(defcustom tiddlywiki-code-block-default-mode 'prog-mode
  "Default mode for code blocks without a specified language."
  :type 'symbol
  :group 'tiddlywiki)

;;; ============================================================
;;; Language to Mode Mapping
;;; ============================================================

(defcustom tiddlywiki-language-mode-alist
  '(("elisp" . emacs-lisp-mode)
    ("emacs-lisp" . emacs-lisp-mode)
    ("python" . python-mode)
    ("julia" . julia-mode)
    ("javascript" . js-mode)
    ("js" . js-mode)
    ("typescript" . typescript-mode)
    ("ts" . typescript-mode)
    ("ruby" . ruby-mode)
    ("rust" . rust-mode)
    ("go" . go-mode)
    ("c" . c-mode)
    ("cpp" . c++-mode)
    ("c++" . c++-mode)
    ("java" . java-mode)
    ("html" . html-mode)
    ("css" . css-mode)
    ("json" . json-mode)
    ("yaml" . yaml-mode)
    ("yml" . yaml-mode)
    ("xml" . xml-mode)
    ("sql" . sql-mode)
    ("sh" . sh-mode)
    ("bash" . sh-mode)
    ("shell" . sh-mode)
    ("lisp" . lisp-mode)
    ("scheme" . scheme-mode)
    ("clojure" . clojure-mode)
    ("clj" . clojure-mode)
    ("haskell" . haskell-mode)
    ("hs" . haskell-mode)
    ("lua" . lua-mode)
    ("perl" . perl-mode)
    ("php" . php-mode)
    ("r" . r-mode)
    ("scala" . scala-mode)
    ("swift" . swift-mode)
    ("kotlin" . kotlin-mode)
    ("markdown" . markdown-mode)
    ("md" . markdown-mode)
    ("org" . org-mode)
    ("latex" . latex-mode)
    ("tex" . tex-mode))
  "Alist mapping language names to Emacs major modes.
Each entry is (LANGUAGE . MODE) where LANGUAGE is the string used
after ``` in code blocks and MODE is the corresponding major mode."
  :type '(alist :key-type string :value-type symbol)
  :group 'tiddlywiki)

;;; ============================================================
;;; Polymode Initialization Functions
;;; ============================================================

(defun tiddlywiki-polymode--init-code-block (&optional _type)
  "Initialize a code block without running mode hooks.
TYPE is the span type (passed by polymode, ignored here).
This function sets up syntax highlighting and indentation
without triggering heavy operations like LSP."
  (setq-local delay-mode-hooks (not tiddlywiki-code-block-run-hooks))
  ;; Font-lock is set up by the mode itself
  (font-lock-mode 1)
  ;; Ensure proper indentation function is available
  (when (local-variable-p 'indent-line-function)
    (setq-local indent-line-function indent-line-function)))

(defun tiddlywiki-polymode--get-mode-symbol (name)
  "Get mode symbol for language NAME using `tiddlywiki-language-mode-alist'."
  (or (cdr (assoc (downcase name) tiddlywiki-language-mode-alist))
      (let ((mode-sym (intern (concat (downcase name) "-mode"))))
        (if (fboundp mode-sym)
            mode-sym
          nil))))

;;; ============================================================
;;; Polymode Definitions
;;; ============================================================

(define-hostmode poly-tiddlywiki-hostmode
  :mode 'tiddlywiki-mode)

(define-auto-innermode poly-tiddlywiki-code-innermode
  :head-matcher "^```\\([a-zA-Z0-9_+-]+\\)?$"
  :tail-matcher "^```$"
  :mode-matcher (cons "^```\\([a-zA-Z0-9_+-]+\\)$" 1)
  :head-mode 'host
  :tail-mode 'host
  :fallback-mode 'prog-mode
  :init-functions '(tiddlywiki-polymode--init-code-block))

;; Advice to use our custom language-to-mode mapping
(defun tiddlywiki-polymode--around-get-mode-symbol (orig-fun name &optional fallback)
  "Advice for `pm-get-mode-symbol-from-name' to use our language alist.
ORIG-FUN is the original function, NAME is the language name,
FALLBACK is the optional fallback mode."
  (or (tiddlywiki-polymode--get-mode-symbol name)
      (funcall orig-fun name fallback)))

(advice-add 'pm-get-mode-symbol-from-name
            :around #'tiddlywiki-polymode--around-get-mode-symbol)

(define-polymode poly-tiddlywiki-mode
  :hostmode 'poly-tiddlywiki-hostmode
  :innermodes '(poly-tiddlywiki-code-innermode)
  (setq-local polymode-run-mode-hooks tiddlywiki-code-block-run-hooks))

(provide 'tiddlywiki-polymode)

;;; tiddlywiki-polymode.el ends here
