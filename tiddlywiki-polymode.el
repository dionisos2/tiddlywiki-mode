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
;;; Language Resolution
;;; ============================================================

(defun tiddlywiki-polymode--get-mode-symbol (name)
  "Get mode symbol for language NAME using `tiddlywiki-language-mode-alist'.
Returns nil if no mode is found."
  (when (and name (stringp name) (> (length name) 0))
    (or (cdr (assoc (downcase name) tiddlywiki-language-mode-alist))
        (let ((mode-sym (intern (concat (downcase name) "-mode"))))
          (when (fboundp mode-sym)
            mode-sym)))))

;; Advice to use our custom language-to-mode mapping
(defun tiddlywiki-polymode--around-get-mode-symbol (orig-fun name &optional fallback)
  "Advice for `pm-get-mode-symbol-from-name' to use our language alist.
ORIG-FUN is the original function, NAME is the language name,
FALLBACK is the optional fallback mode."
  (if (and name (stringp name) (> (length name) 0))
      (or (tiddlywiki-polymode--get-mode-symbol name)
          (funcall orig-fun name fallback))
    ;; If name is nil or empty, return fallback or default
    (or fallback tiddlywiki-code-block-default-mode)))

(advice-add 'pm-get-mode-symbol-from-name
            :around #'tiddlywiki-polymode--around-get-mode-symbol)

;;; ============================================================
;;; Hook Management - Disable mode hooks in inner buffers
;;; ============================================================

;; We use Emacs's built-in delay-mode-hooks mechanism to prevent
;; mode hooks from running in polymode inner buffers.
;; When delay-mode-hooks is t, run-mode-hooks stores hooks in
;; delayed-mode-hooks instead of running them. We then clear
;; delayed-mode-hooks to discard them.

(defvar tiddlywiki-polymode--inhibit-hooks nil
  "When non-nil, mode hooks should be inhibited.")

(defun tiddlywiki-polymode--is-tiddlywiki-inner-buffer-p ()
  "Return non-nil if this is a tiddlywiki polymode inner buffer."
  (and (buffer-base-buffer)  ; We're in an indirect buffer
       (boundp 'pm/polymode)
       pm/polymode
       (let ((hostmode (ignore-errors (eieio-oref pm/polymode 'hostmode))))
         (and hostmode
              (eq (ignore-errors (eieio-oref hostmode 'mode))
                  'tiddlywiki-mode)))))

(defun tiddlywiki-polymode--around-pm-mode-setup (orig-fun mode &optional buffer)
  "Advice around `pm--mode-setup' to inhibit hooks for tiddlywiki inner buffers.
ORIG-FUN is the original function, MODE is the mode to setup, BUFFER is optional."
  (if (and (not tiddlywiki-code-block-run-hooks)
           ;; Check if we're setting up an inner buffer (indirect buffer)
           (buffer-base-buffer (or buffer (current-buffer)))
           ;; Check if base buffer uses tiddlywiki-mode
           (with-current-buffer (buffer-base-buffer (or buffer (current-buffer)))
             (or (eq major-mode 'tiddlywiki-mode)
                 (and (boundp 'polymode-mode) polymode-mode
                      (boundp 'pm/polymode) pm/polymode
                      (let ((hostmode (ignore-errors (eieio-oref pm/polymode 'hostmode))))
                        (and hostmode
                             (eq (ignore-errors (eieio-oref hostmode 'mode))
                                 'tiddlywiki-mode)))))))
      ;; Inhibit hooks by using delay-mode-hooks
      (let ((delay-mode-hooks t))
        (prog1 (funcall orig-fun mode buffer)
          ;; Clear delayed hooks so they never run
          (setq delayed-mode-hooks nil)))
    ;; Normal execution
    (funcall orig-fun mode buffer)))

(advice-add 'pm--mode-setup :around #'tiddlywiki-polymode--around-pm-mode-setup)

;;; ============================================================
;;; Polymode Definitions
;;; ============================================================

(define-hostmode poly-tiddlywiki-hostmode
  :mode 'tiddlywiki-mode)

(define-auto-innermode poly-tiddlywiki-code-innermode
  :head-matcher "^```\\([a-zA-Z0-9_+-]*\\)$"
  :tail-matcher "^```$"
  :mode-matcher (cons "^```\\([a-zA-Z0-9_+-]+\\)$" 1)
  :head-mode 'host
  :tail-mode 'host
  :fallback-mode 'prog-mode)

(define-polymode poly-tiddlywiki-mode
  :hostmode 'poly-tiddlywiki-hostmode
  :innermodes '(poly-tiddlywiki-code-innermode))

;;; ============================================================
;;; Unload Function
;;; ============================================================

(defun tiddlywiki-polymode-unload-function ()
  "Unload function for tiddlywiki-polymode.
Removes advices added by this package."
  (advice-remove 'pm--mode-setup #'tiddlywiki-polymode--around-pm-mode-setup)
  (advice-remove 'pm-get-mode-symbol-from-name #'tiddlywiki-polymode--around-get-mode-symbol)
  ;; Return nil to indicate success
  nil)

(provide 'tiddlywiki-polymode)

;;; tiddlywiki-polymode.el ends here
