;;; tiddlywiki-mode.el --- Major mode for TiddlyWiki .tid files -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Your Name
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: languages, tiddlywiki, wiki
;; URL: https://github.com/your-repo/tiddlywiki-mode

;;; Commentary:

;; A major mode for editing TiddlyWiki .tid files with:
;; - Syntax highlighting for TiddlyWiki wikitext
;; - Multi-wiki support
;; - Navigation and search functions
;; - Header management and automatic timestamp updates

;;; Code:

(require 'cl-lib)

;; Optional dependency declarations
(defvar consult-ripgrep-args)
(declare-function consult-ripgrep "consult")

;;; ============================================================
;;; Configuration Variables
;;; ============================================================

(defgroup tiddlywiki nil
  "Major mode for editing TiddlyWiki files."
  :group 'languages
  :prefix "tiddlywiki-")

(defcustom tiddlywiki-wiki-alist nil
  "List of TiddlyWiki wikis.
Each entry is a cons cell (NAME . PATH) where NAME is a string
identifier and PATH is the path to the tiddlers directory."
  :type '(alist :key-type string :value-type directory)
  :group 'tiddlywiki)

(defcustom tiddlywiki-default-wiki nil
  "Name of the default wiki to use.
Should be a key in `tiddlywiki-wiki-alist'."
  :type '(choice (const nil) string)
  :group 'tiddlywiki)

(defcustom tiddlywiki-update-modified-on-save t
  "If non-nil, update the modified timestamp when saving."
  :type 'boolean
  :group 'tiddlywiki)

(defvar tiddlywiki-current-wiki nil
  "The currently selected wiki name.")

;;; ============================================================
;;; Faces
;;; ============================================================

(defgroup tiddlywiki-faces nil
  "Faces for TiddlyWiki mode."
  :group 'tiddlywiki
  :group 'faces)

(defface tiddlywiki-header-field-face
  '((t :inherit font-lock-keyword-face))
  "Face for header field names."
  :group 'tiddlywiki-faces)

(defface tiddlywiki-header-value-face
  '((t :inherit font-lock-string-face))
  "Face for header field values."
  :group 'tiddlywiki-faces)

(defface tiddlywiki-heading-1-face
  '((t :inherit font-lock-function-name-face :height 1.4 :weight bold))
  "Face for level 1 headings."
  :group 'tiddlywiki-faces)

(defface tiddlywiki-heading-2-face
  '((t :inherit font-lock-function-name-face :height 1.3 :weight bold))
  "Face for level 2 headings."
  :group 'tiddlywiki-faces)

(defface tiddlywiki-heading-3-face
  '((t :inherit font-lock-function-name-face :height 1.2 :weight bold))
  "Face for level 3 headings."
  :group 'tiddlywiki-faces)

(defface tiddlywiki-heading-4-face
  '((t :inherit font-lock-function-name-face :height 1.1 :weight bold))
  "Face for level 4 headings."
  :group 'tiddlywiki-faces)

(defface tiddlywiki-heading-5-face
  '((t :inherit font-lock-function-name-face :height 1.05 :weight bold))
  "Face for level 5 headings."
  :group 'tiddlywiki-faces)

(defface tiddlywiki-heading-6-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for level 6 headings."
  :group 'tiddlywiki-faces)

(defface tiddlywiki-bold-face
  '((t :weight bold))
  "Face for bold text."
  :group 'tiddlywiki-faces)

(defface tiddlywiki-italic-face
  '((t :slant italic))
  "Face for italic text."
  :group 'tiddlywiki-faces)

(defface tiddlywiki-underline-face
  '((t :underline t))
  "Face for underlined text."
  :group 'tiddlywiki-faces)

(defface tiddlywiki-strikethrough-face
  '((t :strike-through t))
  "Face for strikethrough text."
  :group 'tiddlywiki-faces)

(defface tiddlywiki-superscript-face
  '((t :height 0.8 :raise 0.3))
  "Face for superscript text."
  :group 'tiddlywiki-faces)

(defface tiddlywiki-subscript-face
  '((t :height 0.8 :raise -0.3))
  "Face for subscript text."
  :group 'tiddlywiki-faces)

(defface tiddlywiki-code-face
  '((t :inherit font-lock-constant-face))
  "Face for inline code."
  :group 'tiddlywiki-faces)

(defface tiddlywiki-link-face
  '((t :inherit font-lock-keyword-face :underline t))
  "Face for links."
  :group 'tiddlywiki-faces)

(defface tiddlywiki-transclusion-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for transclusions."
  :group 'tiddlywiki-faces)

(defface tiddlywiki-macro-face
  '((t :inherit font-lock-builtin-face))
  "Face for macros."
  :group 'tiddlywiki-faces)

(defface tiddlywiki-widget-face
  '((t :inherit font-lock-type-face))
  "Face for widgets."
  :group 'tiddlywiki-faces)

(defface tiddlywiki-list-face
  '((t :inherit font-lock-comment-delimiter-face))
  "Face for list markers."
  :group 'tiddlywiki-faces)

(defface tiddlywiki-definition-face
  '((t :inherit font-lock-doc-face))
  "Face for definition lists."
  :group 'tiddlywiki-faces)

(defface tiddlywiki-hr-face
  '((t :inherit font-lock-comment-face))
  "Face for horizontal rules."
  :group 'tiddlywiki-faces)

(defface tiddlywiki-code-block-face
  '((t :inherit font-lock-string-face))
  "Face for code blocks."
  :group 'tiddlywiki-faces)

;;; ============================================================
;;; Font Lock (Syntax Highlighting)
;;; ============================================================

(defvar tiddlywiki-font-lock-keywords
  `(
    ;; Header fields (before the blank line)
    ("^\\([a-z-]+\\):\\s-*\\(.*\\)$"
     (1 'tiddlywiki-header-field-face)
     (2 'tiddlywiki-header-value-face))

    ;; Headings (! at beginning of line)
    ("^!!!!!!\\s-*\\(.*\\)$" (0 'tiddlywiki-heading-6-face))
    ("^!!!!!\\s-*\\(.*\\)$" (0 'tiddlywiki-heading-5-face))
    ("^!!!!\\s-*\\(.*\\)$" (0 'tiddlywiki-heading-4-face))
    ("^!!!\\s-*\\(.*\\)$" (0 'tiddlywiki-heading-3-face))
    ("^!!\\s-*\\(.*\\)$" (0 'tiddlywiki-heading-2-face))
    ("^!\\s-*\\(.*\\)$" (0 'tiddlywiki-heading-1-face))

    ;; Code blocks ```
    ("^```.*\n\\(\\(?:.*\n\\)*?\\)```"
     (0 'tiddlywiki-code-block-face))

    ;; Inline code `code`
    ("`\\([^`\n]+\\)`"
     (0 'tiddlywiki-code-face))

    ;; Bold ''text''
    ("''\\([^'\n]+\\)''"
     (0 'tiddlywiki-bold-face))

    ;; Italic //text//
    ("//\\([^/\n]+\\)//"
     (0 'tiddlywiki-italic-face))

    ;; Underline __text__
    ("__\\([^_\n]+\\)__"
     (0 'tiddlywiki-underline-face))

    ;; Strikethrough ~~text~~
    ("~~\\([^~\n]+\\)~~"
     (0 'tiddlywiki-strikethrough-face))

    ;; Superscript ^^text^^
    ("\\^\\^\\([^^]+\\)\\^\\^"
     (0 'tiddlywiki-superscript-face))

    ;; Subscript ,,text,,
    (",,\\([^,\n]+\\),,"
     (0 'tiddlywiki-subscript-face))

    ;; Links [[text]] or [[display|link]]
    ("\\[\\[\\([^]|]+\\)\\(?:|[^]]+\\)?\\]\\]"
     (0 'tiddlywiki-link-face))

    ;; External links [ext[text|url]] or [ext[url]]
    ("\\[ext\\[\\([^]]+\\)\\]\\]"
     (0 'tiddlywiki-link-face))

    ;; Image links [img[url]] or [img[alt|url]]
    ("\\[img\\[[^]]+\\]\\]"
     (0 'tiddlywiki-link-face))

    ;; Transclusions {{tiddler}} or {{tiddler||template}}
    ("{{\\([^}]+\\)}}"
     (0 'tiddlywiki-transclusion-face))

    ;; Macros <<macro args>>
    ("<<\\([^>]+\\)>>"
     (0 'tiddlywiki-macro-face))

    ;; Widgets <$widget> or </$widget>
    ("</?\\$[a-zA-Z][^>]*>"
     (0 'tiddlywiki-widget-face))

    ;; List markers at beginning of line (* - #)
    ("^\\([*#]+\\|[-]\\)\\s-"
     (1 'tiddlywiki-list-face))

    ;; Definition lists (; :)
    ("^\\([;:]\\)\\s-"
     (1 'tiddlywiki-definition-face))

    ;; Horizontal rule ---
    ("^---+$"
     (0 'tiddlywiki-hr-face))

    ;; Block quotes <<<
    ("^<<<.*$"
     (0 'font-lock-comment-face))

    ;; Filtered transclusions {{{ filter }}}
    ("{{{\\([^}]+\\)}}}"
     (0 'tiddlywiki-transclusion-face))

    ;; Procedure/function definitions
    ("^\\\\\\(define\\|procedure\\|function\\|widget\\)\\s-+\\([a-zA-Z0-9_-]+\\)"
     (1 'font-lock-keyword-face)
     (2 'font-lock-function-name-face))

    ;; Variables $(variable)$
    ("\\$(\\([^)]+\\))\\$"
     (0 'font-lock-variable-name-face))

    ;; Filter attribute [attribute]
    ("\\[\\([a-z]+\\)\\[\\([^]]*\\)\\]\\]"
     (0 'font-lock-type-face)))
  "Font lock keywords for TiddlyWiki mode.")

;;; ============================================================
;;; Header Management
;;; ============================================================

(defvar-local tiddlywiki-header-hidden nil
  "Non-nil if the header is currently hidden via narrowing.")

(defun tiddlywiki-header-end-position ()
  "Return the position of the end of the header (first blank line).
Returns nil if no header is found."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^$" nil t)
        (point)))))

(defun tiddlywiki-content-start-position ()
  "Return the position where content starts (after header and blank line)."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (if (re-search-forward "^$" nil t)
          (1+ (point))
        (point-min)))))

(defun tiddlywiki-parse-header ()
  "Parse the header of the current .tid file.
Returns an alist of (FIELD . VALUE) pairs."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((header-end (tiddlywiki-header-end-position))
            (fields nil))
        (when header-end
          (while (and (< (point) header-end)
                      (re-search-forward "^\\([a-z-]+\\):\\s-*\\(.*\\)$" header-end t))
            (push (cons (match-string 1) (match-string 2)) fields)))
        (nreverse fields)))))

(defun tiddlywiki-get-header-field (field)
  "Get the value of FIELD from the header."
  (cdr (assoc field (tiddlywiki-parse-header))))

(defun tiddlywiki-set-header-field (field value)
  "Set FIELD to VALUE in the header.
If FIELD doesn't exist, add it before the blank line."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((header-end (tiddlywiki-header-end-position)))
        (if (re-search-forward (format "^%s:\\s-*.*$" (regexp-quote field)) header-end t)
            (replace-match (format "%s: %s" field value))
          (when header-end
            (goto-char header-end)
            (beginning-of-line)
            (insert (format "%s: %s\n" field value))))))))

(defun tiddlywiki-current-timestamp ()
  "Return the current timestamp in TiddlyWiki format (YYYYMMDDHHmmssSSS)."
  (format-time-string "%Y%m%d%H%M%S000"))

(defun tiddlywiki-update-modified ()
  "Update the modified timestamp in the header.
Called automatically before saving if `tiddlywiki-update-modified-on-save'
is non-nil."
  (when (and tiddlywiki-update-modified-on-save
             (tiddlywiki-header-end-position))
    (tiddlywiki-set-header-field "modified" (tiddlywiki-current-timestamp))))

;;; ============================================================
;;; Header Narrowing
;;; ============================================================

(defun tiddlywiki-hide-header ()
  "Hide the header by narrowing to the content."
  (interactive)
  (let ((content-start (tiddlywiki-content-start-position)))
    (when (> content-start (point-min))
      (narrow-to-region content-start (point-max))
      (setq tiddlywiki-header-hidden t)
      (message "Header hidden. Use `tiddlywiki-show-header' to show it."))))

(defun tiddlywiki-show-header ()
  "Show the header by widening the buffer."
  (interactive)
  (widen)
  (setq tiddlywiki-header-hidden nil)
  (message "Header shown."))

(defun tiddlywiki-toggle-header ()
  "Toggle the visibility of the header."
  (interactive)
  (if tiddlywiki-header-hidden
      (tiddlywiki-show-header)
    (tiddlywiki-hide-header)))

(defun tiddlywiki-goto-content ()
  "Move point to the beginning of the content (after header)."
  (interactive)
  (widen)
  (goto-char (tiddlywiki-content-start-position)))

(defun tiddlywiki-goto-header ()
  "Move point to the beginning of the header."
  (interactive)
  (widen)
  (goto-char (point-min)))

;;; ============================================================
;;; Multi-Wiki Support
;;; ============================================================

(defun tiddlywiki-select-wiki ()
  "Select a wiki from `tiddlywiki-wiki-alist'."
  (interactive)
  (if (null tiddlywiki-wiki-alist)
      (user-error "No wikis configured. Set `tiddlywiki-wiki-alist' first")
    (let ((wiki-name (completing-read "Select wiki: "
                                      (mapcar #'car tiddlywiki-wiki-alist)
                                      nil t nil nil
                                      tiddlywiki-default-wiki)))
      (setq tiddlywiki-current-wiki wiki-name)
      (message "Selected wiki: %s" wiki-name)
      wiki-name)))

(defun tiddlywiki-current-wiki-path ()
  "Return the path of the currently active wiki.
If no wiki is selected, prompt to select one."
  (unless tiddlywiki-current-wiki
    (if tiddlywiki-default-wiki
        (setq tiddlywiki-current-wiki tiddlywiki-default-wiki)
      (tiddlywiki-select-wiki)))
  (let ((path (cdr (assoc tiddlywiki-current-wiki tiddlywiki-wiki-alist))))
    (if path
        (expand-file-name path)
      (user-error "Wiki '%s' not found in `tiddlywiki-wiki-alist'" tiddlywiki-current-wiki))))

(defun tiddlywiki-wiki-path (wiki-name)
  "Return the path for WIKI-NAME."
  (let ((path (cdr (assoc wiki-name tiddlywiki-wiki-alist))))
    (if path
        (expand-file-name path)
      (user-error "Wiki '%s' not found" wiki-name))))

;;; ============================================================
;;; Navigation Functions
;;; ============================================================

(defun tiddlywiki-list-tiddlers (&optional wiki)
  "Return a list of tiddler files in WIKI.
If WIKI is nil, use the current wiki."
  (let* ((wiki-name (or wiki tiddlywiki-current-wiki))
         (path (if wiki-name
                   (tiddlywiki-wiki-path wiki-name)
                 (tiddlywiki-current-wiki-path))))
    (directory-files path t "^[^.$].*\\.tid$")))

(defun tiddlywiki-open-tiddler (&optional wiki)
  "Open a tiddler from WIKI using completion.
If WIKI is nil, use the current wiki.
With prefix argument, prompt to select a wiki first."
  (interactive
   (list (when current-prefix-arg
           (tiddlywiki-select-wiki))))
  (let* ((wiki-name (or wiki tiddlywiki-current-wiki))
         (path (if wiki-name
                   (tiddlywiki-wiki-path wiki-name)
                 (tiddlywiki-current-wiki-path)))
         (files (tiddlywiki-list-tiddlers wiki-name))
         (file-names (mapcar #'file-name-nondirectory files))
         (selected (completing-read "Open tiddler: " file-names nil t)))
    (when selected
      (find-file (expand-file-name selected path)))))

(defun tiddlywiki-grep (&optional wiki)
  "Search in tiddlers of WIKI using `consult-ripgrep'.
If WIKI is nil, use the current wiki.
With prefix argument, prompt to select a wiki first.
Excludes system tiddlers (those containing $)."
  (interactive
   (list (when current-prefix-arg
           (tiddlywiki-select-wiki))))
  (unless (require 'consult nil t)
    (user-error "This function requires the 'consult' package"))
  (let* ((wiki-name (or wiki tiddlywiki-current-wiki))
         (path (if wiki-name
                   (tiddlywiki-wiki-path wiki-name)
                 (tiddlywiki-current-wiki-path)))
         (consult-ripgrep-args (concat consult-ripgrep-args " --glob !*\\$*")))
    (consult-ripgrep path)))

(defun tiddlywiki-new-tiddler (title &optional wiki)
  "Create a new tiddler with TITLE in WIKI.
If WIKI is nil, use the current wiki."
  (interactive
   (list (read-string "Tiddler title: ")
         (when current-prefix-arg
           (tiddlywiki-select-wiki))))
  (let* ((wiki-name (or wiki tiddlywiki-current-wiki))
         (path (if wiki-name
                   (tiddlywiki-wiki-path wiki-name)
                 (tiddlywiki-current-wiki-path)))
         (filename (concat (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_" title) ".tid"))
         (filepath (expand-file-name filename path))
         (timestamp (tiddlywiki-current-timestamp)))
    (if (file-exists-p filepath)
        (user-error "Tiddler file already exists: %s" filepath)
      (find-file filepath)
      (insert (format "created: %s\nmodified: %s\ntags: \ntitle: %s\ntype: text/vnd.tiddlywiki\n\n"
                      timestamp timestamp title))
      (message "Created new tiddler: %s" title))))

(defun tiddlywiki-get-title ()
  "Get the title of the current tiddler from its header."
  (tiddlywiki-get-header-field "title"))

(defun tiddlywiki-get-tags ()
  "Get the tags of the current tiddler as a list."
  (let ((tags-str (tiddlywiki-get-header-field "tags")))
    (when tags-str
      (split-string tags-str " " t))))

(defun tiddlywiki-add-tag (tag)
  "Add TAG to the current tiddler."
  (interactive "sTag to add: ")
  (let* ((current-tags (tiddlywiki-get-header-field "tags"))
         (tags-list (if current-tags (split-string current-tags " " t) nil)))
    (unless (member tag tags-list)
      (push tag tags-list)
      (tiddlywiki-set-header-field "tags" (string-join tags-list " "))
      (message "Added tag: %s" tag))))

(defun tiddlywiki-remove-tag (tag)
  "Remove TAG from the current tiddler."
  (interactive
   (list (completing-read "Tag to remove: " (tiddlywiki-get-tags) nil t)))
  (let* ((current-tags (tiddlywiki-get-header-field "tags"))
         (tags-list (if current-tags (split-string current-tags " " t) nil)))
    (when (member tag tags-list)
      (setq tags-list (delete tag tags-list))
      (tiddlywiki-set-header-field "tags" (string-join tags-list " "))
      (message "Removed tag: %s" tag))))

;;; ============================================================
;;; Keymap
;;; ============================================================

(defvar tiddlywiki-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-o") #'tiddlywiki-open-tiddler)
    (define-key map (kbd "C-c C-s") #'tiddlywiki-grep)
    (define-key map (kbd "C-c C-n") #'tiddlywiki-new-tiddler)
    (define-key map (kbd "C-c C-w") #'tiddlywiki-select-wiki)
    (define-key map (kbd "C-c C-h") #'tiddlywiki-toggle-header)
    (define-key map (kbd "C-c C-t") #'tiddlywiki-add-tag)
    map)
  "Keymap for TiddlyWiki mode.")

;;; ============================================================
;;; Mode Definition
;;; ============================================================

;;;###autoload
(define-derived-mode tiddlywiki-mode text-mode "TiddlyWiki"
  "Major mode for editing TiddlyWiki .tid files.

TiddlyWiki is a non-linear personal web notebook.  This mode provides
syntax highlighting for TiddlyWiki's wikitext markup, header management,
and navigation functions for multi-wiki setups.

\\{tiddlywiki-mode-map}"
  :group 'tiddlywiki

  ;; Font lock
  (setq-local font-lock-defaults '(tiddlywiki-font-lock-keywords nil nil))
  (setq-local font-lock-multiline t)

  ;; Comments (HTML-style)
  (setq-local comment-start "<!-- ")
  (setq-local comment-end " -->")
  (setq-local comment-start-skip "<!--[ \t]*")
  (setq-local comment-end-skip "[ \t]*-->")

  ;; Paragraphs
  (setq-local paragraph-start "\f\\|[ \t]*$\\|[ \t]*[*#;:]")
  (setq-local paragraph-separate "[ \t\f]*$")

  ;; Update modified timestamp on save
  (add-hook 'before-save-hook #'tiddlywiki-update-modified nil t)

  ;; Ensure header is shown when saving
  (add-hook 'before-save-hook #'tiddlywiki--ensure-widened-for-save nil t))

(defun tiddlywiki--ensure-widened-for-save ()
  "Ensure the buffer is widened before saving."
  (when tiddlywiki-header-hidden
    (widen)
    (setq tiddlywiki-header-hidden nil)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tid\\'" . tiddlywiki-mode))

(provide 'tiddlywiki-mode)

;;; tiddlywiki-mode.el ends here
