;;; tiddlywiki-mode-test.el --- Tests for tiddlywiki-mode -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for TiddlyWiki mode functionality.

;;; Code:

(require 'ert)
(require 'tiddlywiki-mode)

;;; ============================================================
;;; Test Fixtures
;;; ============================================================

(defconst tiddlywiki-test-header
  "created: 20230101120000000
modified: 20230615143000000
tags: test example
title: Test Tiddler
type: text/vnd.tiddlywiki

This is the content."
  "Sample tiddler content for testing.")

(defconst tiddlywiki-test-header-no-content
  "created: 20230101120000000
modified: 20230615143000000
title: Empty Tiddler
type: text/vnd.tiddlywiki

"
  "Sample tiddler with no content.")

;;; ============================================================
;;; Header Parsing Tests
;;; ============================================================

(ert-deftest tiddlywiki-test-parse-header ()
  "Test that header parsing works correctly."
  (with-temp-buffer
    (insert tiddlywiki-test-header)
    (tiddlywiki-mode)
    (let ((header (tiddlywiki-parse-header)))
      (should (equal (cdr (assoc "created" header)) "20230101120000000"))
      (should (equal (cdr (assoc "modified" header)) "20230615143000000"))
      (should (equal (cdr (assoc "tags" header)) "test example"))
      (should (equal (cdr (assoc "title" header)) "Test Tiddler"))
      (should (equal (cdr (assoc "type" header)) "text/vnd.tiddlywiki")))))

(ert-deftest tiddlywiki-test-get-header-field ()
  "Test getting individual header fields."
  (with-temp-buffer
    (insert tiddlywiki-test-header)
    (tiddlywiki-mode)
    (should (equal (tiddlywiki-get-header-field "title") "Test Tiddler"))
    (should (equal (tiddlywiki-get-header-field "tags") "test example"))
    (should (null (tiddlywiki-get-header-field "nonexistent")))))

(ert-deftest tiddlywiki-test-set-header-field-existing ()
  "Test updating an existing header field."
  (with-temp-buffer
    (insert tiddlywiki-test-header)
    (tiddlywiki-mode)
    (tiddlywiki-set-header-field "title" "New Title")
    (should (equal (tiddlywiki-get-header-field "title") "New Title"))))

(ert-deftest tiddlywiki-test-set-header-field-new ()
  "Test adding a new header field."
  (with-temp-buffer
    (insert tiddlywiki-test-header)
    (tiddlywiki-mode)
    (tiddlywiki-set-header-field "caption" "My Caption")
    (should (equal (tiddlywiki-get-header-field "caption") "My Caption"))))

(ert-deftest tiddlywiki-test-header-end-position ()
  "Test finding the header end position."
  (with-temp-buffer
    (insert tiddlywiki-test-header)
    (tiddlywiki-mode)
    (let ((header-end (tiddlywiki-header-end-position)))
      (should header-end)
      (should (> header-end (point-min))))))

(ert-deftest tiddlywiki-test-content-start-position ()
  "Test finding the content start position."
  (with-temp-buffer
    (insert tiddlywiki-test-header)
    (tiddlywiki-mode)
    (let ((content-start (tiddlywiki-content-start-position)))
      (should content-start)
      (goto-char content-start)
      (should (looking-at "This is the content.")))))

;;; ============================================================
;;; Tags Tests
;;; ============================================================

(ert-deftest tiddlywiki-test-get-tags ()
  "Test getting tags as a list."
  (with-temp-buffer
    (insert tiddlywiki-test-header)
    (tiddlywiki-mode)
    (let ((tags (tiddlywiki-get-tags)))
      (should (equal tags '("test" "example"))))))

(ert-deftest tiddlywiki-test-add-tag ()
  "Test adding a tag."
  (with-temp-buffer
    (insert tiddlywiki-test-header)
    (tiddlywiki-mode)
    (tiddlywiki-add-tag "newtag")
    (let ((tags (tiddlywiki-get-tags)))
      (should (member "newtag" tags))
      (should (member "test" tags))
      (should (member "example" tags)))))

(ert-deftest tiddlywiki-test-remove-tag ()
  "Test removing a tag."
  (with-temp-buffer
    (insert tiddlywiki-test-header)
    (tiddlywiki-mode)
    (tiddlywiki-remove-tag "test")
    (let ((tags (tiddlywiki-get-tags)))
      (should-not (member "test" tags))
      (should (member "example" tags)))))

;;; ============================================================
;;; Timestamp Tests
;;; ============================================================

(ert-deftest tiddlywiki-test-timestamp-format ()
  "Test that timestamps are in correct format."
  (let ((timestamp (tiddlywiki-current-timestamp)))
    (should (string-match "^[0-9]\\{17\\}$" timestamp))))

;;; ============================================================
;;; Narrowing Tests
;;; ============================================================

(ert-deftest tiddlywiki-test-hide-header ()
  "Test hiding the header via narrowing."
  (with-temp-buffer
    (insert tiddlywiki-test-header)
    (tiddlywiki-mode)
    (tiddlywiki-hide-header)
    (should tiddlywiki-header-hidden)
    (goto-char (point-min))
    (should (looking-at "This is the content."))))

(ert-deftest tiddlywiki-test-show-header ()
  "Test showing the header after hiding."
  (with-temp-buffer
    (insert tiddlywiki-test-header)
    (tiddlywiki-mode)
    (tiddlywiki-hide-header)
    (tiddlywiki-show-header)
    (should-not tiddlywiki-header-hidden)
    (goto-char (point-min))
    (should (looking-at "created:"))))

(ert-deftest tiddlywiki-test-toggle-header ()
  "Test toggling header visibility."
  (with-temp-buffer
    (insert tiddlywiki-test-header)
    (tiddlywiki-mode)
    (should-not tiddlywiki-header-hidden)
    (tiddlywiki-toggle-header)
    (should tiddlywiki-header-hidden)
    (tiddlywiki-toggle-header)
    (should-not tiddlywiki-header-hidden)))

;;; ============================================================
;;; Wiki Management Tests
;;; ============================================================

(ert-deftest tiddlywiki-test-wiki-path ()
  "Test getting wiki path from alist."
  (let ((tiddlywiki-wiki-alist '(("test-wiki" . "/tmp/test-wiki/tiddlers"))))
    (should (equal (tiddlywiki-wiki-path "test-wiki") "/tmp/test-wiki/tiddlers"))))

(ert-deftest tiddlywiki-test-wiki-path-error ()
  "Test error when wiki not found."
  (let ((tiddlywiki-wiki-alist '(("test-wiki" . "/tmp/test-wiki/tiddlers"))))
    (should-error (tiddlywiki-wiki-path "nonexistent"))))

;;; ============================================================
;;; Mode Tests
;;; ============================================================

(ert-deftest tiddlywiki-test-mode-activation ()
  "Test that the mode activates correctly."
  (with-temp-buffer
    (tiddlywiki-mode)
    (should (eq major-mode 'tiddlywiki-mode))
    (should (equal mode-name "TiddlyWiki"))))

(ert-deftest tiddlywiki-test-auto-mode-alist ()
  "Test that .tid files activate tiddlywiki-mode."
  (should (assoc "\\.tid\\'" auto-mode-alist)))

(provide 'tiddlywiki-mode-test)

;;; tiddlywiki-mode-test.el ends here
