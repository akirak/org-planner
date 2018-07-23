;;; org-focus.el --- Agile within a single org file -*- lexical-binding: t -*-

;; Copyright (C) 2018 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 1.0-pre
;; Package-Requires: ((emacs "25.1") (dash "2.10"))
;; URL: https://github.com/akirak/org-focus

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is a collection of tools for planning tasks using
;; org-mode.

;;; Code:

(require 'org)
(require 'org-element)
(require 'dash)

(declare-function 'ivy-read "ivy")
(declare-function 'ivy-add-actions "ivy")

(defgroup org-focus nil
  "A collection of helpers for planning important tasks using
org-mode."
  :prefix "org-focus"
  :group 'calendar)

;;;; Variables

(defvar org-focus-buffer nil)

;;;; Custom variables

(defcustom org-focus-file nil
  "File name of your project tracker."
  :type 'string
  :group 'org-focus)

(defcustom org-focus-completion-engine
  (cond
   ((bound-and-true-p ivy-mode) 'ivy)
   (t nil))
  "Completion engine used in org-focus."
  :type '(coice (const :tag "Ivy" ivy)
                (const :tag "Completing-read" nil))
  :group 'org-focus)

(defcustom org-focus-show-entry t
  "If non-nil, show visited entries."
  :type 'boolean
  :group 'org-focus)

(defcustom org-focus-current-todo-regexp "NEXT"
  "Regular expression for currently active todos."
  :type 'string
  :group 'org-focus)

(defcustom org-focus-show-active-groups-first t
  "If non-nil, show active groups first in `org-focus-show-group'."
  :type 'boolean
  :group 'org-focus)

;;;; Completion interfaces

;;;###autoload
(defun org-focus-show-group ()
  "Select a group and show it."
  (interactive)
  (org-focus--select "org-focus group: "
    (org-focus--with-the-buffer-widen
     (--> (org-focus--query-groups)
          (if org-focus-show-active-groups-first
              (apply #'append
                     (mapcar #'cdr
                             (-group-by #'org-focus--group-active-p it)))
            it)
          (org-focus--candidates-from-elements it
            #'org-focus--format-group-heading-from-element)))
    #'org-focus--goto
    :ivy-caller 'org-focus-show-group))

;;;###autoload
(defun org-focus-show-current-focus ()
  "Select a current focus and show it."
  (interactive)
  (org-focus--select "org-focus current focus: "
    (org-focus--with-the-buffer-widen
     (--> (org-focus--query-focuses :current t)
          (org-focus--candidates-from-elements it)))
    #'org-focus--goto
    :ivy-caller 'org-focus-show-current-focus))

;;;; Query functions

(cl-defun org-focus--query-groups ()
  "Return a list of group elements, i.e. headings at level 1."
  (let (result)
    (goto-char (point-min))
    (while (re-search-forward (org-focus--heading-regexp 1) nil t)
      (let* ((group-end (save-excursion (org-end-of-subtree nil)))
             (element (org-element-headline-parser group-end)))
        (push element result)))
    (nreverse result)))

(cl-defun org-focus--query-focuses (&key current)
  "Return a list of focus elements, i.e. headings at level 2.

If CURRENT is non-nil, return only a list of current todos."
  (let (result
        (regexp (concat (org-focus--heading-regexp 2)
                        (when current
                          (concat org-focus-current-todo-regexp " ")))))
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (let* ((group-end (save-excursion (org-end-of-subtree nil)))
             (element (org-element-headline-parser group-end)))
        (push element result)))
    (nreverse result)))

;;;; Predicates

(defun org-focus--group-active-p (element)
  "Return t if ELEMENT contains a current todo."
  (not (null (save-excursion
               (goto-char (org-element-property :begin element))
               (re-search-forward (concat (org-focus--heading-regexp 2)
                                          org-focus-current-todo-regexp
                                          " ")
                                  (org-element-property :end element)
                                  'noerror)))))

;;;; Actions on a heading

(defun org-focus--goto (point)
  "Go to POINT in `org-focus-buffer'."
  (switch-to-buffer org-focus-buffer)
  (goto-char point)
  (when org-focus-show-entry
    (org-show-entry)))

;;;; Utilities

(defun org-focus--buffer ()
  "Get the buffer of the planner."
  (or org-focus-buffer
      (setq org-focus-buffer
            (or (find-buffer-visiting org-focus-file)
                (find-file-noselect org-focus-file)))))

(defmacro org-focus--with-the-buffer (&rest progn)
  "Evalute PROGN in `org-focus-buffer'."
  `(with-current-buffer (org-focus--buffer)
     ,@progn))

(defmacro org-focus--with-the-buffer-widen (&rest progn)
  "Evaluate PROGN in `org-focus-buffer' with the buffer widen."
  `(org-focus--with-the-buffer
    (org-with-wide-buffer ,@progn)))

(cl-defun org-focus--select (prompt alist action-fn &key ivy-caller)
  "Select an item interactively and apply a function to it."
  (declare (indent 1))
  (cl-case org-focus-completion-engine
    (ivy (ivy-read prompt alist
                   :require-match t
                   :caller ivy-caller
                   :action (lambda (input)
                             (when-let ((choice (cl-typecase input
                                                  (list (cdr input))
                                                  (string (assoc input alist)))))
                               (funcall action-fn choice)))))
    (t (when-let ((choice (assoc (completing-read prompt alist nil t)
                                 alist)))
         (funcall action-fn choice)))))

(cl-defsubst org-focus--candidates-from-elements (elements &optional
                                                           format-fn)
  "Build an alist from org elements in `org-focus-buffer'.

This function returns an alist which is suitable for use in
`org-focus--select'.

ELEMENTS is a list of elements which is returned
by `org-element-headline-parser'.

The first element of each item in the result is a string built by
applying FORMAT-FN to the element, and the second element is a
position of the headline.  If FORMAT-FN is omitted, outline paths
are generated instead."
  (declare (indent 1))
  (mapcar (lambda (element)
            (cons (funcall (or format-fn
                               #'org-focus--element-outline-path)
                           element)
                  (org-element-property :begin element)))
          elements))

(defun org-focus--element-outline-path (element)
  "Format an outline path to a headline ELEMENT."
  (save-excursion
    (goto-char (org-element-property :begin element))
    (concat (substring-no-properties
             (org-format-outline-path (org-get-outline-path t)))
            (when-let ((tags (org-get-tags)))
              (concat " :" (mapconcat #'substring-no-properties tags ":") ":")))))

(defun org-focus--format-group-heading-from-element (element)
  "Format an outline path to ELEMENT with the active status."
  (concat (if (org-focus--group-active-p element)
              "* "
            "- ")
          (org-focus--element-outline-path element)))

(defun org-focus--heading-regexp (level)
  "Build a regular expression for a heading at LEVEL."
  (concat "^" (regexp-quote (make-string (org-get-valid-level level) ?*)) " "))

(provide 'org-focus)
;;; org-focus.el ends here
