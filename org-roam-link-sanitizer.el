;;; org-roam-link-sanitizer.el --- Use ROAM_REFS as citekeys for linked documents that have a ROAM_REFS section.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2024  Maximilian Kueffner

;; Author: Maximilian Kueffner <kueffnermax@gmail.com>
;; Keywords: lisp, extensions, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'org-element)
(require 'dash)

(defgroup org-roam-link-sanitizer nil
  "Sanitize org-roam links with 'ROAM_REFS' cite keys."
  :tag "Org Roam Link Sanitizer"
  :group 'org)

(defun link-sanitizer--check-links-no-follow ()
  "Check validity of links in the (org) buffer where its applied.

For every link, we look into the PROPERTIES section of the target
and extract the 'ROAM_REFS' tag if it is present.
Returns the  links and respective cite keys (equivalent to 'ROAM_REFS')
as an alist.

TODO:
- Presumably, this only works for single roam-refs in file properties
 (one cite key only)?
- Is multiple cite keys in the 'ROAM_REFS' tag even a sane thing? Probably yes.."
  (let ((links)
        (refs))
    (org-element-map (org-element-parse-buffer) 'link
      '(lambda (n)
         (let ((tp (org-element-property :type n))
               (path (org-element-property :path n)) ;; path is an id if type is "id"
               (pusher (lambda ()
                         (let ((ref (org-entry-get 1 "ROAM_REFS")))
                           (when ref
                             (push ref refs)
                             (push (cons (org-element-property :begin n) (org-element-property :end n)) links))))))
           (cond
            ((string= tp "file")
             (progn
               (let ((path (expand-file-name (org-element-property :path n))))
                 (with-current-buffer (find-file-noselect path)
                   (save-excursion
                     (goto-char (point-min))
                     (funcall pusher))))))
            ((string= tp "id")
             (progn
               (let ((id-path (org-id-find path)))
                 (when (and (listp id-path)
                            (stringp (car id-path))
                            (file-exists-p (expand-file-name (car id-path))))
                   (progn
                     (let ((path-link (or (org-id-find path)
                                          (org-roam-id-find path))))
                       (with-current-buffer (find-file-noselect (car (org-id-find path)))
                         (save-excursion
                           (funcall pusher)))))))))
            (t (message (format "Link sanitizing has no effect on links of type %s" tp)))))))
    (-zip links refs)))


(defun link-sanitizer--translate-to-org-citekey (str)
  "Translate a STR cite key from a ROAM_REFS property to a regular org cite key."
  (format "[[%s]]" (string-replace "cite:" "cite:&" str)))

(defun link-sanitizer--replace-link-with-citekey (reg key)
  "Replace a region REG with a cite key KEY.

The semantic of REG is supposed to be a cons list like `(begin . end)'
where 'begin' marks the start of a region and 'end' the end of that region."
  (save-excursion
    (goto-char (car reg))
    (kill-region (car reg) (cdr reg))
    (insert (format "%s " (link-sanitizer--translate-to-org-citekey key)))))

(defun link-sanitizer--sanitize-links-for-export ()
  "A function that iterates over all links and does a sanitization on them."
  (let ((links (link-sanitizer--check-links-no-follow)))
    (dolist (link links)
      (link-sanitizer--replace-link-with-citekey (car link) (cdr link)))))

(defun link-sanitizer-sanitize-links-for-pdf-export (backend)
  "Wrapper function to check if BACKEND is latex derived.

It wraps `link-sanitizer--sanitize-links-for-export'."
  (message "Link sanitizer called.")
  (when (org-export-derived-backend-p backend 'latex)
    (link-sanitizer--sanitize-links-for-export)
    (message "Link sanitizer for latex derived mode.")))

(provide 'org-roam-link-sanitizer)
;;; org-roam-link-sanitizer.el ends here
