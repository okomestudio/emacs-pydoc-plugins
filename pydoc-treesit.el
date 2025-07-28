;;; pydoc-treesit.el --- Pydoc with Tree-sitter  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/pydoc-treesit
;; Version: 0.1.1
;; Keywords:
;; Package-Requires: ((emacs "30.1") (pydoc "0.2"))
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This package uses `treesit' to inspect the object path within Python code to
;; feed `pydoc' lookup.
;;
;;; Code:

(require 'pydoc)

(defun pydoc-treesit--path-to-identifier (node)
  "Get the partial path to identifier at treesit NODE."
  (when-let* ((node (and (equal (treesit-node-type node)
                                "identifier")
                         (treesit-node-at (point)))))
    (let* ((parent (treesit-node-parent node))
           (object (treesit-node-child-by-field-name parent "object"))
           (attribute (treesit-node-child-by-field-name parent "attribute"))
           (identifier (treesit-node-text node)))
      (if (and parent (equal (treesit-node-text attribute) identifier))
          (append (split-string (treesit-node-text object) "\\.")
                  `(,identifier))
        `(,identifier)))))

(defun pydoc-treesit--package-root (&optional path)
  "Search upward from PATH for the Python package root directory.
When not given, PATH defaults to the value of function `buffer-file-name'."
  (let* ((dir (or path (file-name-directory (buffer-file-name))))
         (init-file (expand-file-name "__init__.py" dir)))
    (cond
     ((and (file-exists-p init-file)
           (not (let ((parent (file-name-directory (directory-file-name dir))))
                  (and (not (equal parent dir))
                       (file-exists-p (expand-file-name "__init__.py" parent))))))
      ;; `dir' is at the top-level package.
      (directory-file-name dir))
     (t
      (let ((parent (file-name-directory (directory-file-name dir))))
        (when (not (equal parent dir))
          (pydoc-treesit--package-root parent)))))))

(defun pydoc-treesit--fully-qualify-path (path)
  "Fully resolve Python module PATH when given relatively."
  (if-let*
      ((num-dots (if (string-match "\\`\\.+\\b" path)
                     (length (match-string 0 path))))
       (package-root (pydoc-treesit--package-root))
       (module-path-comps
        (seq-remove
         (lambda (it) (string= it ""))
         `(,(file-name-nondirectory (directory-file-name package-root))
           ,@(string-split
              (string-remove-prefix
               package-root
               (directory-file-name (file-name-directory (buffer-file-name))))
              (regexp-opt (list "/" "\\"))))))
       (module-path (string-join (butlast module-path-comps (1- num-dots))
                                 ".")))
      (if (eq num-dots (length path))
          module-path
        (concat module-path "." (substring path num-dots)))
    path))

(defvar pydoc-treesit--import-query
  '((import_statement
     name: [(dotted_name (identifier) @name-1)
            (aliased_import
             name: (dotted_name (identifier) @name-1-with-alias)
             alias: (identifier) @alias-1)])
    (import_from_statement
     module_name: [(relative_import (_) @module-relative-2)
                   (dotted_name (identifier) @module-2)]
     name: [(dotted_name (identifier) @name-2)
            (aliased_import
             name: (dotted_name (identifier) @name-2-with-alias)
             alias: (identifier) @alias-2)]))
  "Queries for Python import statements.")

(defun pydoc-treesit--partial-match-import-path (identifier)
  "Given an IDENTIFIER, return a matching import path.
IDENTIFIER may be a qualified name (i.e., dotted name). In that case, prefix
matching is performed against imported names."
  (let* ((capture (cl-find-if
                   (lambda (c)
                     (and (equal (treesit-node-text (cdr c)) identifier)
                          (cdr c)))
                   (treesit-query-capture (treesit-buffer-root-node)
                                          pydoc-treesit--import-query)))
         (c (cdr capture)))
    (pcase (car capture)
      ('name-1
       (let* ((p (treesit-node-parent c)))
         `(,(treesit-node-text p))))
      ('alias-1
       (let* ((p (treesit-node-parent c))
              (a (treesit-node-child-by-field-name p "name")))
         `(,(treesit-node-text a))))
      ('name-2
       (let* ((p (treesit-node-parent c))
              (q (treesit-node-parent p))
              (m (treesit-node-child-by-field-name q "module_name")))
         `(,(pydoc-treesit--fully-qualify-path (treesit-node-text m))
           ,(treesit-node-text c))))
      ('alias-2
       (let* ((p (treesit-node-parent c))
              (a (treesit-node-child-by-field-name p "name"))
              (q (treesit-node-parent p))
              (m (treesit-node-child-by-field-name q "module_name")))
         `(,(pydoc-treesit--fully-qualify-path (treesit-node-text m))
           ,(treesit-node-text a)))))))

(defun pydoc-treesit--resolve-to-full-path (node)
  "Resolve treesit NODE to the full path."
  (if-let* ((parts (pydoc-treesit--path-to-identifier node)))
      (let* ((tot (length parts))
             (len tot)
             matched)
        (while (not (or matched (eq len 0)))
          (setq matched (pydoc-treesit--partial-match-import-path
                         (string-join (take len parts) ".")))
          (when matched
            (setq matched (append matched (last parts (- tot len)))))
          (setq len (1- len)))
        (string-join (or matched parts) "."))
    (message "No valid identifier at point")))

;;;###autoload
(defun pydoc-treesit-at-point ()
  "Get help for a Python object at point using `treesit'."
  (interactive)
  (if (derived-mode-p '(python-ts-mode))
      (let ((s (pydoc-treesit--resolve-to-full-path (treesit-node-at (point)))))
        (pydoc s))
    (message "Not in `python-ts-mode'")))

(provide 'pydoc-treesit)
;;; pydoc-treesit.el ends here
