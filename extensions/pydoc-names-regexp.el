;;; pydoc-names-regexp.el --- Pydoc Names Regexp Extension  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/pydoc-plugins/extensions/pydoc-names-regexp.el
;; Version: 0.1.1
;; Keywords:
;; Package-Requires: ((emacs "30.1") (pydoc "0.3"))
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful,n but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; The regexp version of `pydoc'. The code has been retired after the treesit
;; version release. It is kept for reference, for now.
;;
;;; Code:

(defvar pydoc--import-regex
  (let* ((ws+ "[\\\n\s-]+")
         (ws* "[\\\n\s-]*")
         (sym "[a-zA-Z0-9_]+")
         (path "[a-zA-Z0-9_.]+")
         (sym-ptn (concat sym "\\(" ws+ "as" ws+ sym "\\)?"))
         (path-ptn (concat path "\\(" ws+ "as" ws+ sym "\\)?")))
    (concat
     "^\\s-*\\(import\\|from\\)" ws+ "\\("
     "\\(\\(" path "\\)" ws+ "import" ws+
     "(?" ws* "\\(\\(" sym-ptn ws* "," ws* "\\)*\\(" sym-ptn "\\)\\)" ws* ")?"
     "\\)\\|\\(\\(" path-ptn ws* "," ws* "\\)*\\(" path-ptn "\\)" ws* "\\)"
     "\\)" ws* "\\(#.+\\)?" "$"))
  "Regexp to extract components from a '[from ... ] import ...' line.")


(defun pydoc-paths-from-imports ()
  "Get the alist mapping symbol name to flat path to the object."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* ((match-found (re-search-forward pydoc--import-regex nil t))
           (ws+ "[\n\s-]+")
           (as (concat ws+ "as[\\\n\s-]+" ))
           sym-to-paths)
      (while match-found
        (let ((import-or-from-import (match-string 1))
              (path (or (match-string 4) (match-string 10)))
              (items (and (match-string 4) (match-string 5))))
          (if (string= import-or-from-import "import")
              (dolist (item (split-string path "," t ws+))
                (let* ((item (substring-no-properties item))
                       (path-and-alias (split-string item as t ws+))
                       (path (car path-and-alias))
                       (path (pydoc--oref-ensure-fully-qualified path))
                       (alias (cadr path-and-alias)))
                  (push `(,(if alias alias path) . ,path)
                        sym-to-paths)))
            ;; Parse the line of form 'from ... import ...'
            (dolist (item (split-string items "," t ws+))
              (let* ((item (substring-no-properties item))
                     (path (substring-no-properties
                            (replace-regexp-in-string
                             "\\`[\n\s-]+\\|[\n\s-]+\\'" "" path)))
                     (path (pydoc--oref-ensure-fully-qualified path))
                     (ref-and-alias (split-string item as t ws+))
                     (ref (car ref-and-alias))
                     (alias (cadr ref-and-alias)))
                (push `(,(if alias alias ref) . ,(format "%s.%s" path ref))
                      sym-to-paths)))))
        (setq match-found (re-search-forward pydoc--import-regex nil t)))
      sym-to-paths)))


(defun pydoc--locate-python-package-root (&optional dir)
  "Search up from DIR to find the file path to Python package root.
When not given, DIR defaults to the value of function `buffer-file-name'."
  (let* ((dir (or dir (file-name-directory (buffer-file-name))))
         (init-file (expand-file-name "__init__.py" dir)))
    (cond
     ((and (file-exists-p init-file)
           (not (let ((parent (file-name-directory (directory-file-name dir))))
                  (and (not (equal parent dir)) ; not at filesystem root
                       (file-exists-p (expand-file-name "__init__.py" parent))))))
      ;; DIR is at the top-level package.
      (directory-file-name dir))
     (t
      (let ((parent (file-name-directory (directory-file-name dir))))
        (when (not (equal parent dir))
          (pydoc--locate-python-package-root parent)))))))


(defun pydoc--oref-extract ()
  "Get the reference to the Python object at point in dotted notation.
This function attempts to extract all the module path components up to the
point. For example, given 'foo.bar.baz' and the point is on 'bar', then
'foo.bar' will be returned."
  (save-excursion
    (let ((start (point))
          (end (point))
          (symbol-chars "_a-zA-Z0-9")
          full-path)
      (skip-chars-backward (concat symbol-chars "."))
      (setq start (point))
      (goto-char end)
      (skip-chars-forward symbol-chars)
      (setq end (point))
      (setq full-path (buffer-substring-no-properties start end))
      full-path)))


(defun pydoc--oref-parts (path)
  "Generate combinations of parts constituting PATH in dotted notation.
This function returns a list of cons cells where each car is a prefix and cdr is
the suffix. For example, 'a.b.c' becomes (('a' . '.b.c') ('a.b' . '.c') ('a.b.c'
. ''))"
  (let ((parts (split-string path "\\." t))
        result)
    (when parts
      (let ((prefix ""))
        (dolist (part parts result)
          (setq prefix (if (string-empty-p prefix) part (concat prefix "." part)))
          (let ((suffix
                 (string-remove-prefix
                  (concat prefix ".")
                  (concat prefix "." (string-join (cdr (member part parts)) ".")))))
            (push (cons prefix (concat (if (> (length suffix) 0) "." "") suffix))
                  result)))))
    (nreverse result)))


(defun pydoc--oref-ensure-fully-qualified (path)
  "Get the fully-qualified name for the Python object referenced by PATH.
This function expands a relative PATH to its fully-qualified name."
  (if (not (string= (substring path 0 1) "."))
      path
    ;; This is a relative path, so look for a matching fully-qualified
    ;; reference to an existing, imported object.
    (let* ((ndots (progn (string-match "\\`\\.+\\b" path)
                         (length (match-string 0 path))))
           (package-root (pydoc--locate-python-package-root))
           (current-dir (directory-file-name (file-name-directory (buffer-file-name))))
           (root-module (file-name-nondirectory (directory-file-name package-root)))
           (module-path (seq-remove
                         (lambda (x) (string= x ""))
                         `(,root-module
                           ,@(string-split (string-remove-prefix package-root
                                                                 current-dir)
                                           (regexp-opt (list "/" "\\"))))))
           (module-path (string-join (butlast module-path (1- ndots)) ".")))
      (if (eq ndots (length path))
          module-path
        (concat module-path "." (substring path ndots))))))

(defun pydoc-regexp-at-point (&optional prompt)
  "Try to get help for thing at point.
With non-nil PROMPT or without a thing, prompt for the function or module."
  (interactive "P")
  (let ((name-of-symbol (if (null (symbol-at-point)) "" (pydoc--oref-extract)))
        (candidates (pydoc-all-modules prompt)))
    (pydoc
     (if-let*
         ((name
           (or
            (and (member name-of-symbol candidates)
                 name-of-symbol)
            (let ((matches
                   (seq-filter (lambda (s)
                                 (string-match-p
                                  (format "\\.%s$" name-of-symbol)
                                  s))
                               candidates)))
              (when (eq (length matches) 1)
                (car matches)))
            (let ((sym-to-paths (pydoc-paths-from-imports)))
              (cl-loop for pair in (pydoc--oref-parts name-of-symbol)
                       when (cdr-safe (assoc (car pair) sym-to-paths))
                       return (concat (cdr-safe (assoc (car pair) sym-to-paths))
                                      (cdr pair)))))))
         name
       (completing-read "Name of function or module: "
                        candidates
                        nil nil
                        name-of-symbol)))))

(provide 'pydoc-names-regexp)
;;; pydoc-names-regexp.el ends here
