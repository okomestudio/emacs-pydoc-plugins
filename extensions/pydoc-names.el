;;; pydoc-names.el --- Pydoc Names Extension  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/pydoc-treesit/extensions/pydoc-names.el
;; Version: 0.1.1
;; Keywords:
;; Package-Requires: ((emacs "30.1") (pydoc "0.2"))
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,n
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; The `pydoc-names' extension implements the algorithm for collecting Python
;; object paths within the current virtual environment.
;;
;;; Code:

(require 'pydoc)
(require 'project)

(defcustom pydoc-names-use-collector t
  "Set non-nil to use `pydoc-names' to collect object paths."
  :type 'boolean
  :group 'pydoc)

(defcustom pydoc-names-init-script nil
  "Full path to a Python script to perform extra initialization.
This runs before the main process of `all_names.py' runs. An example usage
includes running `django.setup()' within a Django project."
  :type 'string
  :group 'pydoc)

(defun pydoc-names--lines-from-file (file)
  "Get the lines of FILE as a list of strings."
  (with-temp-buffer
    (insert-file-contents file)
    (let (lines)
      (while (not (eobp))
        (push (buffer-substring-no-properties (point) (line-end-position))
              lines)
        (forward-line 1))
      lines)))

(defun pydoc-names--project-root ()
  "Get the project root directory.
When not inside of any project, this function returns nil."
  (and (project-current) (project-root (project-current))))

(defun pydoc-names-collector (&optional reload async)
  "Get all object paths within the current environment.
The function returns a list of strings, each string being an object path to
Python object identifier, e.g., `package.module.function'.

This Emacs Lisp function uses an external Python script to inspect the (virtual)
environment for accessible modules and their contents. The output will be
persisted in a cache file `.pydoc-names' (and its error log in
`.pydoc-names.err') to speed up the subsequent requests. This process typically
takes several seconds and won't be repeated unless RELOAD is set to non-nil,
i.e., `pydoc' is invoked interactively with a prefix argument.

When ASYNC is non-nil, the shell process for the inspection process runs
asynchronously. In this mode, the function will not return the list of module
paths and return nil instead."
  (interactive "P")
  (let* ((proj-dir (or (pydoc-names--project-root) "~/"))
         (pydoc-names-file (file-name-concat proj-dir ".pydoc-names"))
         (here (file-name-directory (symbol-file 'pydoc-names)))
         (cmd (concat pydoc-python-command " "
                      (expand-file-name "all_names.py" here) " "
                      (when pydoc-names-init-script
                        (format "-i %s " pydoc-names-init-script)))))
    (when (or (not (file-exists-p pydoc-names-file)) reload)
      (shell-command (format "%1$s >%2$s 2>%2$s.err %3$s"
                             cmd pydoc-names-file (if async "&" ""))))
    (when (not async)
      (pydoc-names--lines-from-file pydoc-names-file))))

(defun pydoc-names-all-modules (fun &optional reload)
  "Extended `pydoc-all-modules' to obtain all object paths.
Around-advise FUN (`pydoc-all-modules') to use its extended version when
`pydoc-names-use-collector' is non-nil. RELOAD is set non-nil when `pydoc' is
invoked interactively with a prefix argument."
  (if (not pydoc-names-use-collector)
      (apply fun `(,reload))
    (if (and (not reload) *pydoc-all-modules*)
        *pydoc-all-modules*
      (let ((getters '(pydoc-names-collector)))
        (setq *pydoc-all-modules*
              (delete-dups (sort (apply #'append (mapcar #'funcall getters))
                                 #'string<)))))))

(advice-add #'pydoc-all-modules :around #'pydoc-names-all-modules)

(provide 'pydoc-names)
;;; pydoc-names.el ends here
