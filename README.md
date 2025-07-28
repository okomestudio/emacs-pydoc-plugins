# pydoc-treesit

The Emacs package `pydoc-treesit` extends `pydoc` (<https://github.com/statmobile/pydoc>)
to use Tree-sitter to obtain Python object path.

## Installation

To install `pydoc-treesit` using `use-package`, include the following code in your Emacs's
`init.el` file:

``` emacs-lisp
(use-package pydoc-treesit
  :vc (:url "https://github.com/okomestudio/pydoc-treesit.git")
  :bind ( :map python-ts-mode-map
          ("C-h ." . pydoc-treesit-at-point) )
  :hook (python-ts-mode . (lambda () (require 'pydoc-treesit)))

  ;; If using `pydoc-names', add the following lines:
  :config
  (add-to-list 'load-path
               (expand-file-name "pydoc-treesit/extensions"
                                 package-user-dir))
  (require 'pydoc-names))
```

Use `pydoc-treesit-at-point` in place of `pydoc-at-point` to use the `treesit` version.


## The `pydoc-names` extension

This repository includes the `pydoc-names` extension. It extends the `pydoc` function to
include as candidates all names -- including submodules, classes, and functions, etc. --
from Python packages within the active (virtual) environment. By default, `pydoc` only
lists top-level modules for match.

To use the extension, add the following lines to the `use-package` form:

``` emacs-lisp
(use-package pydoc-treesit
  ...
  :config
  (add-to-list 'load-path
               (expand-file-name "pydoc-treesit/extensions"
                                 package-user-dir))
  (require 'pydoc-names))
```

The candidates are parsed and persisted to a cache file. The cache won't be updated on
code modification. Invoking `pydoc` with a prefix argument forces refresh.
