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

This repository includes the `pydoc-names` extension. After loading the package,

``` emacs-lisp
(require 'pydoc-names)
```

invoking `pydoc` will include all Python object names available within the current virtual
environment.
