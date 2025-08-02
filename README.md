# pydoc-plugins

The Emacs package `pydoc-plugins` provides extensions to `pydoc`
(<https://github.com/statmobile/pydoc>).

## Installation

To install `pydoc-plugins` using `use-package`, include the following code in your Emacs's
`init.el` file:

``` emacs-lisp
(use-package pydoc-plugins
  :vc (:url "https://github.com/okomestudio/emacs-pydoc-plugins.git")
  :init
  (add-to-list 'load-path
               (expand-file-name "pydoc-plugins/extensions" package-user-dir))
  (require 'pydoc-names)  ; require desired extension package
  )
```

## Extensions
### `pydoc-names`

The `pydoc-names` module extends the `pydoc` function to include as candidates all names
-- including submodules, classes, and functions, etc. -- from Python packages within the
active (virtual) environment. (In contrast, `pydoc` by default only lists top-level
modules for match.)

The candidates are parsed and persisted to a cache file. The cache won't be updated on
code modification. Invoking the interactive function `pydoc` with a prefix argument forces
refresh.
