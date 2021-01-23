# emacs-scalaunit

This is a Emacs minor mode implementation to run Scala Bloop tests.

Tests are actually run with `bloop test`.

There is no package on Elpa or Melpa.
To install it clone this to some local folder and initialize like this in Emacs:

```
(use-package ocamlunit
  :load-path "~/.emacs.d/plugins/scalaunit")
```

The default key binding is `C-c t`.

To configure a custom key binding do this:

```
(use-package scalaunit
  :load-path "~/.emacs.d/plugins/scalaunit"
  :bind (:map scalaunit-mode-map
              ("C-c C-t" . scalaunit-run))
  :commands
  (scalaunit-mode))
```

When done you have a minor mode called `scalaunit-mode`.

This mode can be enabled for basically every buffer but only `scala-mode` buffers are supported.
On other code or project it just saves the buffer.

The key sequence: `C-c t` (or a custom defined one) will first save the buffer and then run the tests using `bloop`.

After the first execution of `scalaunit-execute` you can view the "ScalaUnit output" buffer for test output.
