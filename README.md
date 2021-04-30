# emacs-bloopunit

This is a Emacs minor mode to run tests in a Scala Bloop project.

Tests are actually run with `bloop test`. The plugin tries to figure out the context of the test to run. Run `bloopunit-run-all` to run all tests in a class. Use `bloopunit-run-single` to run only a single test case. This requires the cursor to be with a `test("some test case")` block, otherwise the parsing might be off. Supported ScalaTest test specs are FunSpec and WordSpecLike.

There is no package on Elpa or Melpa.
To install it clone this to some local folder and initialize like this in Emacs:

```
(use-package bloopunit
  :load-path "~/.emacs.d/plugins/bloopunit"
  ;; those are default bindings. :bind is only necessary if you want to change them. 
  :bind (:map bloopunit-mode-map
              ("C-c C-t" . bloopunit-run-all)
              ("C-c C-s" . bloopunit-run-single)
              ("C-c C-r" . bloopunit-run-last)
              ("C-c C-p" . bloopunit-select-project))
  :commands
  (bloopunit-mode))
```

The default key binding is `C-c C-<*>`.

When done you have a minor mode called `bloopunit-mode`.

This mode can be enabled for basically every buffer but only `scala-mode` buffers are supported.
On other major modes it just saves the buffer.

The key sequence: `C-c C-t` (or a custom defined one) will first save the buffer and then run the tests using `bloop`.

After the first execution of `bloopunit-run-*` you can view the "\*BloopUnit output\*" buffer for test output. It should be mentioned that this is a synchronous process in order to collect the tools return code and show a `Tests OK` or `Tests failed!` as message. So the test class should be the largest context to run this is, otherwise it will just take to long to block up Emacs.

Since Bloop commands require a project there is `bloopunit-select-project` function which reads from `bloop projects` and presents a project selection to choose from.
