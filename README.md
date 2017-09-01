# prettier-eslint for Emacs

prettier-eslint is a function that formats the current buffer using [prettier-eslint-cli](https://github.com/prettier/prettier-eslint-cli). The package also exports a minor mode that applies `(prettier-eslint)` on save.

## Configuration

### Requirements

`(prettier-eslint)` will only work on buffers attached to files that are part of a project that has the [`prettier-eslint-cli`](https://www.npmjs.com/package/prettier-eslint-cli) installed locally. This should be true if `prettier-eslint-cli` is included under `dependencies` or `devDependencies` in your project's `package.json`.

### Basic configuration

1. Clone this repository.
2. Add the repository's path (e.g. `~/.emacs.d/prettier-eslint-emacs`) to your `load-path`.
  - i.e. `(add-to-list 'load-path "~/.emacs.d/prettier-eslint-emacs")`
3. Require the package.
  - `(require 'prettier-eslint)`

Then you can hook to your favorite javascript mode:

```elisp
(add-hook 'rjsx-mode-hook 'prettier-eslint-mode)
(add-hook 'js2-mode-hook 'prettier-eslint-mode)
...
```

### prettier-eslint arguments

To adjust the CLI args used for the prettier-eslint command, you can customize the `prettier-eslint-args` variable:

```elisp
(setq prettier-eslint-args '(
  "--trailing-comma" "all"
  "--bracket-spacing" "false"
))
```

### Usage with web-mode

Web-mode is a popular mode for editing .js and .jsx files, but it is used to edit other template files too. If you want to hook prettier-eslint to web-mode for .js and .jsx files only, you can define a helper function like this:

```elisp
(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
      (funcall (cdr my-pair)))))
```

And then hook to web-mode like this:

```elisp
(add-hook 'web-mode-hook #'(lambda ()
                            (enable-minor-mode
                             '("\\.jsx?\\'" . prettier-eslint-mode))))
```

## Customization

This package is customizable via custom.el:

```
M-x customize-group prettier-eslint
```

* `prettier-eslint-args` are the args passed to the prettier-eslint command
* `prettier-eslint-show-errors` customizes where to display the error output (buffer, echo or nil)
