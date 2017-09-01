# prettier-eslint for Emacs

prettier-eslint is a function that formats the current buffer using [prettier-eslint](https://github.com/prettier/prettier-eslint). The
package also exports a minor mode that applies `(prettier-eslint)` on save.

## Configuration

### Basic configuration

First require the package:

```elisp
(require 'prettier-eslint)
```

Then you can hook to your favorite javascript mode:

```elisp
(add-hook 'js2-mode-hook 'prettier-eslint-mode)
(add-hook 'web-mode-hook 'prettier-eslint-mode)
...
```

### Prettier arguments

To adjust the CLI args used for the prettier command, you can customize the `prettier-eslint-args` variable:

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
