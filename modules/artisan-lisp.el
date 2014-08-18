;;; artisan-lisp.el - Provide common settings for lisp programming

;;; Code:
(require 'artisan-programming)
(artisan-require-package 'rainbow-delimiters)

(defun artisan-lisp-coding-defaults ()
  ;;; see `artisan-programming' module
  (artisan-prog-mode-default-setting)
  ;;; enable smartparens strict mode for lisp programming
  (smartparens-strict-mode +1)
  ;;; enable rainbow-delimiters-mode
  (rainbow-delimiters-mode +1)
  (rainbow-mode +1)
  ;;; turn on emacs lisp doc mode
  (turn-on-eldoc-mode)
  ;;; discard some minor mode lighter
  (require 'diminish)
  (diminish 'rainbow-mode)
  (diminish 'eldoc-mode))

(defun conditionally-enable-smartparens-mode ()
  "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
  (if (eq this-command 'eval-expression)
      (smartparens-mode 1)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-smartparens-mode)

(provide 'artisan-lisp)
;;; artisan-lisp.el ends here
