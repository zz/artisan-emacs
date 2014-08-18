;;; artisan-emacs-lisp.el - Provide emacs lisp programming settings
;;; Code:
(require 'artisan-lisp)

(defun artisan-emacs-lisp-mode-defaults ()
  (artisan-lisp-coding-defaults)
  (setq mode-name "EL"))

(add-hook 'emacs-lisp-mode-hook 'artisan-emacs-lisp-mode-defaults)

(provide 'artisan-emacs-lisp)
;;; artisan-emacs-lisp.el ends here
