;;; artisan-lisp-interaction.el - Provides lisp interaction settings

;;;Code:
;;; require supper module of all lisp programming
(require 'artisan-lisp) 

(defun artisan-interactive-lisp-coding-defaults ()
  (artisan-lisp-coding-defaults)
  (whitespace-mode -1)
  (setq mode-name "LI"))

(add-hook 'lisp-interaction-mode-hook 'artisan-interactive-lisp-coding-defaults)
(artisan-interactive-lisp-coding-defaults)

(provide 'artisan-lisp-interaction)

;;; artisan-lisp-interaction.el ends here
