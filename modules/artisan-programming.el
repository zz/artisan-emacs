;;; artisan-programming.el - Provide common settings for programming mode

;;; Code:
(require 'smartparens-config)
(artisan-require-packages '(projectile grizzl))

(defun artisan-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

(defun artisan-font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
          1 font-lock-warning-face t))))

(defun artisan-projectile-and-grizzl-setting ()
  (projectile-mode)
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-completion-system 'grizzl))

(defun artisan-prog-mode-default-setting ()
  "All pogramming mode default setting, see `prog-mode'"
  (smartparens-mode +1)
  (show-smartparens-mode +1) 
  (artisan-local-comment-auto-fill)
  (artisan-font-lock-comment-annotations)
  (artisan-projectile-and-grizzl-setting))

(setq artisan-prog-mode-hook 'artisan-prog-mode-default-setting)

(add-hook 'prog-mode-hook (lambda ()
                            (run-hooks artisan-prog-mode-hook)))


(provide 'artisan-programming)
;;; artisan-programming.el ends here
