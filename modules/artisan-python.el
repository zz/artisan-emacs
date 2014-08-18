;;; artisan-python.el - Provides python programming settings


;;Code:
(require 'artisan-programming)
(require 'artisan-autocomplete)

;; Install core  package for python completion
(artisan-require-package 'jedi)

(defun artisan-python-prog-default ()
  "Python programming default setting"
  (artisan-prog-mode-default-setting)
  (auto-complete-mode +1)
  (setq jedi:setup-keys t)
  (setq jedi:complete-on-dot t)
  (jedi:setup))

(add-hook 'python-mode-hook 'artisan-python-prog-default)

(provide 'artisan-python)

;; artisan-python.el ends here

