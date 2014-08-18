;;; artisan-global-keybinding.el - Re-bind some default keys

;;Code:

(defun artisan-bind-key (kbd func)
  "User `global-set-key' to bind KBD to FUNC"
  (global-set-key (kbd kbd) func))

;; always use ibuffer
(artisan-bind-key "C-x C-b" 'ibuffer)

;; Font size adjust
(artisan-bind-key "C-+" 'text-scale-increase)
(artisan-bind-key "C--" 'text-scale-decrease)

;; Start proced in a similar manar
(artisan-bind-key "C-x p" 'proced)

;; Start eshell or switch to it if it's active
(artisan-bind-key "C-x m" 'eshell)

;; Use hippie-expand instead of dabbrv
(artisan-bind-key "M-/" 'hippie-expand)

(provide 'artisan-global-keybinding)
;;;artisan-global-keybinding.el ends here
