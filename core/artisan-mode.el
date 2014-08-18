;;; artisan-mode.el - Artisan-mode define key maps
;;Code:

(defvar artisan-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c i") 'artisan-insert-current-file-name-here)
    (define-key map (kbd "<f8>") 'artisan-start-org-capture)
    (define-key map (kbd "C-c a a") 'artisan-org-agenda-view)
    (define-key map (kbd "C-c a t") 'artisan-org-todo-view)
    map)
  "Keymap for Artisan mode.")

(define-minor-mode artisan-mode
  "Minor mode to add Emacs Artisan extensions

\\{artisan-mode-map}"
  :lighter " Art"
  :keymap artisan-mode-map)

(define-globalized-minor-mode artisan-global-mode artisan-mode artisan-on)

(defun artisan-on ()
  "Turn on `artisan-mode'"
  (artisan-mode +1))

(defun artisan-off ()
  "Turn off `artisan-mode'"
  (artisan-mode -1))

(provide 'artisan-mode)
;;; artisan-mode.el ends here
