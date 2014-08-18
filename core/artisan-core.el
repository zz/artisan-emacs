;;; artisan-core.el --- Provides core functions that used by whole package

;;; artisan core functions
;;; Code:
(defun artisan-insert-current-file-name-here ()
  "Insert current `buffer-file-name' at here."
  (interactive)
  (if buffer-file-name
    (insert (file-name-nondirectory (buffer-file-name))))
  nil)

(defun artisan-eval-after-init (form)
  "Add `(lambda () FORM)' to `after-init-hook'.

    If Emacs has already finished initialization, also eval FORM immediately."
  (let ((func (list 'lambda nil form)))
    (add-hook 'after-init-hook func)
    (when after-init-time
      (eval form))))

(defun artisan-module-load-check ()
  (unless (fboundp 'artisan-load-mode-setting)
    ;;need to require artisan-module so that use its method
    (require 'artisan-modules nil 'noerror)))

(defun artisan-org-mode-default ()
  "Default for `org-mode', should load custome `org-mode' settings
files if exists"
  (artisan-module-load-check)
  (artisan-load-mode-setting 'org-mode))

(defun artisan-start-org-capture ()
  "Start `org-capture' help function. Will require `org-capture' module
if necessary"
  (interactive)
  (artisan-org-mode-default)
  (org-capture))

(defun artisan-org-agenda-view ()
  "Start `org-mode' agenda view"
  (interactive)
  (artisan-org-mode-default)
  (org-agenda-list))

(defun artisan-org-todo-view ()
  "Start `org-mode' todo list view"
  (interactive)
  (artisan-org-mode-default)
  (org-todo-list))

(provide 'artisan-core)

;;; artisan-core.el ends here
