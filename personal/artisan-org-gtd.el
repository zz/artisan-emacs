;;; artisan-org-gtd.el - Provide GTD related settings for org-mdoe

;;Code:
(defvar artisan-org-gtd-dir nil
  "Artisan org-mode gtd files root dir")

(defvar artisan-gtd-file-names
  '("01@inbox.org" "02@gtd.org")
  "All files which used to structure my GTD")

(defun artisan-gtd-agenda-files ()
  (let ((files))
    (if artisan-org-gtd-dir
        (dolist (fname artisan-gtd-file-names)
          (add-to-list 'files (expand-file-name fname
                                                artisan-org-gtd-dir))))
    (or files artisan-gtd-file-names)))

(setq org-agenda-files (artisan-gtd-agenda-files))

(setq org-agenda-ndays 7)
(setq calendar-week-start-day 1)
(setq org-agenda-show-all-dates t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-sorting-strategy 
      (quote ((agenda time-up priority-down tag-up) (todo tag-up))))
(setq org-agenda-start-on-weekday nil)
;;(setq org-agenda-todo-ignore-deadlines t)
;;(setq org-agenda-todo-ignore-scheduled t)
;;(setq org-agenda-todo-ignore-with-date t)

(setq org-deadline-warning-days 7)
(setq org-refile-targets '((org-agenda-files :level . 1)))

(provide 'artisan-org-gtd)

;;; artisan-org-gtd.el ends here
