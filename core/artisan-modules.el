;;; artisan-modules.el - Automatically load module after related mode is 
;;;                      activated

;; Emacs will be slow when it require more and more features during initialize.
;; So how to make it be quickly is a problem.
;; As we know, when Emacs not loaded extra settings, it will start very quickly,
;; so why don't we let it to lazy loaded all personal settings?
;; 
;; As we know, many custom configurations are narrowed to specified modes, e.g.
;; all settings about `org-mode' is only related with this `major-mode'. Can we 
;; load and activate all user settings about `org-mode' after it was activated?
;; The answer is yes. This setting is going to do it.
;;
;; How do I do it?
;; We have organized all personal settings to module folder. we also provide a 
;; configurable alist to end-user so that they can configure mode initialize file
;; , see `artisan-mode-and-module-alist'. And we add new hook for 
;; `after-change-major-mode-hook' so that we can load unloaded user settings when
;; specified `major-mode' is activated. 

;; Code:
(defvar artisan-personal-module 
  (expand-file-name "artisan-p-modules.el" artisan-personal-dir)
  "store personal load module")

(defvar artisan-module-initialize-state
  (make-hash-table :test 'equal)
  "Global hash table, which store each module initialized state,
so that we can only load module one time")

(defvar artisan-mode-and-module-alist
  '((emacs-lisp-mode . artisan-emacs-lisp)
    (org-mode . artisan-org)
    (python-mode . artisan-python))
  "The initialize module file for `major-mode'")

(defun artisan-get-module-state (mode)
  "Get the initialize state of MODE(see `major-mode').
 Which state is stored in `artisan-module-initialize-state'"
  (gethash mode artisan-module-initialize-state))

(defun artisan-set-module-state (mode)
  "Set MODE(see `major-mode') initialized state to loaded"
  (puthash mode t artisan-module-initialize-state))

(defun artisan-get-mode-module-alist (mode)
  "Get the mode and module alist from `artisan-mode-and-module-alist'"
  (assoc mode artisan-mode-and-module-alist))

(defun artisan-load-mode-setting (mode)
  "Load settings of current `major-mode'"
  (unless (artisan-get-module-state mode)
    (artisan-set-module-state mode)
    (let* ((module-alist (artisan-get-mode-module-alist mode))
          (module-name (when module-alist (cdr module-alist))))
      (when (and module-name
                 (require module-name nil 'noerror))
        (message "Artisan loaded [%s]" module-name)
        t))))

(defun artisan-major-mode-change-hook ()
  "Hook for `major-mode' change. After the `major-mode' of `current-buffer'
has changed. Then load its initialized module file if necessary.
Its initialized module was configured in `artisan-mode-and-module-alist'"
  (with-current-buffer (current-buffer)
    (unless (artisan-get-module-state major-mode)
      (let* ((mode-hook-symbol (intern (format "%s-hook" major-mode))))
        (when (and (artisan-load-mode-setting major-mode)
                   mode-hook-symbol)
          (run-hooks mode-hook-symbol))))))

;;; install hook for `major-mode' change
(add-hook 'after-change-major-mode-hook 'artisan-major-mode-change-hook)

;;; load personal modules if it exists
(when (file-exists-p artisan-personal-module)
  (load-file artisan-personal-module))

(provide 'artisan-modules)

;;; artisan-modules.el ends here
