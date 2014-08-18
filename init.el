;;; Artisan-emacs is inspired by prelude-emacs.
;; The whole directory structure is same with it.

;;Code:
;;current user name
(defvar current-user (user-full-name))

(message "Emacs is starting... Be patient, User is %s" current-user)

(when (version< emacs-version "24.1")
  (error "Required minimal emacs version is 24.1"))

(defvar artisan-dir (file-name-directory load-file-name)
  "The root directory of emacs configuration")

(defvar artisan-core-dir (expand-file-name "core" artisan-dir)
  "The core configuration of artisan-emacs configuration")

(defvar artisan-modules-dir (expand-file-name "modules" artisan-dir)
  "The built-in modules home directory of artisan-emacs")

(defvar artisan-personal-dir (expand-file-name "personal" artisan-dir)
  "This directory is for personal configuration.
All lisp files in this folder will be loaded by artisan-emacs automatically.")

(defvar artisan-vendors-dir (expand-file-name "vendors" artisan-dir)
  "This directory store all third party plugins which is not managed by `el-get' 
or `elpa' package manager")

(defvar artisan-savefile-dir (expand-file-name "savefile" artisan-dir)
  "This folder is used to store all auto-save files.")

;;Create save file directory when it doesn't exist
(unless (file-exists-p artisan-savefile-dir)
  (make-directory artisan-savefile-dir))

(defun artisan-add-subfolders-to-load-path (parent-dir)
 "Add all level PARENT-DIR subdirs to the `load-path'."
 (dolist (f (directory-files parent-dir))
   (let ((name (expand-file-name f parent-dir)))
     (when (and (file-directory-p name)
                (not (equal f ".."))
                (not (equal f ".")))
       (add-to-list 'load-path name)
       (artisan-add-subfolders-to-load-path name)))))

;; add Artisan's directories to Emacs's `load-path'
(add-to-list 'load-path artisan-core-dir)
(add-to-list 'load-path artisan-modules-dir)
(add-to-list 'load-path artisan-vendors-dir)
(add-to-list 'load-path artisan-personal-dir)
(artisan-add-subfolders-to-load-path artisan-vendors-dir)

(require 'artisan-packages)
(require 'artisan-ui)
(require 'artisan-core)
(require 'artisan-editor)
(require 'artisan-modules)
(require 'artisan-global-keybinding)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;;; load personal file per user setting
;;; we don't recommend you add more setttings in this direcotry
;;; you should use `artisan-modules.el' strategy
;;;(when (file-exists-p artisan-personal-dir)
;;;    (message "Loading personal configuration files in %s..." artisan-personal-dir)
;;;  (mapc 'load (directory-files artisan-personal-dir 't "^[^#].*el$")))

(message "Artisan emacs initialized")

;;; init.el ends here
