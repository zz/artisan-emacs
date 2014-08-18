;;; artisan-editor.el --- Bootstrap settings for core editor

;; Death to the tabs!  However, tabs historically indent to the next
;; 8-character offset; specifying anything else will cause *mass*
;; confusion, as it will change the appearance of every existing file.
;; In some cases (python), even worse -- it will change the semantics
;; (meaning) of the program.
;;
;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance

;; delete the selection with a keypress
(delete-selection-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;;; smart tab behavior - indent or complete
(setq table-always-indent 'complete)

;; disable annoying blink-matching-paren
(setq blink-matching-paren nil)

;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;;; distinguish different buffer with same file name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; enable erase-buffer command
(put 'erase-buffer 'disabled nil)

;;; bookmark setting
(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks" artisan-savefile-dir)
      bookmark-save-flag 1)

;;; enable for ido-mode
(require 'ido)
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length 0)
(setq ido-use-virtual-buffers t)

;;; encoding setting
(set-language-environment "UTF-8")
;;(setq-default locale-coding-system 'utf-8)
(setq-default default-terminal-coding-system 'utf-8)
;;(setq-default default-file-name-coding-system 'utf-8)
;;(setq-default default-buffer-file-coding-system 'utf-8)
;;; setting locale and coding
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'gb18030)
(prefer-coding-system 'utf-8)
;;; Enable CJK text paste from clipbord
(set-clipboard-coding-system 'euc-cn)

(require 'artisan-mode)
(artisan-global-mode +1)

(provide 'artisan-editor)

;;; artisan-editor.el ends here
