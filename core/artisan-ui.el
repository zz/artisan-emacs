;;; UI setting for artisan emacs

;; Avaiable font list pair, can work correctly in org-mode
(defvar artisan-emacs-font-list 
  '((("Consolas" 10) ;;en font name and size
     ("Microsoft YaHei" 14)) ;; han font name and size
    (("DejaVu Sans Mono" 9) 
     ("WenQuanYi Micro Hei" 16)))
  "Emacs font list: en font and han font pair")

(defun artisan-font-existp (font-name)
  "check the given font-name is installed in curren system"
  (interactive)
  (if (null (condition-case nil
                (x-list-fonts font-name)
              (error nil)))
      nil t))

(defun artisan-font-configure ()
  "configure the emacs fonts"
  (catch 'loop
    (dolist (font-pair artisan-emacs-font-list)
      (let* ((en-font-list (car font-pair))
             (han-font-list (first (cdr font-pair)))
             (en-font-name (car en-font-list))
             (en-font-size (first (cdr en-font-list)))
             (han-font-name (car han-font-list))
             (han-font-size (first (cdr han-font-list))))
        (if (and (artisan-font-existp en-font-name)
             (artisan-font-existp han-font-name))
        (progn 
          (set-face-attribute 'default nil :font 
                              (format "%s %d" en-font-name en-font-size))
          (dolist (charset '(kana han symbol cjk-misc bopomofo))
            (set-fontset-font t
                              charset
                              (font-spec :family han-font-name :size han-font-size)))
          (throw 'loop t)))))))

;;; hidden toolbar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;;; hidden menubar
(menu-bar-mode -1)

;;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable startup screen
(setq inhibit-startup-screen t)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; make the fringe (gutter) smaller
;; the argument is a width in pixels (the default is 8)
(if (fboundp 'fringe-mode)
    (fringe-mode 4))

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;;; highlight current line
(global-hi-lock-mode 1)

;;; echo keystrokes quickly 
(setq echo-keystrokes 0.1)

;;enable visualization of matching parens
;;(show-paren-mode t)

;;; hide scroll bar
(scroll-lock-mode t)
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(require 'diminish)
(diminish 'scroll-lock-mode)
(scroll-bar-mode -1)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;;; noisy ring bell
(setq ring-bell-function 'ignore)

;;; usefull frame title
(setq frame-title-format
      '("" current-user " Artisan - " 
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;;; configure emacs font
(artisan-font-configure)

;;; load tango theme
(load-theme 'zenburn t)
;;; (load-theme 'tangotango t)

(provide 'artisan-ui)
;;; artisan-ui.el ends here
