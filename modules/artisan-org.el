;; artisan-org.el - Provide org-mode settings

;; Code:

(require 'iimage)
(require 'ox-latex)
(require 'ox-html)
(require 'ox-md)
(require 'artisan-org-blog)
;;; stock org-mode need the latest htmlize version > 1.3
(artisan-require-package 'htmlize)

(defvar artisan-org-local-setting-file (expand-file-name 
                                        ".org.el"
                                        artisan-savefile-dir)
  "Define the machine independent settings for org-mode")

(defadvice org-html-paragraph (before fsh-org-html-paragraph-advice 
                                      (paragraph contents info) activate) 
  "Join consecutive Chinese lines into a single long line without 
unwanted space when exporting org-mode to html." 
  (let ((orig-contents (ad-get-arg 1)) 
        (reg-han "[[:multibyte:]]"))
    (ad-set-arg 1 (replace-regexp-in-string 
                   (concat "\\(" reg-han "\\) *\n *\\(" reg-han "\\)") 
                   "\\1\\2" orig-contents)))) 

(defun artisan-make-org-latex-processor (latex &optional outdir)
  "Make latex processor for `org-mode', if `outdir' specified.
Then the ouput file will output to that folder"
  (format "%s -shell-escape -interaction nonstopmode -output-directory %s %%f"
          latex (or outdir "%o")))

(defun artisan-org-defaults ()
;;  (org-indent-mode t)
  (iimage-mode)
  (turn-on-auto-fill)
  (setq truncate-lines nil)
  (require 'org-jekyll-mode)
  (require 'diminish)
  (org-jekyll-mode +1)
  (diminish 'iimage-mode))

;; load default machine-independent settings
(when (file-exists-p artisan-org-local-setting-file)
  (load-file artisan-org-local-setting-file))

;; loading gtd files
(require 'artisan-org-gtd)

;;let org-mode highlight the source code
(setq org-src-fontify-natively t)

;; define the org-mode capture template
(setq org-capture-templates
      '(("n" "New" entry (file (expand-file-name "01@inbox.org"
                                                 artisan-org-gtd-dir)))
        ("t" "Task" entry (file (expand-file-name "01@inbox.org"
                                                  artisan-org-gtd-dir))
         "* TODO %?\n %i\n")
        ("c" "Calendar" entry (file (expand-file-name "01@inbox.org"
                                                      artisan-org-gtd-dir)))
        ("i" "Idea" entry (file (expand-file-name "01@inbox.org"
                                                  artisan-org-gtd-dir)))
        ("n" "Note" entry (file (expand-file-name "01@inbox.org"
                                                  artisan-org-gtd-dir)))))

;; org-latex related settings
;; highlight code when export to latex
;; you should install pymint. and make sure the bin directory
;; is in your PATH
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

;;; remove inputenc from default latex package alist
;; This is because it will conflict with xeCjk package for CJK 
;; encoding user
(setq org-latex-default-packages-alist 
      (remove '("AUTO" "inputenc" t) 
              org-latex-default-packages-alist))

;; set default latex processor as xelatex
(setq org-latex-pdf-process 
      `(,(artisan-make-org-latex-processor "xelatex")
        ,(artisan-make-org-latex-processor "xelatex")
        ,(artisan-make-org-latex-processor "xelatex")))

(add-hook 'org-mode-hook 'artisan-org-defaults)

(provide 'artisan-org)

;; artisan-org.el ends here

