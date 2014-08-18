;;; package setting for artisan emacs

;;; code start;
(require 'cl)
(require 'package)


(defvar artisan-packages
  '(smartparens zenburn-theme rainbow-mode diminish multiple-cursors)
  "Basic packages for artisan-emacs")

(defvar artisan-auto-install-alist
  '(("\\.clj\\'" clojure-mode clojure-mode)
    ("\\.coffee\\'" coffee-mode coffee-mode)
    ("\\.css\\'" css-mode css-mode)
    ("\\.csv\\'" csv-mode csv-mode)
    ("\\.d\\'" d-mode d-mode)
    ("\\.dart\\'" dart-mode dart-mode)
    ("\\.erl\\'" erlang erlang-mode)
    ("\\.feature\\'" feature-mode feature-mode)
    ("\\.go\\'" go-mode go-mode)
    ("\\.groovy\\'" groovy-mode groovy-mode)
    ("\\.haml\\'" haml-mode haml-mode)
    ("\\.hs\\'" haskell-mode haskell-mode)
    ("\\.latex\\'" auctex LaTeX-mode)
    ("\\.less\\'" less-css-mode less-css-mode)
    ("\\.lua\\'" lua-mode lua-mode)
    ("\\.markdown\\'" markdown-mode markdown-mode)
    ("\\.md\\'" markdown-mode markdown-mode)
    ("\\.ml\\'" tuareg tuareg-mode)
    ("\\.pp\\'" puppet-mode puppet-mode)
    ("\\.php\\'" php-mode php-mode)
    ("PKGBUILD\\'" pkgbuild-mode pkgbuild-mode)
    ("\\.sass\\'" sass-mode sass-mode)
    ("\\.scala\\'" scala-mode2 scala-mode)
    ("\\.scss\\'" scss-mode scss-mode)
    ("\\.slim\\'" slim-mode slim-mode)
    ("\\.textile\\'" textile-mode textile-mode)
    ("\\.yml\\'" yaml-mode yaml-mode)
    ("\\.org\\'" org-plus-contrib org-mode)
    ("\\.bat\\'" dos dos-mode)
    ("\\.js\\'" js2-mode js2-mode))
  "Auto install missing mode when file with given extension is opened")

(defun artisan-package-version (p)
  "Return the version of installed package P"
  (package-version-join 
   (package-desc-vers (cdr (assq p package-alist)))))

(defun artisan-package-install-dir (p)
  "Return the install directory of package P"
  (package--dir (format "%s" p)
                (artisan-package-version p)))

(defun artisan-packages-installed-p ()
  "Check all required `artisan-packages' install or not"
  (every #'package-installed-p artisan-packages))

(defun artisan-require-package (package)
  "Install PACKAGE unless already installed"
  (unless (memq package artisan-packages)
    (add-to-list 'artisan-packages package))
  (unless (package-installed-p package)
    (package-install package))
  (require package))

(defun artisan-require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'artisan-require-package packages))

(defun artisan-install-packages ()
  "Install all packages listed in `artisan-packages'."
  (unless (artisan-packages-installed-p)
    ;; check for new packages (package versions)
    (message "Emacs Artisan is now refreshing its package database...")
    (package-refresh-contents)
    (message "done.")
    ;; install the missing packages
    (artisan-require-packages artisan-packages)))

(defmacro artisan-auto-install (extension package mode)
  "When file with EXTENSION is opened triggers auto-install of PACKAGE.
PACKAGE is installed only if not already present.  The file is opened in MODE."
  `(add-to-list 'auto-mode-alist
                `(,extension . (lambda ()
                                 (unless (package-installed-p ',package)
                                   (package-refresh-contents)
                                   (package-install ',package))
                                 (,mode)))))

;;setting package source for elpa
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; initialize the elpa plugins
(setq package-user-dir (expand-file-name "packages" artisan-dir))
(package-initialize)

;;; Install all packages listed in artisan-packages
(artisan-install-packages)

;; markdown-mode doesn't have autoloads for the auto-mode-alist
;; so we add them manually if it's already installed
(when (package-installed-p 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(when (package-installed-p 'pkgbuild-mode)
  (add-to-list 'auto-mode-alist '("PKGBUILD\\'" . pkgbuild-mode)))

(when (package-installed-p 'dos)
  (add-to-list 'auto-mode-alist '("\\.bat\\'" . dos-mode)))

(when (package-installed-p 'js2-mode)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

;; build auto-install mappings
(mapc
 (lambda (entry)
   (let ((extension (car entry))
         (package (cadr entry))
         (mode (cadr (cdr entry))))
     (unless (package-installed-p package)
       (artisan-auto-install extension package mode))))
 artisan-auto-install-alist)


(provide 'artisan-packages)

;;; artisan-packages.el ends here
