;;; artisan-autocomplete.el - Auto-complete packages settings

;;Code:
(artisan-require-package 'auto-complete)

;; load default auto complete settings
(require 'auto-complete-config)

(add-to-list 'ac-dictionary-directories
             (expand-file-name "dict"
                               (artisan-package-install-dir 'auto-complete)))
(setq-default ac-sources
              '(ac-source-abbrev
                ac-source-dictionary
                ac-source-words-in-buffer
                ac-source-words-in-same-mode-buffers
                ac-source-semantic))

(setq ac-auto-show-menu 0.5)


(provide 'artisan-autocomplete)

;;; artisan-autocomplete.el ends here


