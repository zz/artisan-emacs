;;; Site configuration for org-mode 
;;;
(require 'ox-publish)

(defvar org-blog-base-directory
  "D:/Documents/Dropbox/Dropbox/01@documents/013@Blog"
  "Base directory of the blog source files")

(defvar org-blog-publish-directory
  "D:/personal/github/blog"
  "Publish directory")

(defvar org-blog-site-name "Tony"
  "Name of your blog")

(defvar org-blog-static-directory
  "D:/Documents/Dropbox/Dropbox/01@documents/013@Blog/static"
  "The root directory of your site static files.")

(defvar org-blog-static-publish-directory
  "D:/personal/github/blog/static"
  "publish directory of static files")

(defvar org-blog-html-preamble
  "
<div class=\"site-nav\">
  <a href=\"/\">HOME</a>
</div>
"
  "Html preamble of the generated fils")

(defvar org-blog-html-postamble
  "
<div id=\"ds-thread\" class=\"ds-thread\"></div>

<div class=\"contact\">
  <p>
    Github: <br />
    Google+: <br />
  </p>
</div>
<div class=\"contact\">
  <p>
    <a href=\"https://github.com/jsuper\">github.com/jsuper</a><br />
    <a href=\"https://plus.google.com/+LingTang\">+Ling Tang</a><br />
  </p>
</div>
"
  "Html postamble of the generated files")

(defvar org-blog-common-style-link
  "
<link rel=\"stylesheet\" href=\"/static/style/main.css\" type=\"text/css\"/>
<script type=\"text/javascript\" src=\"/static/js/jquery-1.11.2.min.js\"></script>
<script type=\"text/javascript\" src=\"/static/js/common.js\"></script>"
  "Stylesheet files of your blog")

(defun org-publish-org-custom-sitemap (project &optional sitemap-filename)
  "Custom sitemap generate function, by hooking it, we generate an index file 
for our blog. This function will call default publish function `org-publish-org-sitemap'"
  (let* ((project-plist (cdr project))
         (dir (file-name-as-directory
               (plist-get project-plist :base-directory)))
         (basedir (file-name-directory dir))
         (exclude-regexp (plist-get project-plist :exclude))
         (files (nreverse
                 (org-publish-get-base-files project exclude-regexp)))
         (sitemap-filename (concat dir (or sitemap-filename "sitemap.org")))
         (site-title (or (plist-get project-plist :site-title)
                         (concat "X's " (car project))))
         (index-filename (concat dir "index.org"))
         (sitemap-sans-extension
          (plist-get project-plist :sitemap-sans-extension))
         (visiting (find-buffer-visiting index-filename))
         (ifn (file-name-nondirectory sitemap-filename))
         file sitemap-buffer)
    (with-current-buffer
        (let ((org-inhibit-startup t))
          (setq sitemap-buffer
                (or visiting (find-file index-filename))))
      (erase-buffer)
      (insert (concat "#+TITLE:" site-title "\n\n"))
      (while (setq file (pop files))
        (let ((link (file-relative-name file dir))
              (tname (file-truename file)))
          (unless (or (equal tname (file-truename sitemap-filename))
                      (equal tname (file-truename index-filename)))
            (let ((entry
		   (org-publish-format-file-entry
		    org-publish-sitemap-file-entry-format file project-plist))
                  (fdate (format-time-string "%Y-%m-%d" (org-publish-find-date file))))
              (insert (concat "+ " fdate " » "
                              "[[file:" link "][" entry "]]\n"))))))
      (save-buffer))
    (or visiting (kill-buffer sitemap-buffer)))
  (org-publish-org-sitemap project sitemap-filename))

(defun creat-publish-project-alist (base-directory
                                    publish-directory
                                    html-preamble
                                    html-postamble
                                    style-link
                                    site-name
                                    static-file-directory
                                    static-file-out)
  "Create custom publish project alist for org-mode 
`base-directory' Root directory of blog source file
`publish-directory' Output root directory of blog
`html-preamble' html preamble
"
  `(("blog"
     :components ("blog-content" "blog-static"))
    ("blog-content"
     :base-directory ,base-directory
     :base-extension "org"
     :publishing-directory ,publish-directory
     :recursive t
     :publishing-function  org-html-publish-to-html
     :export-with-tags nil
     :headline-levels 4             ; Just the default for this project.
     :table-of-contents nil
     :section-numbers nil
     :sub-superscript nil
     :todo-keywords nil
     :author nil
     :with-toc nil ;do not export toc
     :creator-info nil
     :html-preamble ,html-preamble
     :html-postamble ,html-postamble
     :html-head ,style-link
     :timestamp t
     :exclude-tags ("noexport" "todo")
     :auto-preamble t
     :auto-sitemap t
     :site-title ,site-name
     :sitemap-filename "sitemap.org"
     :sitemap-sort-files anti-chronologically
     :sitemap-function org-publish-org-custom-sitemap
     :sitemap-sans-extension t
     :sitemap-sort-folders last
     :sitemap-title "Sitemap")
    ("blog-static"
     :base-directory ,static-file-directory
     :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|otf"
     :publishing-directory ,static-file-out
     :recursive t
     :publishing-function org-publish-attachment)))

(dolist (proj (creat-publish-project-alist org-blog-base-directory
                                           org-blog-publish-directory
                                           org-blog-html-preamble
                                           org-blog-html-postamble
                                           org-blog-common-style-link
                                           org-blog-site-name
                                           org-blog-static-directory
                                           org-blog-static-publish-directory))
  (add-to-list 'org-publish-project-alist proj))

(provide 'artisan-org-blog)
