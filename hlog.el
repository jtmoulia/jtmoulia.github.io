(require 'org)
(require 'ox-publish)
(require 'ox-html)
(require 'org-element)
(require 'ox-rss)

(defun hlog//prepare (project-plist)
    "Touch `index.org' to rebuild it.
     Argument `PROJECT-PLIST' contains information about the current project."
    (let* ((base-directory (plist-get project-plist :base-directory))
           (buffer (find-file-noselect (expand-file-name "index.org" base-directory) t)))
      (with-current-buffer buffer
        (set-buffer-modified-p t)
        (save-buffer 0))
      (kill-buffer buffer)))

(defvar hlog-head
  "<link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/css/bootstrap.css\"/>
  <link rel=\"stylesheet\" type=\"text/css\" href=\"https://fonts.googleapis.com/css?family=Amaranth|Handlee|Libre+Baskerville|Bree+Serif|Ubuntu+Mono|Pacifico&subset=latin,greek\"/>
  <link rel=\"shortcut icon\" type=\"image/x-icon\" href=\"favicon.ico\">")

(defun hlog//preamble (_plist)
  "Pre-amble for jtmoulia.github.com"
  "<div class=\"banner\">
    <a href=\"/\"> Ramblings from a Corner </a>
  </div>
  <ul class=\"banner-links\">
    <li><a href=\"/\"> About Me </a> </li>
    <li><a href=\"/archive.html\"> Posts </a> </li>
  </ul>
  <hr>")

(defun hlog//postamble (plist)
  "Post-amble for whole blog."
  (concat
   "<footer class=\"footer\">
      <!-- Footer Definition -->
      FOOOTER
   </footer>"))

(defun hlog//sitemap-format-entry (entry _style project)
  "Return string for each ENTRY in PROJECT."
  (when (s-starts-with-p "posts/" entry)
    (format "@@html:<span class=\"archive-item\"><span class=\"archive-date\">@@ %s @@html:</span>@@ [[file:%s][%s]] @@html:</span>@@"
            (format-time-string "%h %d, %Y"
                                (org-publish-find-date entry project))
            entry
            (org-publish-find-title entry project))))

(defun hlog//sitemap-function (title list)
  "Return sitemap using TITLE and LIST returned by `org-blog-sitemap-format-entry'."
  (concat "#+TITLE: " title "\n\n"
          "\n#+begin_archive\n"
          (mapconcat (lambda (li)
                       (format "@@html:<li>@@ %s @@html:</li>@@" (car li)))
                     (seq-filter #'car (cdr list))
                     "\n")
          "\n#+end_archive\n"))

(defun hlog//publish-to-html (plist filename pub-dir)
  "Same as `org-html-publish-to-html' but modifies html before finishing."
  (let ((file-path (org-html-publish-to-html plist filename pub-dir)))
    (with-current-buffer (find-file-noselect file-path)
      (goto-char (point-min))
      (search-forward "<body>")
      (insert (concat "\n<div class=\"content-wrapper container\">\n "
                      "  <div class=\"row\"> <div class=\"col\"> </div> "
                      "  <div class=\"col-sm-6 col-md-8\"> "))
      (goto-char (point-max))
      (search-backward "</body>")
      (insert "\n</div>\n<div class=\"col\"></div></div>\n</div>\n")
      (save-buffer)
      (kill-buffer))
    file-path))

(defvar hlog-root-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "The hlog root directory")

(defvar hlog-base-directory
  (concat (file-name-as-directory hlog-root-directory) "src")
  "The hlog src directory")

(defvar hlog-publishing-directory
  (concat (file-name-directory hlog-root-directory) "build")
  "The hlog publishing directory")

(defvar hlog-assets-base-directory
  (concat (file-name-as-directory hlog-base-directory) "assets")
  "The jtmoulia-glob assets src directory")

(defvar hlog-assets-publishing-directory
  (concat (file-name-as-directory hlog-publishing-directory) "assets")
  "The jtmoulia-glob assets publishing directory")

(defvar hlog-rss-publishing-directory
  (concat (file-name-as-directory hlog-publishing-directory) "rss")
  "The jtmoulia-glob rss publishing directory")

(setq org-publish-project-alist
      `(("orgfiles"
         :base-directory ,hlog-base-directory
         :exclude ".*drafts/.*"
         :base-extension "org"
         :publishing-directory ,hlog-publishing-directory

         :recursive t
         :preparation-function hlog//prepare
         :publishing-function hlog//publish-to-html

         :with-toc nil
         :with-title t
         :with-date t
         :section-numbers nil
         :html-doctype "html5"
         :html-html5-fancy t
         :html-head-include-default-style nil
         :html-head-include-scripts nil
         :htmlized-source t
         :html-head-extra ,hlog-head
         :html-preamble hlog//preamble
         :html-postamble hlog//postamble

         :auto-sitemap t
         :sitemap-filename "archive.org"
         :sitemap-title "Posts"
         :sitemap-style list
         :sitemap-sort-files anti-chronologically
         :sitemap-format-entry hlog//sitemap-format-entry
         :sitemap-function hlog//sitemap-function)

        ("assets"
         :base-directory ,hlog-assets-base-directory
         :base-extension ".*"
         :publishing-directory ,hlog-assets-publishing-directory
         :publishing-function org-publish-attachment
         :recursive t)

        ("rss"
         :base-directory ,hlog-base-directory
         :base-extension "org"
         :html-link-home "http://jtmoulia.pocketknife.io/"
         :html-link-use-abs-url t
         :rss-extension "xml"
         :publishing-directory ,hlog-publishing-directory
         :publishing-function org-rss-publish-to-rss
         :exclude ".*"
         :include ("archive.org")
         :section-numbers nil
         :table-of-contents nil)

        ("hlog" :components ("orgfiles" "assets" "rss"))))

(with-eval-after-load 'prodigy
  (prodigy-define-service
    :name "hlog@localhost:8020"
    :command "python2"
    :args '("-m" "SimpleHTTPServer" "8020")
    :cwd hlog-publishing-directory
    :tags '(file-server)
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t))
