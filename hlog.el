(require 'org)
(require 'ox-publish)
(require 'ox-html)
(require 'org-element)
(require 'ox-rss)

(defvar hlog--directory default-directory)

(defvar hlog-default-uri-template "/blog/%y/%m/%d/%t")

(defun hlog//prepare (project-plist)
    "Touch `index.org' to rebuild it.
     Argument `PROJECT-PLIST' contains information about the current project."
    (let* ((base-directory (plist-get project-plist :base-directory))
           (buffer (find-file-noselect (expand-file-name "index.org" base-directory) t)))
      (with-current-buffer buffer
        (set-buffer-modified-p t)
        (save-buffer 0))
      (kill-buffer buffer)))

(defun hlog//get-string-from-file (file-path)
  "Return file-path's file content."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file-path hlog--directory))
    (buffer-string)))

(defvar hlog-head (hlog//get-string-from-file "./templates/head.html"))

(defun hlog//preamble (_plist)
  "Pre-amble for jtmoulia.github.com"
  (hlog//get-string-from-file "./templates/preamble.html"))

(defun hlog//postamble (_plist)
  "Post-amble for whole blog."
  (hlog//get-string-from-file "./templates/postamble.html"))

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

(defun hlog//encode-string-to-url (string)
  "Encode STRING to legal URL. Why we do not use `url-encode-url' to encode the
string, is that `url-encode-url' will convert all not allowed characters into
encoded ones, like %3E, but we do NOT want this kind of url.

Shamelessly lifted from https://github.com/kelvinh/org-page"
  (downcase (replace-regexp-in-string "[ .,:;/\\]+" "-" (replace-regexp-in-string "[.!?'\"]" "" (replace-regexp-in-string "[.!?]+$" "" string)))))

(defun hlog//read-org-option (option)
  "Read option value of org file opened in current buffer.
e.g:
#+TITLE: this is title
will return \"this is title\" if OPTION is \"TITLE\"

Shamelessly lifted from https://github.com/kelvinh/org-page"
  (let ((match-regexp (org-make-options-regexp `(,option))))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward match-regexp nil t)
        (match-string-no-properties 2 nil)))))

(defun hlog//read-org-option-from-file (filename option)
  "Read OPTION value of org file from FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (hlog//read-org-option option)))

(defun hlog//fix-timestamp-string (date-string)
  "This is a piece of code copied from Xah Lee (I modified a little):
Returns yyyy-mm-dd format of date-string
For examples:
   [Nov. 28, 1994]     => [1994-11-28]
   [November 28, 1994] => [1994-11-28]
   [11/28/1994]        => [1994-11-28]
Any \"day of week\", or \"time\" info, or any other parts of the string, are
discarded.
Code detail: URL `http://xahlee.org/emacs/elisp_parse_time.html'"
  (let ((date-str date-string)
        date-list year month date yyyy mm dd)
    (setq date-str (replace-regexp-in-string "^ *\\(.+\\) *$" "\\1" date-str))
    (cond
     ;; USA convention of mm/dd/yyyy
     ((string-match
       "^\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9][0-9][0-9]\\)$"
       date-str)
      (concat (match-string 3 date-str) "-" (match-string 1 date-str) "-"
              (match-string 2 date-str)))
     ((string-match
       "^\\([0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9][0-9][0-9]\\)$"
       date-str)
      (concat (match-string 3 date-str) "-" (match-string 1 date-str) "-"
              (match-string 2 date-str)))
     ;; some ISO 8601. yyyy-mm-dd
     ((string-match
       "^\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)$\
T[0-9][0-9]:[0-9][0-9]" date-str)
      (concat (match-string 1 date-str) "-" (match-string 2 date-str) "-"
              (match-string 3 date-str)))
     ((string-match
       "^\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)$"
       date-str)
      (concat (match-string 1 date-str) "-" (match-string 2 date-str) "-"
              (match-string 3 date-str)))
     ((string-match "^\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)$" date-str)
      (concat (match-string 1 date-str) "-" (match-string 2 date-str)))
     ((string-match "^\\([0-9][0-9][0-9][0-9]\\)$" date-str)
      (match-string 1 date-str))
     (t (progn
          (setq date-str
                (replace-regexp-in-string "January " "Jan. " date-str))
          (setq date-str
                (replace-regexp-in-string "February " "Feb. " date-str))
          (setq date-str
                (replace-regexp-in-string "March " "Mar. " date-str))
          (setq date-str
                (replace-regexp-in-string "April " "Apr. " date-str))
          (setq date-str
                (replace-regexp-in-string "May " "May. " date-str))
          (setq date-str
                (replace-regexp-in-string "June " "Jun. " date-str))
          (setq date-str
                (replace-regexp-in-string "July " "Jul. " date-str))
          (setq date-str
                (replace-regexp-in-string "August " "Aug. " date-str))
          (setq date-str
                (replace-regexp-in-string "September " "Sep. " date-str))
          (setq date-str
                (replace-regexp-in-string "October " "Oct. " date-str))
          (setq date-str
                (replace-regexp-in-string "November " "Nov. " date-str))
          (setq date-str
                (replace-regexp-in-string "December " "Dec. " date-str))
          (setq date-str
                (replace-regexp-in-string " 1st," " 1" date-str))
          (setq date-str
                (replace-regexp-in-string " 2nd," " 2" date-str))
          (setq date-str
                (replace-regexp-in-string " 3rd," " 3" date-str))
          (setq date-str
                (replace-regexp-in-string "\\([0-9]\\)th," "\\1" date-str))
          (setq date-str
                (replace-regexp-in-string " 1st " " 1 " date-str))
          (setq date-str
                (replace-regexp-in-string " 2nd " " 2 " date-str))
          (setq date-str
                (replace-regexp-in-string " 3rd " " 3 " date-str))
          (setq date-str
                (replace-regexp-in-string "\\([0-9]\\)th " "\\1 " date-str))
          (setq date-list (parse-time-string date-str))
          (setq year (nth 5 date-list))
          (setq month (nth 4 date-list))
          (setq date (nth 3 date-list))
          (setq yyyy (number-to-string year))
          (setq mm (if month (format "%02d" month) ""))
          (setq dd (if date (format "%02d" date) ""))
          (concat yyyy "-" mm "-" dd))))))

(cl-defun hlog//generate-uri (&optional uri-template creation-date title)
  "Generate URI of org file opened in current buffer. It will be firstly created
by #+URI option, if it is nil, DEFAULT-URI-TEMPLATE will be used to generate the
uri. If CREATION-DATE is nil, current date will be used. The uri template option
can contain following parameters:
%y: year of creation date
%m: month of creation date
%d: day of creation date
%f: base file name with suffix .html (a.org->a.html)
%t: title of current buffer

  Shamelessly lifted from https://github.com/kelvinh/org-page
  ... if only he would keep it updated"
  (debug)
  (message "22345 inhlog! %s %s" (hlog//read-org-option "URI") (hlog//read-org-option "DATE"))
  (let ((uri-template (or uri-template
                          (hlog//read-org-option "URI")
                          hlog-default-uri-template))
        (date-list (split-string (hlog//fix-timestamp-string (or creation-date
                                                                 (hlog//read-org-option "DATE")))
                                 "-"))
        (html-file-name (concat (file-name-base (buffer-file-name)) ".html"))
        (encoded-title (hlog//encode-string-to-url (or title
                                                       (hlog//read-org-option "TITLE")))))
    (message "32345")
    (format-spec uri-template `((?y . ,(car date-list))
                                (?m . ,(cadr date-list))
                                (?d . ,(cl-caddr date-list))
                                (?f . ,html-file-name)
                                (?t . ,encoded-title)))))

(defun hlog//publish-to-html (plist filename pub-dir)
  "Same as `org-html-publish-to-html' but modifies html before finishing."
  (let* ((expanded-pub-dir (expand-file-name pub-dir (hlog//generate-uri)))
         (file-path (org-html-publish-to-html plist filename expanded-pub-dir)))
    (message "1234 at")
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
    (message "WUH %s" file-path)
    file-path))

(defvar hlog-root-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "The hlog root directory")

(defvar hlog-base-directory
  (concat (file-name-as-directory hlog-root-directory) "src")
  "The hlog src directory")

(defvar hlog-publishing-directory
  (concat (file-name-directory hlog-root-directory) "dist")
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
