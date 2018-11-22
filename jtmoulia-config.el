;;; jtmoulia-config.el --- Blog configuration

;;; Commentary:
;; Config for the 'org-page blog
;;
;; This config depends on `my-op', a custom project manager for org-page which
;; is in my spacemacs config. Happily, no one will likely ever run this but me.

;;; Code:

(require 'org-page)
(require 'ox-publish)
(require 'ox-html)
(require 'org-element)
(require 'dash)  ;; req'd by org-page anyways

(let* ((root (file-name-directory (file-truename buffer-file-name)))
       (theme-root-directory (file-name-as-directory (concat root "themes"))))
  (add-to-list 'my-op-sites
               `(jtblog .
                        ((org-html-toplevel-hlevel . 4)
                         (org-confirm-babel-evaluate . nil)
                         (op/highlight-render . 'htmlize)
                         (op/repository-directory . ,root)
                         (op/site-domain . "http://jtmoulia.pocketknife.io")
                         (op/repository-org-branch . "source")
                         (op/repository-html-branch . "master")
                         (op/site-main-title . "jtmoulia")
                         (op/site-sub-title . "public notebook 📓")
                         (op/personal-github-link . "http://github.com/jtmoulia")
                         (op/theme-root-directory . ,theme-root-directory)
                         (op/theme . custom)
                         (op/personal-google-analytics-id . "UA-60774978-1")
                         (op/category-ignore-list . ("themes" "assets" "_build"))
                         (op/category-config-alist
                          . (("blog" ;; this is the default configuration
                              :show-meta t
                              :show-comment nil
                              :uri-generator op/generate-uri
                              :uri-template "/blog/%y/%m/%d/%t/"
                              :sort-by :date     ;; how to sort the posts
                              :category-index t) ;; generate category index or not
                             ("index"
                              :show-meta nil
                              :show-comment nil
                              :uri-generator op/generate-uri
                              :uri-template "/"
                              :sort-by :date
                              :category-index nil)
                             ("about"
                              :show-meta nil
                              :show-comment nil
                              :uri-generator op/generate-uri
                              :uri-template "/about/"
                              :sort-by :date
                              :category-index nil)))))))

(with-eval-after-load 'prodigy
  (prodigy-define-service
    :name "hlog@localhost:8020"
    :command "python"
    :args '("-m" "http.server" "8020")
    :cwd "./_build"
    :tags '(file-server)
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t))

(provide 'jtmoulia-config)
;;; jtmoulia-config.el ends here
