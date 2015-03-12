;;; jtmoulia-config.el --- Blog configuration

;;; Commentary:
;; Config for the 'org-page blog

;;; Code:

(require 'org-page)


(setq op/repository-directory (file-name-directory
                               (file-truename buffer-file-name))
      op/site-domain "http://jtmoulia.pocketknife.io"
      op/repository-org-branch "source"
      op/repository-html-branch "master"
      op/site-main-title "jtmoulia"
      op/site-sub-title "public notebook 📓"
      op/personal-github-link "http://github.com/jtmoulia"
      op/theme 'mdo
      op/category-config-alist '(("blog" ;; this is the default configuration
                                  :show-meta t
                                  :show-comment t
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
                                  :category-index nil)))

(provide 'jtmoulia-config)
;;; jtmoulia-config.el ends here
