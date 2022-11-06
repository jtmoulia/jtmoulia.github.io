;;; my-op.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Thomas Moulia
;;
;; Author: Thomas Moulia <jtmoulia@pocketknife.io>
;; Maintainer: Thomas Moulia <jtmoulia@pocketknife.io>
;; Created: November 05, 2022
;; Modified: November 05, 2022
;; Version: 0.0.1
;; Keywords: docs
;; Homepage: https://github.com/jtmoulia/jtmoulia.github.io
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Helper library for organizing a set of org page sites.
;;
;; Note: org-page is currently unsupported, so this approach is a bit of a
;; dead-end.
;;
;;; Code:

(defvar my-op-sites nil
  "The alist of site configurations.")

(defvar my-op-site nil
  "The active site configuration.")

(defvar my-op-site-policy 'my-op/ask-if-nil
  "The policy for selecting `my-op-site'.")

(defvar my-op-force-all t
  "Whether to force rebuilding all files by default when publishing.")

;; Helper Functions

(defun my-op//apply-variables (vars)
  (-each vars
    (lambda (var)
      (let ((name (car var))
            (value (cdr var)))
        (set name value))))
  vars)

(defun my-op//get-vars (names)
  "Return an alist of variable `(name . value)' for NAMES."
  (-map
   (lambda (name)
     ;; TODO a value other than `nil' should be used so the var may be unbound
     (let ((value (if (boundp name) (eval name) nil)))
       `(,name . ,value)))
   names))

;; (defun my-op//apply-vars ()
;;   "Apply the org-page configuration."
;;   (let ((vars jtsite/vars-alist)) ;; TODO: overlay config
;;     (my-op//apply-variables vars)
;;     vars))

(defun my-op//read-site ()
  "Helper function for reading a SITE given `my-op-sites'."
  (intern-soft
   (completing-read "Site: "
                    (-map (function car) my-op-sites))))

;; Helper Macros

(defmacro my-op|with-vars (vars form)
  "Not Used: apply VARS overlay, restoring the original variables
after evaluating form."
  `(let ((old-vars my-op//get-vars ,(-map (function car) vars)))
     (my-op//apply-vars ,vars)
     ;; TODO error handling when body fails
     ,form
     (my-op//apply-vars old-vars)))

(defmacro my-op|with-default-directory (directory form)
  `(let ((old-default-directory default-directory))
     (setq default-directory ,directory)
     (let ((result ,form))
       (setq default-directory old-default-directory)
       result)))

;; Public Interface

(defun my-op/ask-if-nil ()
  "Return the current site. This is `my-op-site' if it is truthy,
else it asks for and sets the active site."
  (if my-op-site
      my-op-site
    (setq my-op-site (my-op//read-site))))

(defun my-op/site ()
  "Get the current site by calling `my-op-site-policy'."
  (funcall my-op-site-policy))

(defun my-op/select (&optional site)
  "Select SITE by applying its configuration. Returns `nil' if
SITE is invalid.

See `my-op-sites'."
  (interactive (list (my-op/site)))
  (let ((vars (cdr (assoc site my-op-sites))))
    (if vars (my-op//apply-variables vars))))

(defun my-op/do-publication (&optional site force-all base-git-commit pub-base-dir auto-commit auto-push)
  "Publish the SITE given the settings."
  (interactive (list (my-op/site)))
  (my-op/select site)
  (my-op|with-default-directory op/repository-directory
                                (op/do-publication force-all
                                                   base-git-commit
                                                   pub-base-dir
                                                   auto-commit
                                                   auto-push)))

(defun my-op/publish-to-build (&optional site)
  "Publish SITE to the build directory."
  (interactive (list (my-op/site)))
  (my-op/do-publication site my-op-force-all nil "_build" nil nil))

(defun my-op/publish-to-master (&optional site)
  (interactive (list (my-op/site)))
  (my-op/do-publication site my-op-force-all nil nil nil nil))


(provide 'my-op)
;;; my-op.el ends here
