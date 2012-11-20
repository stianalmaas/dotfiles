(require 'package)
(add-to-list 'package-archives 
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
 (add-to-list 'package-archives
 	     '("tromey" . "http://tromey.com/elpa/"))
(package-initialize)

(when (null package-archive-contents)
  (package-refresh-contents))

;; (defvar my-packages '(starter-kit 
;;                       starter-kit-lisp 
;;                       starter-kit-bindings
;;                       starter-kit-eshell 
;;                       zenburn-theme
;;                       clojure-mode 
;;                       clojure-test-mode
;;                       markdown-mode 
;;                       yaml-mode
;;                       tuareg 
;;                       haskell-mode
;;                       marmalade 
;;                       oddmuse 
;;                       scpaste))

;; (dolist (p my-packages)
;;   (when (not (package-installed-p p))
;;     (package-install p)))



;; el-get - see https://github.com/dimitri/el-get
(setq el-get-dir
      (expand-file-name "el-get" user-emacs-directory))

(add-to-list 'load-path 
             (expand-file-name "el-get/el-get" user-emacs-directory))
(setq el-get-user-package-directory
      (expand-file-name "packages.d" user-emacs-directory))

(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(el-get 'sync)


(setq el-get-sources
      '(
;        (:name yasnippet :type elpa)
;	(:name gist :type elpa)
	(:name starter-kit :type marmalade)
	(:name starter-kit-elisp :type marmalade)
	(:name starter-kit-bindings :type marmalade)
        (:name starter-kit-eshell :type marmalade)
;	(:name clojure-mode :type elpa)
;	(:name clojure-test-mode :type elpa)
	
        ))


