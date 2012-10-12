(require 'package)
(add-to-list 'package-archives 
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
 (add-to-list 'package-archives
 	     '("tromey" . "http://tromey.com/elpa/"))
(package-initialize)

;; (when (null package-archive-contents)
;;   (package-refresh-contents))

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
(setq el-get-dir (expand-file-name "el-get" user-emacs-directory))

(add-to-list 'load-path 
             (expand-file-name "el-get/el-get" user-emacs-directory))
(setq el-get-user-package-directory "~/.emacs.d/packages.d/")

(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(el-get 'sync)

;; (unless (require 'el-get nil t) 
;;   (url-retrieve "https://raw.github.com/dimitri/el-get/master/el-get-install.el" 
;;                 (lambda (s) (end-of-buffer) (eval-print-last-sexp))))

;; local sources
(setq el-get-sources
      '(
;(:name asciidoc
;	       :type elpa
;	       :after (lambda ()
;			(autoload 'doc-mode "doc-mode" nil t)
;			(add-to-list 'auto-mode-alist '("\\.adoc$" . doc-mode))
;			(add-hook 'doc-mode-hook '(lambda ()
;						    (turn-on-auto-fill)
;						    (require 'asciidoc)))))
					;        (:name xml-rpc :type elpa)
;        (:name yasnippet :type elpa)
					;        (:name feature-mode :type elpa)
					;        (:name findr :type elpa)
					;        (:name gh :type elpa)
					;        (:name hexrgb :type emacswiki)
					;        (:name inflections :type elpa)
					;        (:name jump :type elpa)
;        (:name magit-gh-pulls :type elpa)        
					;	(:name eieio :type elpa)
;	(:name gist :type elpa)
	(:name starter-kit :type marmalade)
	(:name starter-kit-elisp :type marmalade)
	(:name starter-kit-bindings :type marmalade)
        (:name starter-kit-eshell :type marmalade)
;	(:name clojure-mode :type elpa)
;	(:name clojure-test-mode :type elpa)
	
;	(:name magit				; git meet emacs, and a binding
;	     :after (progn
;		      (global-set-key (kbd "C-x C-z") 'magit-status)))
                                        ;	)
        ))

(setq sla-packages
      '(
	; eieio
					; xml-rpc
					; gh
					; inflections
					; jump
					; feature-mode
;	yasnippet
					; rvm
					; rhtml-mode
					; org-mode
					; coffee-mode
;	fill-column-indicator
					; findr
					; haml-mode
					; hexrgb
					; htmlize
					; inflections
					; inf-ruby
					; jump
;	starter-kit
;        starter-kit-bindings
;        starter-kit-elisp
;	magit
;	magit-gh-pulls
;	magithub
	; zenburn-theme
					; minimap
					; multi-term
					; scss-mode
	))

;; Annoying packages that explode during install if their deps are not met
;(setq sla-dependent-packages
 ;     '(; org2blog 
					; gist
					; sass-mode
;	))


;(el-get 'sync (mapcar 'prin1-to-string sla-packages))
;(el-get 'sync (mapcar 'prin1-to-string sla-dependent-packages))
