;; My emacs config, mainly inspired by Avdi Grimm and Phil Hagelberg

;; directory variables
(setq sla-elisp-dir (expand-file-name "elisp" user-emacs-directory))
(setq sla-elisp-external-dir
      (expand-file-name "external" user-emacs-directory))
(setq sla-init-dir
      (expand-file-name "init.d" user-emacs-directory))

;; Load all elisp files in ./init.d
(if (file-exists-p sla-init-dir)
    (dolist (file (directory-files sla-init-dir t "\\.el$"))
      (load file)))

;; Set up 'custom' system
(setq custom-file (expand-file-name "emacs-customizations.el" user-emacs-directory))
(load custom-file)

