;; create backup directory for edited files
(setq backup-directory-alist
      (list (cons "." (expand-file-name "backup" user-emacs-directory))))
