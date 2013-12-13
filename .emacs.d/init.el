;; Emacs restart

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen
(setq inhibit-startup-message t)

;; Load local stuff
(load "~/.emacs.d/local.el")

;; Cask and Pallet package management
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

(require 'f)
(require 'use-package)

;; Loading files
(defun load-x (file)
  (load (f-expand file user-emacs-directory)))

;(let ((default-directory user-emacs-directory))
;  (load-x "defuns")
;  (load-x "misc")
;  (load-x "local")
;  (when (eq system-type 'darwin)
;    (load-x "osx"))
;)


;; Use packages

;; Better defaults, by technomancy
(require 'better-defaults)

;; Fringe decoration for Git
(when (window-system)
  (require 'git-gutter-fringe))

(global-git-gutter-mode +1)
(setq-default indicate-buffer-boundaries 'left)
(setq-default indicate-empty-lines +1)

;; Smart mode line
(require 'smart-mode-line)
(sml/setup)

;; Make scrolling smoother when using keyboard
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)
;; And for mousing
(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; Autocomplete
(require 'fuzzy)
(require 'auto-complete)
(setq ac-auto-show-menu t
      ac-quick-help-delay 0.5
      ac-use-fuzzy t)
(global-auto-complete-mode +1)

;; Ace jump for navigation
(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

;; Smex for fuzzy matching in M-x
(use-package smex
  :init (smex-initialize)
  :bind ("M-x" . smex))

;; Multiple cursors like Sublime Text
(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

;; Drag stuff to move pieces of text intuitively
(use-package drag-stuff
  :init (drag-stuff-global-mode 1)
  :bind (("M-N" . drag-stuff-down)
         ("M-P" . drag-stuff-up)))

;; Expand region like IntelliJ
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Remember file positions
(use-package saveplace
  :config (progn
            (setq-default save-place t)
            (setq save-place-file (expand-file-name ".places" user-emacs-directory))))

;; Moving between windows
(use-package windmove
  :config (windmove-default-keybindings 'shift))

;; Use bash as shell
(setq shell-file-name "bash")
(setq explicit-shell-file-name shell-file-name)

;; Tramp for remote editing
(require 'tramp)
(setq tramp-default-method "sshx")
(setq tramp-debug-buffer t)

;; Load theme
(load-theme 'tango-dark t)

;; Guide-key for helping with key sequences
;;(use-package guide-key
;;  :init (setq guide-key-mode 1)
;;  :bind ("C-x r" . guide-key/guide-key-sequence))

;; Customize-* file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Smarter way of opening new lines, like in http://blog.pluralsight.com/commanding-your-text-editor
(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)



;; Some lisp helper functions...
;; Todo - move to a more proper place
(defun sla-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this. The function inserts linebreaks to separate tags that have
nothing but whitespace between them. It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
      (backward-char) (insert "\n") (setq end (1+ end)))
    (indent-region begin end))
  (message "Ah, much better!"))

;; Formatting for JSON
(load "~/.emacs.d/json-reformat.el")


;; Rename file for buffer
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)


;; And a function to delete the file as well
(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)


;; Move more quickly
(global-set-key (kbd "C-S-n")
                (lambda ()
                  (interactive)
                  (ignore-errors (next-line 5))))

(global-set-key (kbd "C-S-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (previous-line 5))))

(global-set-key (kbd "C-S-f")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-char 5))))

(global-set-key (kbd "C-S-b")
                (lambda ()
                  (interactive)
                  (ignore-errors (backward-char 5))))

;; Join lines
(global-set-key (kbd "M-j")
            (lambda ()
                  (interactive)
                  (join-line -1)))

;; bash tab completion
(autoload 'bash-completion-dynamic-complete 
  "bash-completion"
  "BASH completion hook")
(add-hook 'shell-dynamic-complete-functions
  'bash-completion-dynamic-complete)
(add-hook 'shell-command-complete-functions
  'bash-completion-dynamic-complete)



;; Allow narrow-to-region, I can handle it!
(put 'narrow-to-region 'disabled nil)
