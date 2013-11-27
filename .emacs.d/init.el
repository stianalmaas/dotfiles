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

;; Adding cygwin to exec-path
;(if (file-directory-p "c:/cygwin/bin")
;  (add-to-list 'exec-path "c:/cygwin/bin"))

;; Use bash as shell
(setq shell-file-name "bash")
(setq explicit-shell-file-name shell-file-name)

;; Tramp for remote editing
(require 'tramp)
(setq tramp-default-method "sshx")
(setq tramp-debug-buffer t)

;; Load theme
(load-theme 'tango-dark t)

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

