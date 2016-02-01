;; setup packages 
(package-initialize)

;; Setup DNB proxy
(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|10.*\\)")
	("http" . "proxy:88")
	("https" . "proxy:88")))

(setq user-full-name "Stian Lammio Almås"
      user-mail-address "stian.almaas@gmail.com")

;; Emacs restart
(setq debug-on-error t)
;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen
(setq inhibit-startup-message t)

;; Setting up repositories for packages
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (package-refresh-contents))

;; Set up packages with use-package
(add-to-list 'load-path "~/.emacs.d/lisp/")
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(require 'use-package)
(use-package auto-compile
  :ensure t
  :config (auto-compile-on-load-mode))


;; load local stuff
;; (load "~/.emacs.d/local.el")

;; Move file backups to one directory
(setq backup-directory-alist '(("." . "~/backups")))

;; Highlight current line
(global-hl-line-mode t)

;; Better defaults, by technomancy
(use-package better-defaults)

;; Helm
(use-package helm
             :ensure t
             :diminish helm-mode
             :init
             (progn
               (require 'helm-config)
               (setq helm-candidate-number-list 100)
               ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
               ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
               ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
               (global-set-key (kbd "C-c h") 'helm-command-prefix)
               (global-unset-key (kbd "C-x c"))


               (when (executable-find "curl")
                 (setq helm-google-suggest-use-curl-p t))

               (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
                     helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
                     helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
                     helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
                     helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
                     helm-ff-file-name-history-use-recentf t)

               (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
                     helm-input-idle-delay 0.01  ; this actually updates things reeeelatively quickly.
                     helm-yas-display-key-on-candidate t
                     helm-quick-update t
                     helm-M-x-requires-pattern nil
                     helm-ff-skip-boring-files t)

               (helm-mode))
             :config
             (progn
               (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
               (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
               (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
               )
             :bind (("C-x b" . helm-mini)
                    ("C-h a" . helm-apropos)
                    ("C-x C-b" . helm-buffers-list)
                    ("C-x b" . helm-buffers-list)
                    ("C-x C-f" . helm-find-files)
                    ("M-y" . helm-show-kill-ring)
                    ("M-x" . helm-M-x)
                    ("C-x c o" . helm-occur)
                    ("C-x c s" . helm-swoop)
                    ("C-x c y" . helm-yas-complete)
                    ("C-x c Y" . helm-yas-create-snippet-on-region)
                    ("C-x c SPC" . helm-all-mark-rings))
             )
(ido-mode -1)

;; Load theme
;; (maybe-install-and-require 'afternoon-theme)
;; (load-theme 'afternoon t)
(use-package monokai-theme
  :ensure t
  :init (load-theme 'monokai t))


(setq-default indicate-buffer-boundaries 'left)
(setq-default indicate-empty-lines +1)

;; Better title bar
(setq frame-title-format
      '("Current: " (buffer-file-name "%f"
                                      (dired-directory dired-directory "%b"))))


;; ;; Smart mode line
(use-package smart-mode-line
             :defer t
             :config
             (progn
               (setq sml/no-confirm-load-theme t)
               (sml/setup)
               (sml/apply-theme 'automatic)
               ))

;; Make scrolling smoother when using keyboard
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)
;; And for mousing
(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; Winmode
(windmove-default-keybindings)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; Remove trailing whitespace before saving
(add-hook 'before-save-hook
	  'delete-trailing-whitespace)

;; Automatically revert files that are changed on disk
(global-auto-revert-mode)

;; Ask for Y or N instead of Yes or No
(defalias 'yes-or-no-p 'y-or-n-p)

;; ;; Agressive indent mode
(use-package agressive-indent
             :defer t
             :config
             (progn
               (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
               (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
               (add-hook 'css-mode-hook #'aggressive-indent-mode)
               (add-hook 'js2-mode-hook #'aggressive-indent-mode))
             )

;; ;; Company Autocomplete
(use-package company
             :ensure t
             :config
             (add-hook 'after-init-hook 'global-company-mode))

;; ;; web-mode for html and css
;; ;; js2-mode for Javascript
(use-package web-mode
             :ensure t
             :defer t
             :mode "\\.html?\\'"
             :config
             (progn
               (setq web-mode-enable-current-element-highlight t)
               (setq web-mode-ac-sources-alist
                     '(("css" . (ac-source-css-property))
                       ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
               (add-hook 'web-mode-hook (lambda ()
                                          (setq web-mode-markup-indent-offset 2)
                                          (setq web-mode-css-indent-offset 2)
                                          (setq web-mode-code-indent-offset 2)))
               ))

(use-package js2-mode
             :ensure t
             :defer t
             :commands js2-mode
             :mode "\\.js\\'")

;; (require 'fuzzy)
;; (require 'auto-complete)
;; (setq ac-auto-show-menu t
;;       ac-quick-help-delay 0.5
;;       ac-use-fuzzy t)
;; (global-auto-complete-mode +1)

;; Echo keystrokes in minibuffer
(setq echo-keystroke 0.1)

;; Ace jump for navigation
;; (use-package ace-jump-mode
;;  :bind ("C-c SPC" . ace-jump-mode))

;; Smex for fuzzy matching in M-x
;; (use-package smex
;;   :init (smex-initialize)
;;   :bind ("M-x" . smex))

;; Multiple cursors like Sublime Text
;; (use-package multiple-cursors
;;   :bind (("C->" . mc/mark-next-like-this)
;;          ("C-<" . mc/mark-previous-like-this)))

;; Drag stuff to move pieces of text intuitively
(use-package drag-stuff
  :init (drag-stuff-global-mode 1)
  :bind (("M-N" . drag-stuff-down)
         ("M-P" . drag-stuff-up)))

;; Expand region like IntelliJ
;; (use-package expand-region
;;   :bind (("C-'" . er/expand-region) ;; For Windows
;;          ("C-@". er/expand-region)  ;; For Mac
;;          ("C-*" . er/contract-region)))

;; Remember file positions
(use-package saveplace
  :config (progn
            (setq-default save-place t)
            (setq save-place-file (expand-file-name ".places" user-emacs-directory))))

;; Moving between windows
;; (use-package windmove
;;   :config (windmove-default-keybindings 'shift))

;; Use bash as shell
;; (setq shell-file-name "bash")
;; (setq explicit-shell-file-name shell-file-name)

;; Tramp for remote editing
;; (require 'tramp)
;; (setq tramp-default-method "sshx")
;; (setq tramp-debug-buffer t)

;; Guide-key for helping with key sequences
;; (use-package guide-key
;;   :init (setq guide-key-mode 1))
;; (setq guide-key/guide-key-sequence '("C-x"))
;; (setq guide-key/recursive-key-sequence-flag t)
;; (guide-key-mode 1)
;; (setq guide-key/popup-window-position 'bottom)

;; Discover.el, also for helping with key sequences
;; (require 'discover)
;; (global-discover-mode 1)

;; Visual Regexp. Finally...
(use-package visual-regexp
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)))

;; Customize-* file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Auto refresh buffers
;; (global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)


;; Which-key for help
(use-package which-key
  :diminish which-key-mode
  :config (which-key-mode))


;; Shortcut for duplicating current line - http://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (newline)
  (yank)
)
(global-set-key (kbd "C-S-d") 'duplicate-line)


;; http://tuxicity.se/emacs/elisp/2010/03/11/duplicate-current-line-or-region-in-emacs.html
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

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


;; Setup for on-the-fly syntax checking
;; First clone this repo : https://github.com/davidmiller/lintnode.git

;; Inside that repo run : npm install express connect-form haml underscore
;;(add-to-list 'load-path "C:\\utv\\repos\\lintnode")
;;(require 'flymake-jslint)
;; Make sure we can find the lintnode executable
;;(setq lintnode-location "C:\\utv\\repos\\lintnode")
;; JSLint can be... opinionated
;;(setq lintnode-jslint-excludes (list 'nomen 'undef 'plusplus 'onevar 'white))
;; Start the server when we first open a js file and start checking
;; (add-hook 'js-mode-hook
;;           (lambda ()
;;             (lintnode-hook)))



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
(global-set-key (kbd "<C-c x>") 'sla-pretty-print-xml-region)

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

;; Using smartparens to show matching delimiters
;; (smartparens-global-mode t)
;; (show-smartparens-global-mode +1)

;; Allow narrow-to-region, I can handle it!
;; (put 'narrow-to-region 'disabled nil)
