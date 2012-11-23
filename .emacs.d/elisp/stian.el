;; Providing myself
(provide 'stian)

;; Pretty print marked region of xml-text, from : http://blog.bookworm.at/2007/03/pretty-print-xml-with-emacs.html
(defun sla-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Ah, much better!"))


;; Open a cygwin shell, doesn't work yet...
(defun cygwin-shell ()
  "Run cygwin bash in shell mode."
  (interactive)
  (let ((explicit-shell-file-name "C:/cygwin/bin/bash"))
    (call-interactively 'shell))
  (setq explicit-bash-args '("--login" "-i")))


;; The command "M-x uniquify-buffer-lines" will remove identical
;; adjacent lines
;; in the current buffer, similar to what is obtained with the Unix uniq command.

(defun uniquify-region-lines (beg end)
  "Remove duplicate adjacent lines in region."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
      (replace-match "\\1"))))

(defun uniquify-buffer-lines ()
  "Remove duplicate adjacent lines in the current buffer."
  (interactive)
  (uniquify-region-lines (point-min) (point-max)))

;; Fetched from http://peepcode.com/blog/2012/commanding-your-text-editor/emacs-newline.el
(defun newline-previous ()
  "Insert a blank line above the cursor and move the cursor up one line."
  (interactive)
  (beginning-of-line)
  (newline)
  (previous-line)
  (indent-according-to-mode))

;; From https://github.com/defunkt/textmate.el
(defun newline-next ()
  "Inserts an indented newline after the current line and moves the point to it."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(global-set-key (kbd "C-<return>") 'newline-next)
(global-set-key (kbd "C-S-<return>") 'newline-previous)

;; Set up autocomplete
(global-set-key (kbd "C-'") 'dabbrev-expand)
