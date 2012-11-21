(setq url-proxy-services '(("no_proxy" . "work\\.com")
                           ("http" . "proxy:88")
			   ("https" . "proxy:88")))

(if (file-directory-p "c:/cygwin/bin")
    (add-to-list 'exec-path "c:/cygwin/bin"))

(setq shell-file-name "bash")
(setq explicit-shell-file-name shell-file-name)

(cond  ((eq window-system 'w32)
	(setq tramp-default-method "scpx"))
       (t
	(setq tramp-default-method "scpc")))
