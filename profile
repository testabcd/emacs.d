export TERM=emacs
export EDITOR=emacs
export PAGER=cat
export PATH=$HOME/bin:/usr/local/bin:$PATH

(ignore
 (when (memq window-system '(mac ns))
   (let ((path (getenv "PATH")))
     (setq eshell-path-env path
           exec-path (append (parse-colon-path path) (list exec-directory)))))

 (let ((fortune-exists-p (executable-find "fortune"))
       (cowsay-exists-p (executable-find "cowsay"))
       (fortune-command "fortune 70% computers computers -o")
       (cowsay-command "cowsay -n -f small"))
   (cond
    ((and fortune-exists-p cowsay-exists-p)
     (eshell-interactive-print
      (shell-command-to-string (concat fortune-command " | " cowsay-command))))
    (fortune-exists-p
     (progn
       (eshell-interactive-print (shell-command-to-string fortune-command))
       (eshell-interactive-print "\n"))))))
