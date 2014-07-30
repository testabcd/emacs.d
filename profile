export TERM=emacs
export EDITOR=emacs
export PAGER=cat
export PATH=$HOME/bin:/usr/local/bin:$PATH

(ignore
 (when (memq window-system '(mac ns))
   (let ((path (getenv "PATH")))
     (setq eshell-path-env path
           exec-path (append (parse-colon-path path) (list exec-directory))))))

# life's fun
fortune 70% computers computers -o | cowsay -n -f small
