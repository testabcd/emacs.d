;; Some code may be inspired or copied from them.
;; https://sites.google.com/site/steveyegge2/my-dot-emacs-file
;; https://github.com/technomancy/dotfiles/tree/master/.emacs.d
;; https://github.com/Silex/emacs-config/blob/master/config/tramp.el

(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(dolist (p '(better-defaults
             browse-kill-ring
             diminish
             expand-region
             find-file-in-project
             flx-ido
             gitconfig-mode
             gitignore-mode
             golden-ratio
             idle-highlight-mode
             ido-vertical-mode
             magit
             markdown-mode
             parenface
             ;; sbt-mode
             ;; scala-mode2
             scheme-here
             smartparens
             undo-tree
             yaml-mode
             smex))
  (when (not (package-installed-p p))
    (package-install p)))

(defmacro fun-after-load (file &rest body)
  (declare (indent defun))
  `(eval-after-load ,file
     '(progn ,@body)))

(add-to-list 'load-path user-emacs-directory)
(add-to-list 'exec-path "/usr/local/bin")

(setq inhibit-startup-message t
      visible-bell t
      disabled-command-function 'ignore ; disable disabled command
      custom-file (concat user-emacs-directory "custom.el")
      confirm-nonexistent-file-or-buffer nil
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      scroll-conservatively 9999)
(setq-default indicate-empty-lines t)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq ido-create-new-buffer 'always
      ido-enable-tramp-completion nil
      ido-enable-last-directory-history nil
      ido-record-command nil
      ido-max-work-file-list 0
      ido-max-work-directory-list 0
      ido-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-confirm-unique-completion t
      ido-use-filename-at-point 'guess
      ido-max-prospects 10)
(ido-vertical-mode t)
(flx-ido-mode t)
(setq flx-ido-use-faces nil)

(require 'recentf)
(setq recentf-save-file (concat user-emacs-directory ".recentf")
      recentf-auto-cleanup 'never
      recentf-max-menu-items 99
      recentf-max-saved-items 200)
(recentf-mode t)

(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)

(load custom-file t)
(require 'tramp)
(column-number-mode t)
(global-undo-tree-mode 1)
(golden-ratio-mode 1)
(require 'parenface)
(require 'smartparens-config)
(smartparens-global-mode t)

(set-language-environment 'UTF-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system 'utf-8
      default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(fun-after-load 'simple (diminish 'auto-fill-function))
(fun-after-load 'hi-lock (diminish 'hi-lock-mode))
(fun-after-load 'abbrev (diminish 'abbrev-mode))
(fun-after-load 'autorevert (diminish 'auto-revert-mode))
(fun-after-load 'undo-tree (diminish 'undo-tree-mode))
(fun-after-load 'golden-ratio (diminish 'golden-ratio-mode))
(fun-after-load 'smartparens (diminish 'smartparens-mode))
(fun-after-load 'magit (diminish 'magit-auto-revert-mode))

(when window-system
  (tooltip-mode -1)
  (blink-cursor-mode -1)
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (cond
   ((member "Consolas" (font-family-list))
    (set-face-attribute 'default nil :font "Consolas-13"))
   ((member "Inconsolata" (font-family-list))
    (set-face-attribute 'default nil :font "Inconsolata-14"))
   ((member "Monaco" (font-family-list))
    (set-face-attribute 'default nil :font "Monaco")))
  (load-theme 'wombat t)
  (set-face-background 'default "#1e1e1e")
  (set-face-background 'fringe "grey13")
  (set-face-background 'vertical-border "grey13")
  (set-face-foreground 'vertical-border "grey10")
  (fun-after-load 'parenface
    (set-face-foreground 'parenface-paren-face "SteelBlue4")
    (set-face-foreground 'parenface-bracket-face "SteelBlue4")
    (set-face-foreground 'parenface-curly-face "SteelBlue4"))
  (fun-after-load 'hl-line
    (set-face-background 'hl-line "gray15")
    (set-face-foreground 'highlight nil)
    (set-face-underline-p 'hl-line nil))
  (fun-after-load 'magit
    (set-face-background 'magit-item-highlight "#1e1e1e"))
  (fun-after-load 'em-prompt
    (set-face-attribute 'eshell-prompt nil :foreground "SkyBlue")))

(when (eq system-type 'darwin)
  (defun ns-get-pasteboard ()
    "Returns the value of the pasteboard, or nil for unsupported formats."
    (condition-case nil
        (ns-get-selection-internal 'CLIPBOARD)
      (quit nil))))

;; eshell
(fun-after-load 'esh-opt
  (mapc 'require '(em-prompt em-term em-smart))
  (setq eshell-cmpl-ignore-case t
        eshell-cmpl-cycle-completions nil
        eshell-save-history-on-exit t
        eshell-history-size 1024
        eshell-buffer-shorthand t
        eshell-cp-interactive-query t
        eshell-ln-interactive-query t
        eshell-mv-interactive-query t
        eshell-mv-overwrite-files nil
        eshell-aliases-file (concat user-emacs-directory "alias")
        eshell-rc-script (concat user-emacs-directory "profile")
        eshell-review-quick-commands t
        eshell-prompt-function 'fun-eshell-prompt)
  (add-to-list 'eshell-visual-commands "ssh")
  (add-to-list 'eshell-visual-commands "tail")
  (add-hook 'eshell-mode-hook 'eshell-smart-initialize)

  (defun eshell/cdu ()
    "Change directory to the project's root."
    (eshell/cd (or (locate-dominating-file default-directory "src")
                   (locate-dominating-file default-directory ".git"))))

  (defun eshell/cdd (&optional dir)
    "Go into a directory as deep as possible if it contains only one directory."
    (or (not dir) (eshell/cd dir))
    (let ((dirfiles (directory-files (eshell/pwd) nil "[^^\\.$\\|^\\..$]" t)))
      (when (= (length dirfiles) 1)
        (let ((firstfile (car dirfiles)))
          (when (file-directory-p firstfile)
            (eshell/cd firstfile)
            (eshell/cdd))))))

  (defun fun-dir-last-part-name (dir)
    "Return the last part of a directory. For example, ~/foo/bar/emacs will return emacs"
    (if (string= dir (getenv "HOME"))
        "~"
      (let ((dir-lists (split-string dir "/")))
        (let ((dir-name (car (last dir-lists))))
          (if (string= dir-name "") "/" dir-name)))))

  (defun fun-eshell-prompt ()
    (concat "空 "
            ;; tramp-user @ tramp-host
            (when (tramp-tramp-file-p default-directory)
              (let ((tramp-path (tramp-dissect-file-name default-directory)))
                (concat "(" (tramp-file-name-user tramp-path) "@"
                        (tramp-file-name-real-host tramp-path) ") ")))
            (fun-dir-last-part-name (abbreviate-file-name (eshell/pwd)))
            ;; git branch && git dirty status
            (when (and (eshell-search-path "git")
                       (locate-dominating-file (eshell/pwd) ".git"))
              (let ((name (shell-command-to-string "git branch | grep \\* | awk '{print $2}'")))
                (concat " ("
                        (if (> (length name) 0)
                            (substring name 0 -1)
                          "no branch")
                        (unless (string-match "nothing to commit.*clean"
                                              (shell-command-to-string "git status"))
                          "*")
                        ")")))
            (if (= (user-uid) 0) " # " " $ "))))

;; programming
(add-hook 'prog-mode-hook
          (lambda ()
            (set (make-local-variable 'comment-auto-fill-only-comments) t)
            (auto-fill-mode t)
            (hl-line-mode t)
            (idle-highlight-mode)
            (setq show-trailing-whitespace t)
            (add-hook 'after-save-hook 'check-parens nil t)
            (font-lock-add-keywords
             nil `(("(?\\(lambda\\>\\)"
                    (0 (progn
                         (compose-region (match-beginning 1)
                                         (match-end 1)
                                         ,(make-char 'greek-iso8859-7 107))
                         nil)))))))

(fun-after-load 'scala-mode2
  (require 'sbt-mode)
  (defalias 'fun-run-scala 'run-scala)

  (defun fun-sbt-start ()
    (interactive)
    (if (comint-check-proc (sbt:buffer-name))
        (switch-to-buffer-other-window (sbt:buffer-name))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (sbt-start))))

  (defun fun-sbt-switch-to ()
    (interactive)
    (if (comint-check-proc (sbt:buffer-name))
        (switch-to-buffer-other-window (sbt:buffer-name))
      (message "SBT hasn't started.")))

  (defun fun-sbt-compile-program ()
    (interactive)
    (sbt-command "compile")
    (message "sbt-compile starts."))

  (defun fun-sbt-run-program ()
    (interactive)
    (sbt-command "run")
    (message "sbt-run starts."))

  (defun fun-sbt-test-program ()
    (interactive)
    (sbt-command "test")
    (message "sbt-test starts."))

  (defun fun-package-name ()
    "Insert your scala package name."
    (interactive)
    (let ((pwd (substring (nth 1 (split-string (pwd))) 0 -1)))
      (insert (replace-regexp-in-string
               "\/" "." (replace-regexp-in-string
                         ".+?\/src\/main\/scala\/" "" pwd)))))

  (fun-after-load 'find-file-in-project
    (setq ffip-patterns (append '("*.scala" "*.sbt") ffip-patterns)))

  (setq sbt:clear-buffer-before-command nil
        compilation-skip-threshold 1)
  (define-key sbt:mode-map (kbd "C-a") 'comint-bol)
  (define-key sbt:mode-map (kbd "C-c M-t") 'fun-sbt-test-program)
  (define-key sbt:mode-map (kbd "C-c M-c") 'fun-sbt-compile-program)
  (define-key sbt:mode-map (kbd "C-c M-r") 'fun-sbt-run-program)

  (define-key scala-mode-map (kbd "C-c M-j") 'fun-sbt-start)
  (define-key scala-mode-map (kbd "C-c M-t") 'fun-sbt-test-program)
  (define-key scala-mode-map (kbd "C-c M-c") 'fun-sbt-compile-program)
  (define-key scala-mode-map (kbd "C-c M-r") 'fun-sbt-run-program)
  (define-key scala-mode-map (kbd "C-c C-r") 'sbt-send-region)
  (define-key scala-mode-map (kbd "C-c C-z") 'fun-sbt-switch-to))

(fun-after-load 'scheme
  (setq scheme-program-name "petite"
        scheme-macro-expand-command "(expand `%s)")

  (require 'iuscheme)
  (define-key inferior-scheme-mode-map (kbd "RET") 'comint-send-input)

  (require 'scheme-here)
  (define-key scheme-mode-map (kbd "C-c M-j") 'run-scheme-here)
  (define-key scheme-mode-map (kbd "C-x C-e") 'scheme-here-send-sexp)
  (define-key scheme-mode-map (kbd "C-c C-r") 'scheme-here-send-region)
  (define-key scheme-mode-map (kbd "C-c C-e") 'scheme-here-send-def)
  (define-key scheme-mode-map (kbd "C-c C-b") 'scheme-here-eval-buffer)
  (define-key scheme-mode-map (kbd "C-c C-t") 'scheme-here-trace-procedure)
  (define-key scheme-mode-map (kbd "C-c C-x") 'scheme-here-expand-current-form)
  (define-key scheme-mode-map (kbd "C-c C-l") 'scheme-here-load-file)
  (define-key scheme-mode-map (kbd "C-c C-z") 'switch-to-scheme-here))

;; utils
(defun fun-indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max))
  (message "Indent the buffer."))

(defun fun-untabify ()
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (message "Untabify and delete trailing whitespaces."))

(defun fun-rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun fun-delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun fun-sudo-edit-file ()
  (interactive)
  (defun local-file-name-as-sudo (file-name)
    "Transforms /foo/bar.ext into /sudo::/foo/bar.ext"
    (concat "/sudo::" file-name))

  (defun tramp-file-name-as-sudo (file-name)
    "Transforms /scp:user@host:/foo/bar.ext into /ssh:user@host|sudo:host:/foo/bar.ext"
    (let* ((parts (tramp-dissect-file-name file-name))
           (host (tramp-file-name-host parts)))
      (setq file-name (replace-regexp-in-string (regexp-quote (concat host ":"))
                                                (concat host "|sudo:" host ":")
                                                file-name t t))
      (setq file-name (replace-regexp-in-string "^/scp" "/ssh" file-name))))

  (defun buffer-file-name-as-sudo (&optional buffer)
    (let* ((buffer (or buffer (current-buffer)))
           (file-name (or (buffer-file-name buffer) dired-directory)))
      (if (tramp-tramp-file-p file-name)
          (tramp-file-name-as-sudo file-name)
        (local-file-name-as-sudo file-name))))
  (find-alternate-file (buffer-file-name-as-sudo)))

;; bindings
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-x f") 'find-file-in-project)
(global-set-key (kbd "C-c k") 'browse-kill-ring)
(global-set-key (kbd "C-x m") 'eshell)
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))
(global-set-key (kbd "C-c i") 'fun-indent-buffer)
(global-set-key (kbd "C-c u") 'fun-untabify)
(global-unset-key (kbd "C-z"))

(fun-after-load 'smartparens-config
  (define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
  (define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)
  (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
  (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)
  (define-key sp-keymap (kbd "C-)") 'sp-forward-slurp-sexp)
  (define-key sp-keymap (kbd "C-(") 'sp-backward-slurp-sexp)
  (define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
  (define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward))

(when window-system (eshell))
