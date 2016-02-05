(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(setq tmp-dir (file-name-as-directory (concat dotfiles-dir "tmp")))
(make-directory tmp-dir t)

(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")))

(when (not package-archive-contents)
  (package-refresh-contents))

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))


(setq inhibit-startup-screen t)

;; -------------------------------------------
;; taken from better-defaults and starter-kit
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(defun esk-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'esk-add-watchwords)
;; Perform general cleanup.

(defun esk-untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun esk-indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun esk-cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (esk-indent-buffer)
  (esk-untabify-buffer)
  (delete-trailing-whitespace))

(global-set-key (kbd "C-c n") 'esk-cleanup-buffer)

(defun esk-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c e") 'esk-eval-and-replace)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))


(show-paren-mode 1)
;; (setq-default indent-tabs-mode nil)
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      ;; visible-bell t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

;; -------------------------------------------

;; Load use-package and its dependencies.
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(require 'server)
(unless (server-running-p)
  (server-start))

(use-package markdown-mode
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (x-focus-frame nil)
  (exec-path-from-shell-initialize))

;; Stop typing full "yes or no" answers to Emacs.
(defalias 'yes-or-no-p 'y-or-n-p)

(defun save-all ()
  (interactive)
  (save-some-buffers t))
(add-hook 'save-buffer 'save-all)


;; apperance
(set-face-attribute 'default nil
                    :family "Inconsolata"
                    :height 160)

;; show line numbers
(global-linum-mode t)

;; spaces instead of tabs
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(use-package ido
  :ensure t
  :config
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-use-filename-at-point 'guess
        ido-max-prospects 10)
  (ido-mode +1)
  (ido-everywhere +1))

(use-package ido-ubiquitous
  :ensure t
  :config
  (ido-ubiquitous-mode +1))

(use-package flx-ido
  :ensure t
  :config
  (flx-ido-mode +1)
  (setq ido-use-faces nil))

(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("C-x C-m" . smex)
         ("C-c C-m" . smex)))

(use-package idle-highlight-mode
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook 'idle-highlight-mode))
  :config
  (setq idle-highlight-idle-time 0.1))


(use-package color-theme
  :ensure t)

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :init
  (progn
    (load-theme 'sanityinc-tomorrow-eighties t)
    (set-face-foreground 'region "white")
    (set-face-background 'region "blue")))

(use-package paredit
  :ensure t
  :init
  (progn
    (add-hook 'clojure-mode-hook 'enable-paredit-mode)
    (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
    (add-hook 'lisp-mode-hook 'enable-paredit-mode)
    (add-hook 'scheme-mode-hook 'enable-paredit-mode))
  :config
  (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-(") 'paredit-forward-slurp-sexp))

(use-package company
  :ensure t
  :init
  (progn
    (global-company-mode)
    (global-set-key (kbd "<M-tab>") 'company-complete)
    (setq company-idle-delay 0.2)
    (setq company-minimum-prefix-length 2))
  :diminish company-mode)

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode +1)
  (setq projectile-project-root-files
        (quote ("rebar.config" "project.clj" "pom.xml" "build.sbt" "build.gradle" "Gemfile" "requirements.txt" "package.json" "gulpfile.js" "Gruntfile.js" "bower.json" "composer.json" "Cargo.toml" "mix.exs" ".git" ".projectile_root")))
  (setq projectile-project-root-files-bottom-up (quote (".projectile" ".hg" ".fslckout" ".bzr" "_darcs")))
  (setq projectile-file-exists-remote-cache-expire (* 10 60)))

(use-package ag
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 1)
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :inherit 'error))

(use-package cider
  :ensure t
  ;; :pin melpa-stable
  :bind
  ("C-c k" . cider-refresh)
  :init
  (progn
    (add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
    (add-hook 'cider-mode-hook (lambda ()
                                 (cider-turn-on-eldoc-mode)))))

(use-package clojure-mode
  :ensure t
  ;; :pin melpa-stable
  :config
  (define-clojure-indent
    (POST 'defun)
    (GET 'defun)
    (DELETE 'defun)
    (PUT 'defun)
    (ANY 'defun)
    (context 'defun)
    (register-sub 'defun)
    (register-handler 'defun)))

(use-package clj-refactor
  :ensure t
  ;; :pin melpa-stable
  :init
  (progn
    (add-hook 'clojure-mode-hook (lambda ()
                                   (clj-refactor-mode 1)
                                   (yas-minor-mode 1)
                                   (cljr-add-keybindings-with-prefix "C-c C-x")))
    (setq cljr-favor-prefix-notation nil)
    (setq cljr-warn-on-eval nil)
    (setq cljr-find-usages-ignore-analyzer-errors t))
  :config
  (setq cljr-ignore-analyzer-errors t)
  (dolist (mapping '(("maps" . "outpace.util.maps")
                     ("seqs" . "outpace.util.seqs")
                     ("times" . "outpace.util.times")
                     ("repl" . "outpace.util.repl")
                     ("time" . "clj-time.core")
                     ("string" . "clojure.string")
                     ("http" . "clj-http.client")
                     ("json" . "cheshire.core")
                     ("async" . "clojure.core.async")))
    (add-to-list 'cljr-magic-require-namespaces mapping t)))


(use-package avy
  :ensure t
  :bind (("C-c j" . avy-goto-word-or-subword-1)
         ("C-x j" . avy-pop-mark)
         ("s-." . avy-goto-word-or-subword-1)))

(use-package magit
  :ensure t
  ;; :pin melpa-stable
  :bind ("C-c g" . magit-status)
  :init (use-package with-editor
          :ensure t))

(use-package web-mode
  :ensure t)

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)))))))

(defun scratch-buffer ()
  (interactive)
  (switch-to-buffer (make-temp-name "scratch")))

(defun my-update-env (fn)
  (let ((str
         (with-temp-buffer
           (insert-file-contents fn)
           (buffer-string))) lst)
    (setq lst (split-string str "\000"))
    (while lst
      (setq cur (car lst))
      (when (string-match "^\\(.*?\\)=\\(.*\\)" cur)
        (setq var (match-string 1 cur))
        (setq value (match-string 2 cur))
        (setenv var value))
      (setq lst (cdr lst)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cljr-ignore-analyzer-errors t)
 '(css-indent-offset 2)
 '(inhibit-startup-screen t)
 '(web-mode-code-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
