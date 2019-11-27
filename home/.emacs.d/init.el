;;; init.el --- Initialization file for Emacs.
;;;
;;;
;;; Commentary:
;;;
;;; Author: Jake McCrary

;;; Code:

(defvar dotfiles-dir)

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(defvar tmp-dir)
(setq tmp-dir (file-name-as-directory (concat dotfiles-dir "tmp")))
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(make-directory tmp-dir t)

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" .
                          ;; "https://elpa.zilongshanren.com/melpa/"
                          "https://melpa.org/packages/"
                          )
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))


(setq inhibit-startup-screen t)

;; Taken from http://stackoverflow.com/questions/2081577/setting-emacs-split-to-horizontal
(setq split-height-threshold nil)
(setq split-width-threshold 200)

(global-linum-mode t)

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

;; Better titlebar look for Mac
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(defun esk-add-watchwords ()
  "Font lock words from emacs-starter-kit."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\)"
          1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'esk-add-watchwords)
;; Perform general cleanup.

(defun esk-untabify-buffer ()
  "Remove tabs from buffer.  Taken from emacs-starter-kit."
  (interactive)
  (untabify (point-min) (point-max)))

(defun esk-indent-buffer ()
  "Indent buffer.  Taken from emacs-starter-kit."
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
(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      ring-bell-function 'ignore
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      auto-save-file-name-transforms `((".*", (concat user-emacs-directory "autosave/") t)))



(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))


;; -------------------------------------------

;; Load use-package and its dependencies.
(eval-when-compile
  (require 'use-package))
(use-package diminish
  :ensure t)
(require 'diminish)
(require 'bind-key)

(use-package server
  :custom (server-socket-dir (expand-file-name "server" user-emacs-directory))
  :config
  (unless (server-running-p)
    (server-start)))

(use-package xclip
  :ensure t
  :defer t
  :diminish ""
  :config
  (xclip-mode 1))

(use-package super-save
  :ensure t
  :config
  (super-save-mode +1))

;; (setq super-save-auto-save-when-idle t) ; autosave on idle
;; (setq auto-save-default nil) ; turn off built in


;; (use-package system-packages
;;   :custom (system-packages-package-manager (quote brew))
;;   :ensure t)

(use-package use-package-ensure-system-package
  :ensure t)


(use-package visual-regexp-steroids
  :ensure t
  :bind (("M-%" . vr/query-replace)
         ("C-s" . vr/isearch-forward)
         ("C-r" . vr/isearch-backward)))

;; (use-package all-the-icons)

;; somewhat taken from https://github.com/sam217pa/emacs-config/blob/develop/init.el and https://sam217pa.github.io/2016/09/13/from-helm-to-ivy/
(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :bind
  (:map ivy-mode-map
        ("C-'" . ivy-avy))
  :config
  (ivy-mode 1)
  :custom
  (ivy-use-virtual-buffers t "add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.")
  (ivy-height 10 "number of result lines to display")
  (ivy-count-format "" "does not count candidates")
  (ivy-initial-inputs-alist nil "no regexp by default")
  ;; allow input not in order
  (ivy-re-builders-alist '((t   . ivy--regex-ignore-order))))

(use-package dumb-jump
  :ensure t
  :hook ((prog-mode . dumb-jump-mode))
  ;; :ensure-system-package (ag . the_silver_surfer)
  :custom (dumb-jump-selector 'ivy))

(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C.

From: https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html"
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))


(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C.

From: https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html"
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(defun air-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\".

From: https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html"
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))


(use-package bazel-mode
  :ensure t
  :custom
  (python-indent 2))

(use-package dockerfile-mode
  :ensure t)

(use-package org
  :ensure t
  :bind (("C-c a" . org-agenda)
         ("C-c c" . 'org-capture))
  :custom
  (org-modules '(org-habit org-w3m org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail))
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
     (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
     (sequence "|" "CANCELED(c)")))
  ;; (org-refile-targets '(("~/org/gtd.org" :maxlevel . 3)
  ;;                       ("~/org/someday.org" :level . 1)
  ;;                       ("~/org/tickler.org" :maxlevel . 2)))
  (org-todo-keyword-faces '(("NEXT" . "yellow")))
  ;; Many config settings from https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
  (org-agenda-files '("~/org"))
  (org-default-notes-file "~/org/todo.org")
  (org-agenda-custom-commands
   '(("d" "Daily agenda and all TODOs"
      ((tags "PRIORITY=\"A\""
             ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
              (org-agenda-overriding-header "High-priority unfinished tasks:")))
       (agenda "" ((org-agenda-span-to-ndays 1)))
       (alltodo ""
                ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                (air-org-skip-subtree-if-priority ?A)
                                                (org-agenda-skip-if nil '(scheduled deadline))))
                 (org-agenda-overriding-header "ALL normal priority tasks:"))))
      ((org-agenda-compact-blocks t)))))

  ;; https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html
  (org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (org-outline-path-complete-in-steps nil)
  (org-refile-use-outline-path 'file)
  (org-refile-allow-creating-parent-nodes 'confirm)

  (org-clock-into-drawer "CLOCKING")
  (org-log-note-clock-out t)
  (org-log-done 'note)

  (org-capture-templates
   '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
      "* TODO %?\n  %u\n  %i\n  %a")
     ("m" "Movie" enntry (file+olp+datetree "~/org/movies.org")
      "** MOVIE @ Theater"))))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  ;; (projectile-register-project-type 'python '()
  ;;                                   :test-prefix "_test.py")
  
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :custom
  (projectile-completion-system 'ivy)
  ;; (projectile-project-root-files
  ;;  (quote ("rebar.config" "project.clj" "pom.xml" "build.sbt" "build.gradle" "Gemfile" "requirements.txt" "package.json" "gulpfile.js" "Gruntfile.js" "bower.json" "composer.json" "Cargo.toml" "mix.exs" ".git" ".projectile_root")))
  ;; (projectile-project-root-files-bottom-up (quote (".projectile" ".hg" ".fslckout" ".bzr" "_darcs")))
  (projectile-file-exists-remote-cache-expire (* 10 60)))


(use-package smex
  :ensure t)

(defun swiper-under-point ()
  "Use swiper for searching at symbol under cursor."
  (interactive)
  (swiper (format "\\<%s\\>" (thing-at-point 'symbol))))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
                                        ;         ("M-*" . swiper-under-point)
         ))

(use-package restclient
  :ensure t
  :init
  (use-package company-restclient
    :ensure t
    :config (add-to-list 'company-backends 'company-restclient)))

(use-package flycheck
  :ensure t
  :diminish ""
  ;; :pin melpa-stable
  :custom
  (flycheck-javascript-standard-executable "semistandard")
  :init
  (global-flycheck-mode))

(use-package flycheck-joker
  :ensure t)

(use-package jake-js
  :load-path "lisp")

(use-package edit-indirect
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (x-focus-frame nil)
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

;; Stop typing full "yes or no" answers to Emacs.
(defalias 'yes-or-no-p 'y-or-n-p)

(defun save-all ()
  "Save all buffers."
  (interactive)
  (save-some-buffers t))
(add-hook 'save-buffer 'save-all)


;; apperance
(set-face-attribute 'default nil
                    :family "Inconsolata"
                    :height (if (memq window-system `(mac ns))
                                180
                              120))


(global-auto-revert-mode)

;; Setting up emoji fonts, from https://github.com/dunn/company-emoji
(defun --set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs can display emoji properly."
  (if (eq system-type 'darwin)
      ;; For NS/Cocoa
      (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
    ;; For Linux
    (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))

;; For when Emacs is started in GUI mode:
(--set-emoji-font nil)
;; Hook for when a frame is created with emacsclient
;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
(add-hook 'after-make-frame-functions '--set-emoji-font)

;; spaces instead of tabs
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)



(use-package idle-highlight-mode
  :ensure t
  :hook ((prog-mode . idle-highlight-mode))
  :custom
  (idle-highlight-idle-time 0.1))

(use-package highlight-symbol
  :ensure t
  :bind (("M-*" . 'highlight-symbol)))

;; rodio's settings
;; (global-set-key [(control f3)] 'highlight-symbol)
;; (global-set-key [f3] 'highlight-symbol-next)
;; (global-set-key [(shift f3)] 'highlight-symbol-prev)
;; (global-set-key [(meta f3)] 'highlight-symbol-query-replace)

(use-package yaml-mode
  :ensure t)

;; (use-package color-theme
;;   :ensure t)

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :init
  (progn
    ;; (load-theme 'sanityinc-tomorrow-day t)
    (load-theme 'sanityinc-tomorrow-eighties t)
    (set-face-foreground 'region "white")
    (set-face-background 'region "blue")))

(use-package paredit
  :ensure t
  :diminish ""
  :hook
  ((clojure-mode . enable-paredit-mode)
   (emacs-lisp-mode . enable-paredit-mode)
   (lisp-interaction-mode . enable-paredit-mode)
   (lisp-mode . enable-paredit-mode)
   (scheme-mode . enable-paredit-mode))
  :bind (:map paredit-mode-map
              ("M-)" . paredit-forward-slurp-sexp)
              ( "M-(" . paredit-forward-slurp-sexp)
              ("M-}" . paredit-forward-barf-sexp)
              ("M-{" . paredit-backward-barf-sexp)))

(use-package company
  :ensure t
  :diminish ""
  :commands global-company-mode
  :custom
  (company-idle-delay 0.2)
  (company-selection-wrap-around t)
  (company-minimum-prefix-length 0)
  (company-candidates-length 30)
  (company-require-match nil)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil)
  (company-show-numbers t)
  :config
  (global-company-mode)
  (use-package company-statistics
    :ensure t
    :config
    (company-statistics-mode))
  (bind-keys :map company-active-map
             ("TAB" . company-complete)))

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode))

(use-package counsel
  :ensure t
  :bind*
  (("M-x" . counsel-M-x)
   ("C-c C-m" . counsel-M-x)
   ("C-x C-m" . counsel-M-x)
   ("C-x m" . counsel-M-x)
   ("C-x C-f" . counsel-find-file))
  :custom
  (counsel-find-file-ignore-regexp "\\.DS_Store\\|.git"))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode))

(use-package ag
  :ensure t
  :custom
  (ag-highlight-search t)
  (ag-reuse-buffers t)
  :config
  (add-to-list 'ag-arguments "--word-regexp"))

(use-package rainbow-delimiters
  :ensure t
  :hook ((prod-mode . rainbow-delimiters-mode))
  :custom
  (rainbow-delimiters-max-face-count 1)
  :config
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :inherit 'error))

(use-package cider
  :ensure t
  :diminish ""
  :pin melpa-stable
  :bind
  ("C-c k" . cider-ns-refresh)
  :custom
  (cider-prompt-for-symbol nil)
  (cider-print-options '(("length" 50) ("right-margin" 180)))
  (cider-jdk-src-paths '("~/.java/openjv-8-src/"
                         "~/src/opensource/clojure/src/jvm"))
  :hook
  ((cider-repl-mode . enable-paredit-mode)
   (cider-mode . (lambda () (eldoc-mode)))))

(use-package clojure-mode
  :ensure t
  :pin melpa-stable
  :bind
  ("C-:" . clojure-toggle-keyword-string)
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
  :pin melpa-stable
  :diminish ""
  :init
  (add-hook 'clojure-mode-hook (lambda ()
                                 (clj-refactor-mode 1)
                                 (yas-minor-mode 1)
                                 (cljr-add-keybindings-with-prefix "C-c C-x")))
  :custom
  (cljr-warn-on-eval nil)
  (cljr-ignore-analyzer-errors t)
  :config
  (dolist (mapping '(("time" . "clj-time.core")
                     ("string" . "clojure.string")
                     ("http" . "clj-http.client")
                     ("json" . "cheshire.core")
                     ("async" . "clojure.core.async")))
    (add-to-list 'cljr-magic-require-namespaces mapping t)))

;; (use-package ensime
;;   :ensure t
;;   :pin melpa-stable)

(use-package scala-mode
  :ensure t
  :defer t
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package sbt-mode
  :ensure t
  :defer t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(use-package lsp-mode
  :ensure t
  :defer t
  :hook (scala-mode . lsp)
  :config (setq lsp-prefer-flymake nil))

(use-package lsp-ui
  :ensure t
  :defer t)

(use-package company-lsp
  :ensure t :defer t)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode t))

(use-package avy
  :ensure t
  :bind (("C-c j" . avy-goto-word-or-subword-1)
         ("C-x j" . avy-pop-mark)))

(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window)))

(use-package magit
  :ensure t
  ;; :pin melpa-stable
  :bind ("C-c g" . magit-status)
  :init
  (use-package with-editor :ensure t)

  ;; Have magit-status go full screen and quit to previous
  ;; configuration.  Taken from
  ;; http://whattheemacsd.com/setup-magit.el-01.html#comment-748135498
  ;; and http://irreal.org/blog/?p=2253
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  (defadvice magit-quit-window (after magit-restore-screen activate)
    (jump-to-register :magit-fullscreen)))

(use-package git-timemachine
  :ensure t)

(use-package web-mode
  :ensure t
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-css-colorization t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-current-column-highlight t)
  (web-mode-enable-engine-detection t)
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode)))

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
  "Opens a scratch buffer."
  (interactive)
  (switch-to-buffer (make-temp-name "scratch")))


;;------ Python -------


(use-package python
  :ensure t
  :commands python-mode
  :interpreter ("python3" . python-mode)
  :custom
  (python-environment-virtualenv (quote ("python3" "-m" "venv"))))

(use-package realgud
  :ensure t
  :commands realgud:pdb)

;; (use-package elpy
;;   :ensure t
;;   :defer t
;;   :init
;;   (advice-add 'python-mode :before 'elpy-enable)
;;   :config
;;   ;; (when (require 'flycheck nil t)
;;   ;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   ;;   (add-hook 'elpy-mode-hook 'flycheck-mode))
;;   )


;; jedi provides auto completion for Python programs. Depends on the
;; Python packages "jedi" and "epc" to be installed on the host
;; machine. Don't use this with company, install company-jedi instead
;; (use-package jedi
;;   :ensure t
;;   :init
;;   (add-hook 'python-mode-hook 'jedi:setup)
;;   (add-hook 'python-mode-hook 'jedi:ac-setup)
;;   (setq jedi:complete-on-dot t)
;; ;  (setq jedi:setup-keys t)
;;   )

;; company-jedi wires up jedi to be a backend for the auto completion
;; library, company-mode.
(use-package company-jedi
  :ensure t
  :config
  :hook
  ((python-mode . jedi:setup))
  :init
  (setq jedi:complete-on-dot t)
  (setq jedi:use-shortcuts t)
  (add-hook 'python-mode-hook
            (lambda () (add-to-list 'company-backends 'company-jedi))))

(use-package pyenv-mode
  :ensure t)

;; (use-package pipenv
;;   :ensure t
;;   :hook (python . pipenv-mode)
;;   :init
;;   (setq
;;    pipenv-projectile-after-switch-function
;;    #'pipenv-projectile-after-switch-extended))

(use-package rainbow-mode
  :ensure t)

(use-package terraform-mode
  :ensure t)

(defun unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the reverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column 90002000))
    (fill-paragraph nil)))

(defun lein-test-refresh ()
  (interactive)
  (let ((lein-test-refresh-buffer (get-buffer "*lein-test-refresh*")))
    (when (not lein-test-refresh-buffer)
      (setq lein-test-refresh-buffer (compile "lein test-refresh"))
      (with-current-buffer lein-test-refresh-buffer
        (rename-buffer "*lein-test-refresh*")))
    lein-test-refresh-buffer))

(provide 'init)
;;; init.el ends here
