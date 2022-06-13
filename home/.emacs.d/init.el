;;; init.el --- Initialization file for Emacs.
;;;
;;;
;;; Commentary:
;;;
;;; Author: Jake McCrary

;;; Code:


(setq comp-speed 2)

(when (boundp 'comp-eln-load-path)
  (let ((eln-cache-dir (expand-file-name "eln-cache/" user-emacs-directory))
        (find-exec (executable-find "find")))
    ;; Quitting emacs while native compilation in progress can leave zero byte
    ;; sized *.eln files behind. Hence delete such files during startup.
    (when find-exec
      (call-process find-exec nil nil nil eln-cache-dir
                    "-name" "*.eln" "-size" "0" "-delete" "-or"
                    "-name" "*.eln.tmp" "-size" "0" "-delete"))))

(setq native-comp-async-report-warnings-errors nil)


(defvar dotfiles-dir)

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(defvar tmp-dir)
(setq tmp-dir (file-name-as-directory (concat dotfiles-dir "tmp")))
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(make-directory tmp-dir t)

(setq inhibit-startup-screen t)

;; Taken from http://stackoverflow.com/questions/2081577/setting-emacs-split-to-horizontal
(setq split-height-threshold nil)
(setq split-width-threshold 200)

(global-linum-mode t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package straight
  :custom (straight-use-package-by-default t))

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

;; (global-set-key (kbd "C-c n") 'esk-cleanup-buffer)

(defun esk-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))


(defun jake/shrink-other-window-if-larger-than-buffer ()
  (interactive)
  (ace-window t)
  (shrink-window-if-larger-than-buffer)
  (ace-window t))
(global-set-key (kbd "C-x _") 'jake/shrink-other-window-if-larger-than-buffer)

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
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups/")))
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

(use-package diminish)
(require 'diminish)
(require 'bind-key)

(use-package server
  :custom (server-socket-dir (expand-file-name "server" user-emacs-directory))
  :config
  (unless (server-running-p)
    (server-start)))

(use-package xclip
  :diminish ""
  :config
  (xclip-mode 1))

(use-package super-save
  :config
  (super-save-mode +1))

;; (setq super-save-auto-save-when-idle t) ; autosave on idle
;; (setq auto-save-default nil) ; turn off built in


;; ;; (use-package system-packages
;; ;;   :custom (system-packages-package-manager (quote brew)))

;; (use-package use-package-ensure-system-package)


(use-package visual-regexp-steroids
  :bind (("M-%" . vr/query-replace)
         ("C-s" . vr/isearch-forward)
         ("C-r" . vr/isearch-backward)))

;; (use-package all-the-icons)

;; somewhat taken from https://github.com/sam217pa/emacs-config/blob/develop/init.el and https://sam217pa.github.io/2016/09/13/from-helm-to-ivy/
(use-package ivy
  :diminish (ivy-mode . "")
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
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-selector 'ivy))

(use-package xref
  :bind (("M-." . xref-find-definitions)))

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


(use-package bazel
  :mode
  (("\\.bzl$" . bazel-mode)
   ("\\.bazel" . bazel-mode)
   ("^WORKSPACE$" . bazel-mode))
  :custom
  (python-indent 4))

(use-package dockerfile-mode)

(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c c" . 'org-capture))
  :custom
  (org-modules '(org-habit ol-w3m ol-bbdb ol-bibtex ol-docview ol-gnus ol-info ol-irc ol-mhe ol-rmail))
  (org-startup-folded t)
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "|" "DONE(d)")
     (sequence "REPORT(r)" "BLOCKED(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
     (sequence "|" "CANCELED(c)")))
  ;; (org-refile-targets '(("~/org/gtd.org" :maxlevel . 3)
  ;;                       ("~/org/someday.org" :level . 1)
  ;;                       ("~/org/tickler.org" :maxlevel . 2)))
  (org-todo-keyword-faces '(("NEXT" . "yellow")
                            ("STARTED" . "cyan")
                            ("BLOCKED" . "red")))
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
      "** MOVIE @ Theater")
     ("j" "Journal entry" entry (function org-journal-find-location)
      "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?"))))

;; Take from https://stackoverflow.com/questions/17435995/paste-an-image-on-clipboard-to-emacs-org-mode-file-without-saving-it
(defun my-org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (org-display-inline-images)
  (setq filename
        (concat
         (make-temp-name
          (concat (file-name-nondirectory (buffer-file-name))
                  "_imgs/"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (unless (file-exists-p (file-name-directory filename))
    (make-directory (file-name-directory filename)))
                                        ; take screenshot
  (if (eq system-type 'darwin)
      (call-process "screencapture" nil nil nil "-i" filename))
  (if (eq system-type 'gnu/linux)
      (call-process "import" nil nil nil filename))
                                        ; insert into file if correctly taken
  (if (file-exists-p filename)
      (insert (concat "[[file:" filename "]]"))))

(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  (goto-char (point-min)))

(defun org-journal-create-new-id ()
  "Create org-id at beginning of new journal document."
  (message "Starting org-journal-new-id")
  (goto-char (point-min))
  (org-id-get-create)
  (goto-char (point-max))
  (message "finished org-journal-new-id"))

(defun jm/org-journal-new-entry-with-id (prefix)
  (interactive "P")
  (org-journal-new-entry prefix)
  (org-journal-create-new-id))

(use-package org-journal
  :init
  (add-hook 'org-journal-after-header-create-hook 'org-journal-create-new-id)
  :bind (("C-c s-j" . jm/org-journal-new-entry-with-id)
         ("C-c M-s-j" . org-journal-open-current-journal-file))
  :custom
  ;; (org-journal-date-prefix "#+title: ")
  (org-journal-file-header "#+title: %A, %d %B %Y \n* %A, %d %B %Y")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "~/org/roam/journal/")
  (org-journal-date-format "%A, %d %B %Y")
  (org-journal-time-format "%m/%d %R")
  (org-journal-enable-agenda-integration t)
  (org-journal-carryover-items "TODO=\"TODO\"|TODO=\"NEXT\"|TODO=\"STARTED\"|TODO=\"BLOCKED\""))

(defvar jm/org-roam-directory (file-truename "~/org/roam/"))

(use-package org-roam
  :custom
  (org-roam-directory jm/org-roam-directory)
  
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :init
  (setq org-roam-v2-ack t)
  :config
  (org-roam-db-autosync-enable)
  (setq org-roam-v2-ack t)
  ;; (setq org-roam-dailes-directory "daily/")
  ;; (setq org-roam-dailies-capture-templates
  ;;     '(("d" "default" entry
  ;;        "* %?"
  ;;        :target (file+head "%<%Y-%m-%d>.org"
  ;;                           "#+title: %<%Y-%m-%d>\n"))))
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer))))

(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory jm/org-roam-directory))

(use-package projectile
  :config
  (projectile-mode +1)

  (projectile-update-project-type
   'lein-test
   :related-files-fn
   (list
    (projectile-related-files-fn-test-with-suffix "clj" "_test")
    (projectile-related-files-fn-test-with-suffix "clj" "_expectations")))
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :custom
  (projectile-project-search-path '(("~/src/" . 2)
                                     ("~/git.drwholdings.com/beefalo/" . 1)
                                    ("~/github.com/" . 2)))
  (projectile-completion-system 'ivy)
  (projectile-enable-caching t)
  (projectile-file-exists-remote-cache-expire (* 10 60))
  (projectile-file-exists-local-cache-expire (* 10 60)))

(defun beefalo/project-buf-name ()
  (ignore-errors
    (flet ((omg (s) (file-name-nondirectory (directory-file-name (file-name-directory s)))))
      (rename-buffer
       (format "%s [%s]"
               (file-name-nondirectory buffer-file-name)
               (omg (projectile-project-root)))))))

(add-hook 'find-file-hook 'beefalo/project-buf-name)


;; (projectile-register-project-type 'python '()
;;                                   :test-prefix "_test.py")


;; (projectile-project-root-files
;;  (quote ("rebar.config" "project.clj" "pom.xml" "build.sbt" "build.gradle" "Gemfile" "requirements.txt" "package.json" "gulpfile.js" "Gruntfile.js" "bower.json" "composer.json" "Cargo.toml" "mix.exs" ".git" ".projectile_root")))
;; (projectile-project-root-files-bottom-up (quote (".projectile" ".hg" ".fslckout" ".bzr" "_darcs")))


(use-package smex)

(defun swiper-under-point ()
  "Use swiper for searching at symbol under cursor."
  (interactive)
  (swiper (format "\\<%s\\>" (thing-at-point 'symbol))))

(use-package swiper

  :bind (("C-s" . swiper)
                                        ;         ("M-*" . swiper-under-point)
         ))

(use-package org-present
  :init
  (add-hook 'org-present-mode-hook
            (lambda ()
              (org-present-big)
              (org-display-inline-images)
              (org-present-hide-cursor)
              (org-present-read-only)
              org-present-add-overlays))
  (add-hook 'org-present-mode-quit-hook
            (lambda ()
              (org-present-small)
              (org-remove-inline-images)
              (org-present-show-cursor)
              (org-present-read-write))))

;; (use-package restclient
;;   :init
;;   (use-package company-restclient
;;     :config (add-to-list 'company-backends 'company-restclient)))

(use-package flycheck
  :diminish ""
  :custom
  (flycheck-javascript-standard-executable "semistandard")
  :init
  (global-flycheck-mode))

(use-package flycheck-joker)
(use-package flycheck-clj-kondo)

;; (use-package jake-js
;;   :load-path "lisp")

(use-package edit-indirect)

(use-package markdown-mode)

(use-package exec-path-from-shell
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
  :hook ((prog-mode . idle-highlight-mode))
  :custom
  (idle-highlight-idle-time 0.1))

(use-package highlight-symbol
  :bind (("M-*" . 'highlight-symbol)))

;; rodio's settings
;; (global-set-key [(control f3)] 'highlight-symbol)
;; (global-set-key [f3] 'highlight-symbol-next)
;; (global-set-key [(shift f3)] 'highlight-symbol-prev)
;; (global-set-key [(meta f3)] 'highlight-symbol-query-replace)

(use-package yaml-mode)

;; (use-package color-theme)

(use-package color-theme-sanityinc-tomorrow
  :init
  (progn
    ;; (load-theme 'sanityinc-tomorrow-day t)
    (load-theme 'sanityinc-tomorrow-eighties t)
    (set-face-foreground 'region "white")
    (set-face-background 'region "blue")))

(use-package paredit
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
  :diminish ""
  :commands global-company-mode
  :custom
  (company-idle-delay 0.2)
  (company-selection-wrap-around t)
  (company-minimum-prefix-length 1)
  (company-candidates-length 30)
  (company-require-match nil)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil)
  (company-show-numbers t)
  :config
  (global-company-mode)
  (use-package company-statistics
    :config
    (company-statistics-mode))
  (bind-keys :map company-active-map
             ("TAB" . company-complete)))

(use-package company-quickhelp
  :config
  (company-quickhelp-mode))

(use-package counsel
  :bind*
  (("M-x" . counsel-M-x)
   ("C-c C-m" . counsel-M-x)
   ("C-x C-m" . counsel-M-x)
   ("C-x m" . counsel-M-x)
   ("C-x C-f" . counsel-find-file))
  :custom
  (counsel-find-file-ignore-regexp "\\.DS_Store\\|.git"))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

(use-package ag
  :custom
  (ag-highlight-search t)
  (ag-reuse-buffers t)
  :config
  (add-to-list 'ag-arguments "--word-regexp"))

(use-package rainbow-delimiters
  :hook ((prod-mode . rainbow-delimiters-mode))
  :custom
  (rainbow-delimiters-max-face-count 1)
  :config
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :inherit 'error))

(use-package cider
  :diminish ""
  :bind
  ("C-c k" . cider-ns-refresh)
  :custom
  (cider-prompt-for-symbol nil)
  (cider-print-options '(("length" 50) ("right-margin" 180)))
  (cider-jdk-src-paths '("~/.java/openjv-8-src/"
                         "~/src/opensource/clojure/src/jvm"))
  (cider-eldoc-display-for-symbol-at-point nil) ; disable cider showing eldoc since clojure-lsp does this
  (cider-xref-fn-depth 90) ;; complete after lsp
  :hook
  ((cider-repl-mode . enable-paredit-mode)
   (cider-mode . (lambda () (eldoc-mode)))))


(use-package clojure-mode
  :bind
  ("C-:" . clojure-toggle-keyword-string)
  :requires (flycheck-joker flycheck-clj-konda)
  :config
  (require 'flycheck-joker)
  (require 'flycheck-clj-kondo)
  (dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
    (setq flycheck-checkers (cons checker (delq checker flycheck-checkers))))
  (dolist (checkers '((clj-kondo-clj . clojure-joker)
                      (clj-kondo-cljs . clojurescript-joker)
                      (clj-kondo-cljc . clojure-joker)
                      (clj-kondo-edn . edn-joker)))
    (flycheck-add-next-checker (car checkers) (cons 'error (cdr checkers))))
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
  :diminish ""
  :init
  (add-hook 'clojure-mode-hook (lambda ()
                                 (clj-refactor-mode 1)
                                 (yas-minor-mode 1)
                                 (cljr-add-keybindings-with-prefix "C-c C-x")))
  :custom
  (cljr-warn-on-eval nil)
  (cljr-ignore-analyzer-errors t)
  (cljr-add-ns-to-blank-clj-files nil) ; disable clj-refactor adding ns to blank files since clojure-lsp will do it
  ;; :config
  ;; (dolist (mapping '(("time" . "clj-time.core")
  ;;                    ("string" . "clojure.string")
  ;;                    ("http" . "clj-http.client")
  ;;                    ("json" . "cheshire.core")
  ;;                    ("async" . "clojure.core.async")))
  ;;   (add-to-list 'cljr-magic-require-namespaces mapping t))
  )



;; (use-package ensime)


(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
(use-package lsp-mode
  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :custom
  ;; Installed on macos using brew because emacs was too unreliable at
  ;; installing automatically
  (lsp-clojure-custom-server-command '("bash" "-c" "/usr/local/bin/clojure-lsp"))
  (lsp-auto-guess-root t)
  (lsp-keymap-prefix "C-c l")
  (lsp-prefer-flymake nil)
  (lsp-lens-enable nil)
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.emacs.d/straight\\'")

  :commands lsp)

(use-package lsp-treemacs
  :custom
  (lsp-treemacs-error-list-current-project-only t))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-show-with-cursor nil))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package lsp-java
  :custom
  (lsp-java-java-path "/Users/jmccrary/.jenv/versions/17/bin/java")
  :config
  (add-hook 'java-mode-hook 'lsp))

(use-package which-key
  :config
  (which-key-mode))

(use-package yasnippet
  :config
  (yas-global-mode t))

(use-package avy
  :bind (("C-c j" . avy-goto-word-or-subword-1)
         ("C-x j" . avy-pop-mark)))

(use-package ace-window
  :bind (("C-x o" . ace-window)))

(use-package with-editor)

(use-package magit
  :bind ("C-c g" . magit-status)
  :custom
  (magit-diff-refine-hunk t)
  (magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1))

(use-package git-timemachine)

(use-package web-mode
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


;; (use-package python
;;   :commands python-mode
;;   :interpreter ("python3" . python-mode)
;;   :custom
;;   (python-environment-virtualenv (quote ("python3" "-m" "venv"))))

;; (use-package realgud
;;   :commands realgud:pdb)

;; (use-package elpy
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
;;   :init
;;   (add-hook 'python-mode-hook 'jedi:setup)
;;   (add-hook 'python-mode-hook 'jedi:ac-setup)
;;   (setq jedi:complete-on-dot t)
;; ;  (setq jedi:setup-keys t)
;;   )

;; company-jedi wires up jedi to be a backend for the auto completion
;; library, company-mode.
;; (use-package company-jedi
;;   :config
;;   :hook
;;   ((python-mode . jedi:setup))
;;   :init
;;   (setq jedi:complete-on-dot t)
;;   (setq jedi:use-shortcuts t)
;;   (add-hook 'python-mode-hook
;;             (lambda () (add-to-list 'company-backends 'company-jedi))))

;; (use-package pyenv-mode)

;; (use-package pipenv
;;   :hook (python . pipenv-mode)
;;   :init
;;   (setq
;;    pipenv-projectile-after-switch-function
;;    #'pipenv-projectile-after-switch-extended))

(use-package rainbow-mode)

(use-package terraform-mode)

(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package olivetti
  :custom
  (olivetti-body-width 100))

(use-package go-mode)


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

(let ((machine-specific (concat user-emacs-directory "machine-specific.el")))
  (when (file-exists-p machine-specific)
    (load machine-specific)))

(provide 'init)
;;; init.el ends here
