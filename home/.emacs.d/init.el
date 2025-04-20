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

(defvar native-comp-deferred-compilation-deny-list nil)
(setq native-comp-async-report-warnings-errors nil)


(defvar dotfiles-dir)

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(defvar tmp-dir)
(setq tmp-dir (file-name-as-directory (concat dotfiles-dir "tmp")))
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(make-directory tmp-dir t)

(setq straight-repository-branch "develop")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package straight
  :custom (straight-use-package-by-default t))

(use-package gcmh
  :ensure t
  :demand t
  :diminish gcmh-mode
  :config
  (gcmh-mode 1))

(defun jake/fit-other-window-to-buffer ()
  (interactive)
  (ace-window t)
  (fit-window-to-buffer)
  (ace-window t))

(defun jm/choose-font-size ()
  "Choose between three different font sizes: 16, 18, and 20."
  (interactive)
  (let ((sizes '(16 18 20)))
    (set-face-attribute 'default nil :height
                        (* 10 (string-to-number
                               (completing-read "Choose font size: " (mapcar #'number-to-string sizes)))))))

(use-package emacs
  :straight nil
  :custom
  (inhibit-startup-screen t)
  ;; Taken from http://stackoverflow.com/questions/2081577/setting-emacs-split-to-horizontal
  (split-height-threshold nil)
  (split-width-threshold 200)
  (tab-always-indent 'complete)
  (enable-recursive-minibuffers t)
  :bind (("C-x -" . fit-window-to-buffer)
         ("C-x _" . jake/fit-other-window-to-buffer)
         ("C-x C-m" . execute-extended-command)
         ("C-x m" . execute-extended-command))
  :config
  (set-face-attribute 'default nil :font "Inconsolata-20")
  ;; (set-frame-font "Inconsolata-20" nil t)
  ;; (set-face-attribute 'default nil :font "Inconsolata-18")
  ;; (set-frame-font "Inconsolata-18" nil t)
  (global-auto-revert-mode)
  (global-display-line-numbers-mode)

  (menu-bar-mode -1)
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

  
  (add-to-list 'auto-mode-alist '("\\.fig\\'" . (lambda ()
                                                  (setq-local comment-start "#")
                                                  (setq-local comment-end "")))))

(use-package num3-mode
  :config
  ;; (set-face-attribute 'num3-face-odd nil :background nil)
  (set-face-attribute 'num3-face-even nil
                      :underline t
                      :weight 'normal
                      :background nil)
  (global-num3-mode))

(use-package whitespace
  :straight (:type built-in)
  :custom
  (whitespace-style '(face lines-char))
  (whitespace-line-column 86)
  :init
  (add-hook 'clojure-mode-hook 'whitespace-mode))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(use-package uniquify
  :straight (:type built-in)
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package saveplace
  :straight (:type built-in)
  :config
  (setq-default save-place t))

;; (defadvice he-substitute-string (after he-paredit-fix)
;;   "remove extra paren when expanding line in paredit"
;;   (if (and paredit-mode (equal (substring str -1) ")"))
;;       (progn (backward-delete-char 1) (forward-char))))

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)

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
  :diminish
  :custom
  (super-save-remote-files nil)
  (super-save-auto-save-when-idle nil)
  :config
  (super-save-mode +1)
  (add-to-list 'super-save-triggers 'ace-window)
  (add-to-list 'super-save-hook-triggers 'find-file-hook)
  ;; turn off built in auto-save
  (setq auto-save-default nil))


;; ;; (use-package system-packages
;; ;;   :custom (system-packages-package-manager (quote brew)))

;; (use-package use-package-ensure-system-package)


(use-package visual-regexp-steroids
  :custom
  (vr/engine 'emacs)
  :bind (("M-%" . vr/query-replace)
         ("C-r" . vr/isearch-backward)))

;; marginalia, all-the-icons-completion, vertico all inspired from https://kristofferbalintona.me/posts/202202211546/
;; archive.is: https://archive.ph/fQeRP
(use-package marginalia
  :after vertico
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode)
  ;; (setq marginalia-command-categories
  ;;       (append '((projectile-find-file . project-file)
  ;;                 (projectile-find-dir . project-file)
  ;;                 (projectile-switch-project . file))
  ;;               marginalia-command-categories))
  )

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)        ;; pick some comfortable binding
   ("C-;" . embark-dwim)       ;; good alternative: M-.
   ("C-h B" . embark-bindings) ;; alternative for `describe-bindings'

   :map vertico-map
   ("C-c C-o" . embark-export)

   :map isearch-mode-map
   ("C-c C-o" . embark-export))
  
  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-flycheck)
(use-package emacsql :ensure t)
(use-package consult-org-roam
  :after org-roam
  :custom
  ;; Use `ripgrep' for searching with `consult-org-roam-search'
  (consult-org-roam-grep-func #'consult-ripgrep)
  ;; Configure a custom narrow key for `consult-buffer'
  (consult-org-roam-buffer-narrow-key ?r)
  ;; Display org-roam buffers right after non-org-roam buffers
  ;; in consult-buffer (and not down at the bottom)
  (consult-org-roam-buffer-after-buffers t)
  :config
  ;; Eventually suppress previewing for certain functions
  (consult-customize
   consult-org-roam-forward-links
   :preview-key (kbd "M-."))
  :bind
  ;; Define some convenient keybindings as an addition
  ("C-c n e" . consult-org-roam-file-find)
  ("C-c n b" . consult-org-roam-backlinks)
  ("C-c n l" . consult-org-roam-forward-links)
  ("C-c n r" . consult-org-roam-search))

(use-package wgrep
  :ensure t)

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark) ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop) ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck) ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)   ;; orig. goto-line
         ("M-g M-g" . consult-goto-line) ;; orig. goto-line
         ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
  
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("C-s" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history) ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
         ("C-s" . consult-line) ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history) ;; orig. next-matching-history-element
         ("M-r" . consult-history)) ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
;;;; 2. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  )

(use-package all-the-icons
  :if (display-graphic-p))
;; Must install all the fonts from fonts directory as well. Can try invoking (all-the-icons-install-fonts) to so

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package vertico
  :custom
  (vertico-count 13) ; Number of candidates to display
  (vertico-resize t)
  (vertico-cycle nil) ; Go from last to first candidate and first to last (cycle)?
  :config
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

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
  :hook
  (org-mode . visual-line-mode)
  ;; (org-mode . variable-pitch-mode)
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
      "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"))))

(use-package ox-gfm
  :ensure t)

;; Take from https://stackoverflow.com/questions/17435995/paste-an-image-on-clipboard-to-emacs-org-mode-file-without-saving-it
(defun jm/my-org-screenshot ()
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
  (org-journal-new-entry t)
  (org-journal-create-new-id)
  (org-journal-new-entry prefix))

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
  (projectile-update-project-type
   'lein-test
   :related-files-fn
   (list
    (projectile-related-files-fn-test-with-suffix "clj" "_test")
    (projectile-related-files-fn-test-with-suffix "clj" "_expectations")))
  :init (projectile-mode)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :custom
  (projectile-project-search-path '(("~/src/" . 2)
                                    ("~/drwsrc/beefalo/" . 1)
                                    ("~/drwsrc-github.com/" . 2)))
  (projectile-completion-system 'default)
;  (projectile-enable-caching )
  (projectile-file-exists-remote-cache-expire (* 10 60))
  (projectile-file-exists-local-cache-expire (* 10 60)))

(use-package makefile-executor
  :config
  (add-hook 'makefile-mode-hook 'makefile-executor-mode))

(use-package consult-projectile
  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master"))


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

;; (use-package swiper
;;   :bind (("C-s" . swiper)
;;                                         ;         ("M-*" . swiper-under-point)
;;          ))

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
  (flycheck-checker-error-threshold 900)
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
  (add-to-list 'exec-path-from-shell-variables "GOPATH")
  (add-to-list 'exec-path-from-shell-variables "LSP_USE_PLISTS")
  (add-to-list 'exec-path-from-shell-variables "FIG_REMOTE_URL")
  (exec-path-from-shell-initialize))

;; Stop typing full "yes or no" answers to Emacs.
(defalias 'yes-or-no-p 'y-or-n-p)

(defun save-all ()
  "Save all buffers."
  (interactive)
  (save-some-buffers t))
(add-hook 'save-buffer 'save-all)


;; apperance



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




;; (use-package idle-highlight-mode
;;   :hook ((prog-mode . idle-highlight-mode))
;;   :custom
;;   (idle-highlight-idle-time 0.1))

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
  :disabled t
  :config
  (load-theme 'sanityinc-tomorrow-eighties t)
  (set-face-foreground 'region "white")
  (set-face-background 'region "blue"))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; (load-theme 'doom-gruvbox t)
  ;; (load-theme 'doom-laserwave t) ;; kind of nice, line numbers a bit hard
  ;; (load-theme 'doom-miramare t) ;; pretty good
  (load-theme 'doom-oceanic-next t) ;; also good
  ;; (load-theme 'doom-shades-of-purple	t)
  ;; (load-theme 'doom-feather-dark t) ;; kind of a dark purple theme
  ;; Enable flashing mode-line on errors
  ;;(doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  ;;(setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;;  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  ;;(doom-themes-org-config)
  )

(use-package paredit
  :diminish ""
  :hook
  ((clojure-mode . enable-paredit-mode)
   (emacs-lisp-mode . enable-paredit-mode)
   (lisp-interaction-mode . enable-paredit-mode)
   (lisp-mode . enable-paredit-mode)
   (scheme-mode . enable-paredit-mode))
  :bind (:map paredit-mode-map
              ("RET" . nil)
              ("C-j" . paredit-newline)
              ("M-)" . paredit-forward-slurp-sexp)
              ("M-(" . paredit-forward-slurp-sexp)
              ("M-}" . paredit-forward-barf-sexp)
              ("M-{" . paredit-backward-barf-sexp)))


;; (use-package counsel
;;   :bind*
;;   (("M-x" . counsel-M-x)
;;    ;; ("C-c C-m" . counsel-M-x)
;;    ("C-x C-m" . counsel-M-x)
;;    ;;("C-x m" . counsel-M-x)
;;    ("C-x C-f" . counsel-find-file))
;;   :custom
;;   (counsel-find-file-ignore-regexp "\\.DS_Store\\|.git"))

;; (use-package counsel-projectile
;;   :config
;;   (counsel-projectile-mode))

(use-package ripgrep)

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

;; corfu, orderless, kind-icons inspired from https://kristofferbalintona.me/posts/202202270056/
;; archive link: https://archive.ph/tEfT3
(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.25)

  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)     ; Always have the same width
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-cycle nil)

  (corfu-quit-at-boundry nil)
  (corfu-separator ?\s)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current 'insert)
  (corfu-preselect-first t)

  (corfu-echo-documentation nil) ;; using corfu-doc for this

  :bind
  (:map corfu-map
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)
        ("SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode)
  :config
  ;; Enable Corfu more generally for every minibuffer, as long as no other
  ;; completion UI is active. If you use Mct or Vertico as your main minibuffer
  ;; completion UI. From
  ;; https://github.com/minad/corfu#completing-with-corfu-in-the-minibuffer
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input)
                (eq (current-local-map) read-passwd-map))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1))

;; (use-package corfu-doc
;;   ;; NOTE 2022-02-05: At the time of writing, `corfu-doc' is not yet on melpa
;;   :straight (corfu-doc :type git :host github :repo "galeo/corfu-doc")
;;   :after corfu
;;   :hook (corfu-mode . corfu-doc-mode)
;;   :bind
;;   (:map corfu-map
;;         ("M-h" . corfu-doc-toggle)     ; Remap the default doc command
;;         ("M-n" . corfu-doc-scroll-up)
;;         ("M-p" . corfu-doc-scroll-down))
;;   :custom
;;   (corfu-doc-delay 0.5)
;;   (corfu-doc-max-width 70)
;;   (corfu-doc-max-height 20)
;;   (corfu-echo-documentation nil))


(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil) ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Enable `kind-icon'

  ;; Add hook to reset cache so the icon colors match my theme
  ;; NOTE 2022-02-05: This is a hook which resets the cache whenever I switch
  ;; the theme using my custom defined command for switching themes. If I don't
  ;; do this, then the backgound color will remain the same, meaning it will not
  ;; match the background color corresponding to the current theme. Important
  ;; since I have a light theme and dark theme I switch between. This has no
  ;; function unless you use something similar
  ;;(add-hook 'kb/themes-hooks #'(lambda () (interactive) (kind-icon-reset-cache)))
  )

;; Use Dabbrev with Corfu! (from corfu readme)
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode))

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package cape)

(use-package js
  :straight (:type built-in)
  :custom
  (js-indent-level 2))

(use-package cider
  :diminish ""
  :bind
  ("C-c k" . cider-ns-refresh)
  :custom
  (cider-reuse-dead-repls 'auto)
  (cider-prompt-for-symbol nil)
  (cider-repl-pop-to-buffer-on-connect nil)
  (cider-print-options '(("length" 50) ("right-margin" 180)))
  (cider-jdk-src-paths '("~/.java/openjv-8-src/"
                         "~/src/opensource/clojure/src/jvm"))
  (cider-eldoc-display-for-symbol-at-point nil) ; disable cider showing eldoc since clojure-lsp does this
  (cider-xref-fn-depth 90)                      ;; complete after lsp
  (nrepl-use-ssh-fallback-for-remote-hosts t)
  (clojure-toplevel-inside-comment-form t) ;; don't treat comment block as the toplevel s-exp
  :hook
  ((cider-repl-mode . enable-paredit-mode)
   (cider-mode . (lambda () (eldoc-mode)))))

(use-package clojure-mode
  :bind
  ("C-:" . clojure-toggle-keyword-string)
  :requires (flycheck-joker flycheck-clj-kondo)
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
  (put-clojure-indent 'match 1))


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

(defun jm/toggle-window-split ()
  "Toggle between horizontal and vertical split for two windows. Thanks ChatGPT"
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;;TODO: this is not correct
(defun jake/window-is-small? ()
  (let ((half-height (/ (window-total-height (frame-root-window)) 2)))
    (print half-height)
    (print (window-total-height))
    (< (+ (window-total-height) 10) half-height)))

(defun jake/sometimes-fit-window-to-buffer ()
  (interactive)
  (when (jake/window-is-small?)
    (fit-window-to-buffer)))

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
(setq lsp-use-plists t)
(use-package lsp-mode
  :custom
  (lsp-use-plists t)
  (lsp-completion-provider :none) ;; use corfu instead of default for lsp completions
  ;; Installed on macos using brew because emacs was too unreliable at
  ;; installing automatically
  (lsp-clojure-custom-server-command (if (file-exists-p "/usr/local/bin/clojure-lsp")
                                         '("bash" "-c" "/usr/local/bin/clojure-lsp")
                                       '("bash" "-c" "/opt/homebrew/bin/clojure-lsp")))
  ;; (lsp-clojure-custom-server-command "/Users/jake/.bin/clojure-lsp-dev")
  (lsp-auto-guess-root t)
  (lsp-keymap-prefix "C-c l")
  (lsp-prefer-flymake nil)
  (lsp-lens-enable t)
  (lsp-idle-delay 0.2)
  (lsp-signature t)
  ;; (lsp-enable-on-type-formatting nil)
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex))) ;; Configure flex
  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp)
         (c-mode . lsp)
         (c++-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         (js-mode . lsp)
         (lsp-completion-mode . my/lsp-mode-setup-completion))
  :config
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.emacs.d/straight\\'")
  ;;  (advice-add 'lsp-find-references :after 'jake/sometimes-fit-window-to-buffer)
  :commands lsp)

(use-package lsp-treemacs
  :custom
  (lsp-treemacs-error-list-current-project-only t))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-show-with-cursor nil))

(use-package lsp-java
  :config
  (add-hook 'java-mode-hook 'lsp))

(use-package apheleia
  :straight (apheleia :host github :repo "raxod502/apheleia")
  :config
  (setf (alist-get 'zprint apheleia-formatters)
        '("zprint" "{:style [:community] :map {:comma? false}}" "--write" inplace))
  (setf (alist-get 'clojure-mode apheleia-mode-alist) 'zprint
        (alist-get 'clojure-ts-mode apheleia-mode-alist) 'zprint)
  (apheleia-global-mode t))

(use-package which-key
  :diminish
  :config
  (which-key-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode t))

(use-package avy
  :bind (("C-c j" . avy-goto-char-timer)
         ("C-x j" . avy-pop-mark)))

(use-package ace-window
  :bind (("C-x o" . ace-window)))

(use-package with-editor)

(use-package magit
  :bind ("C-c g" . magit-status)
  :custom
  (magit-diff-refine-hunk t)
  (magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1
                                 ;magit-display-buffer-fullframe-status-v1
                                 ))

(use-package git-timemachine
  :straight (git-timemachine :fetcher codeberg :repo "pidu/git-timemachine"))

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

(use-package cmake-mode)

(c-add-style "guessed"
             '("linux"
               (c-basic-offset . 4)     ; Guessed value
               (c-offsets-alist
                (arglist-cont . 0)          ; Guessed value
                (arglist-intro . ++)        ; Guessed value
                (block-close . 0)           ; Guessed value
                (case-label . +)            ; Guessed value
                (defun-block-intro . +)     ; Guessed value
                (defun-close . 0)           ; Guessed value
                (defun-open . 0)            ; Guessed value
                (else-clause . 0)           ; Guessed value
                (member-init-cont . 0)      ; Guessed value
                (member-init-intro . 5)     ; Guessed value
                (statement . 0)             ; Guessed value
                (statement-block-intro . +) ; Guessed value
                (statement-case-intro . +)  ; Guessed value
                (substatement-open . 0)     ; Guessed value
                (topmost-intro . 0)         ; Guessed value
                (topmost-intro-cont . 0)    ; Guessed value
                (access-label . -)
                (annotation-top-cont . 0)
                (annotation-var-cont . +)
                (arglist-close . c-lineup-close-paren)
                (arglist-cont-nonempty . c-lineup-arglist)
                (block-open . 0)
                (brace-entry-open . 0)
                (brace-list-close . 0)
                (brace-list-entry . 0)
                (brace-list-intro . +)
                (brace-list-open . 0)
                (c . c-lineup-C-comments)
                (catch-clause . 0)
                (class-close . 0)
                (class-open . 0)
                (comment-intro . c-lineup-comment)
                (composition-close . 0)
                (composition-open . 0)
                (cpp-define-intro c-lineup-cpp-define +)
                (cpp-macro . -1000)
                (cpp-macro-cont . +)
                (do-while-closure . 0)
                (extern-lang-close . 0)
                (extern-lang-open . 0)
                (friend . 0)
                (func-decl-cont . +)
                (inclass . +)
                (incomposition . +)
                (inexpr-class . +)
                (inexpr-statement . +)
                (inextern-lang . +)
                (inher-cont . c-lineup-multi-inher)
                (inher-intro . +)
                (inlambda . 0)
                (inline-close . 0)
                (inline-open . +)
                (inmodule . +)
                (innamespace . +)
                (knr-argdecl . 0)
                (knr-argdecl-intro . 0)
                (label . 0)
                (lambda-intro-cont . +)
                (module-close . 0)
                (module-open . 0)
                (namespace-close . 0)
                (namespace-open . 0)
                (objc-method-args-cont . c-lineup-ObjC-method-args)
                (objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
                (objc-method-intro .
                                   [0])
                (statement-case-open . 0)
                (statement-cont . +)
                (stream-op . c-lineup-streamop)
                (string . -1000)
                (substatement . +)
                (substatement-label . 0)
                (template-args-cont c-lineup-template-args +))))

(use-package cc-mode
  :defer
  :hook ((c-mode-hook . lsp)
         (c++-mode-hook . lsp))
  :config
  (add-to-list 'c-default-style '(c++-mode . "guessed"))

  (use-package clang-format
    :if (executable-find "clang")
    :bind
    (:map c-mode-base-map
          ("C-x =" . clang-format-region))))

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

(defun lein-autoexpect ()
  (interactive)
  (let ((buffer (get-buffer "*lein-autoexpect*")))
    (when (not buffer)
      (setq buffer (compile "EXPECTATIONS_COLORIZE=false ./lein autoexpect"))
      (with-current-buffer buffer
        (rename-buffer "*lein-autoexpect*")))
    buffer))

(let ((machine-specific (concat user-emacs-directory "machine-specific.el")))
  (when (file-exists-p machine-specific)
    (load machine-specific)))

(provide 'init)
;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
