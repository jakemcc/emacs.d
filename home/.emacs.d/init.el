;; PACKAGES
;;--------------------------------------------------

;; dir to store all extra extensions
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(setq tmp-dir (file-name-as-directory (concat dotfiles-dir "tmp")))
(make-directory tmp-dir t)

(package-initialize)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(
                      starter-kit
                      starter-kit-lisp
                      starter-kit-bindings
                      starter-kit-ruby
                      starter-kit-js
                      starter-kit-eshell
                      highlight
                      clojure-mode
                      clojurescript-mode
                      clj-refactor
                      coffee-mode
                      markdown-mode
                      highlight-symbol
                      cider
                      ac-nrepl
                      exec-path-from-shell
                      yaml-mode
                      ace-jump-mode
                      auto-complete
                      popup
                      fuzzy
                      git-messenger
                      restclient
                      )
  "A list of packages to ensure are installed at launch.")

(defun install-package (package)
  (when (not (package-installed-p package))
    (package-install package)))

(dolist (p my-packages)
  (install-package p))

;; ENVIRONMENT
;;--------------------------------------------------

(when (memq window-system '(mac ns))
  (x-focus-frame nil)
  (exec-path-from-shell-initialize))

(defun load-system-specific-configs (postfix)
  "Load system specific/user specific files if around"
  (setq system-specific-config (concat dotfiles-dir "user/" system-name postfix ".el")
        user-specific-config (concat dotfiles-dir "user/" user-login-name postfix ".el")
        user-specific-dir (concat dotfiles-dir "user/" user-login-name postfix))
  (add-to-list 'load-path user-specific-dir)

  (if (file-exists-p system-specific-config) (load system-specific-config))
  (if (file-exists-p user-specific-config) (load user-specific-config))
  (if (file-exists-p user-specific-dir)
      (mapc #'load (directory-files user-specific-dir nil ".*el$"))))

(load-system-specific-configs "")

(setq vendor-dir (concat dotfiles-dir "/vendor"))
(add-to-list 'load-path vendor-dir)

(setq ispell-program-name "aspell")

;; CODING STYLES
;;--------------------------------------------------

;; smooth-scrolling stops that annoying jump when moving around
(require 'smooth-scrolling)

;; makes sexps flash when you eval them!
(require 'highlight)
(require 'eval-sexp-fu)
(require 'nrepl-eval-sexp-fu)
(setq nrepl-eval-sexp-fu-flash-duration 0.5)

;; use inconsolata
(set-face-attribute 'default nil
                    :family "Inconsolata"
                    :height 160)

;; show line numbers
(global-linum-mode t)

;; tabs are 2 spaces
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; color theme
(add-to-list 'custom-theme-load-path (concat dotfiles-dir "themes"))
(load-theme 'zenburn t)

(set-face-foreground 'region "white")
(set-face-background 'region "blue")

;; KEYBINDINGS
;;--------------------------------------------------


;; steve yegges's suggested keybindings
(global-set-key "\C-x\C-m" 'smex)
(global-set-key "\C-c\C-m" 'smex)

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(global-set-key [f5] 'call-last-kbd-macro)

;; find file in project
(require 'find-file-in-project)
(add-to-list 'ffip-patterns "*\.cljs")
(add-to-list 'ffip-patterns "*\.coffee")

;; slime and paredit
(defun fix-paredit-repl ()
  (interactive)
  (local-set-key "{" 'paredit-open-curly)
  (local-set-key "}" 'paredit-close-curly)
  (modify-syntax-entry ?\{ "(}")
  (modify-syntax-entry ?\} "){")
  (modify-syntax-entry ?\[ "(]")
  (modify-syntax-entry ?\] ")["))

(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1) (fix-paredit-repl)))

(add-hook 'clojure-mode-hook (lambda () (paredit-mode +1)))

(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-x")))
(add-hook 'clojure-mode-hook (lambda () (yas/minor-mode 1)))

;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)


;; nrepl
(add-hook 'cider-interaction-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-mode-hook (lambda ()
                             (cider-turn-on-eldoc-mode)
                             (paredit-mode +1)
                             (fix-paredit-repl)))
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(setq cider-repl-popup-stacktraces t)
(setq cider-auto-select-error-buffer t)
(setq cider-repl-wrap-history t)
;; (setq cider-repl-history-file ".cider-repl-history")

(require 'ac-nrepl)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-interaction-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-mode))
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

;; Use ac-nrepl-popup-doc to show in-line docs in a clojure buffer
(eval-after-load "cider"
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))

;; Use ac-nrepl-popup-doc to show in-line docs in an nrepl buffer
(eval-after-load "cider"
  '(define-key cider-repl-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))

;; specify the print length to be 100 to stop infinite sequences
;; killing things.
(setq cider-repl-print-length 100)


;; auto-complete configuration
(require 'popup)
(require 'fuzzy)
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(ac-flyspell-workaround)
(setq ac-comphist-file (concat tmp-dir "ac-comphist.dat"))
(global-auto-complete-mode t)
(setq ac-auto-show-menu t)
(setq ac-dwim t)
(setq ac-quick-help-delay 1)
(setq ac-quick-help-height 60)
(setq ac-disable-inline t)
(setq ac-show-menu-immediately-on-auto-complete t)
(setq ac-auto-start 2)
(setq ac-candidate-menu-min 0)

(set-default 'ac-sources
             '(ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-semantic))



(dolist (mode '(magit-log-edit-mode log-edit-mode org-mode text-mode haml-mode
                                    sass-mode yaml-mode csv-mode espresso-mode haskell-mode
                                    html-mode nxml-mode sh-mode smarty-mode clojure-mode
                                    lisp-mode textile-mode markdown-mode tuareg-mode))
  (add-to-list 'ac-modes mode))

;; erc
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; markdown
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; ace jump mode
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

(global-auto-revert-mode 1)

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files"
  (interactive)
  (let* ((list (buffer-list))
         (buffer (car list)))
    (while buffer
      (when (and (buffer-file-name buffer)
                 65          (not (buffer-modified-p buffer)))
        (set-buffer buffer)
        (revert-buffer t t t))
      (setq list (cdr list))
      (setq buffer (car list))))
  (message "Refreshed open files"))


;; kibit
;; Teach compile the syntax of the kibit output
(require 'compile)
(add-to-list 'compilation-error-regexp-alist-alist
             '(kibit "At \\([^:]+\\):\\([[:digit:]]+\\):" 1 2 nil 0))
(add-to-list 'compilation-error-regexp-alist 'kibit)

;; A convenient command to run "lein kibit" in the project to which
;; the current emacs buffer belongs to.
(defun kibit ()
  "Run kibit on the current project.
Display the results in a hyperlinked *compilation* buffer."
  (interactive)
  (compile "lein kibit"))


(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; rename file and buffer
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

;; python jedi

;; need to have epc and jedi installed
;; (setq jedi:setup-keys t)
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (add-hook 'python-mode-hook 'auto-complete-mode)
;; (put 'upcase-region 'disabled nil)


;; tern
;; (add-to-list 'load-path "~/src/github/tern/emacs/")
;; (autoload 'tern-mode "tern.el" nil t)
;; (add-hook 'js-mode-hook (lambda () (tern-mode t)))



;; git-messenger
(require 'git-messenger)

(defun byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-directory user-emacs-directory))

(defun scratch-buffer ()
  (interactive)
  (switch-to-buffer (make-temp-name "scratch")))


(load-system-specific-configs "-after")
