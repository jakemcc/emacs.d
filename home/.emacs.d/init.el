
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
;; "/Users/jake/.emacs.d/"
(setq tmp-dir (file-name-as-directory (concat dotfiles-dir "tmp")))
(make-directory tmp-dir t)
;; "/Users/jake/.emacs.d/tmp/"

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

;; Load use-package and its dependencies.
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package ace-jump-mode
             :bind (("C-c SPC" . ace-jump-mode)
                    ("C-x SPC" . ace-jump-mode-pop-mark)))
