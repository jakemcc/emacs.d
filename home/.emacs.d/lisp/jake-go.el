;;; jake-go --- Go setup

;;; Commentary:
;;
;; Code from around the Internet to make editing Go files better.

;;; Author: Jake McCrary


;;; Code:

;; Run these to setup
;; go get -u github.com/nsf/gocode
;; go get -u github.com/rogpeppe/godef
;; go get -u golang.org/x/tools/cmd/goimports
;; go get -u github.com/jstemmer/gotags
;; go get github.com/matryer/moq

(use-package go-mode
  :ensure t
  :config
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'go-mode-hook 'flycheck-mode)
  (add-hook 'go-mode-hook 'dumb-jump-mode)
  (setq go-packages-function 'go-packages-go-list))

(use-package company-go
  :ensure t
  :config
  (add-hook 'go-mode-hook 'company-mode)
  (add-to-list 'company-backends 'company-go))

(use-package go-stacktracer
  :ensure t)

(use-package go-add-tags
  :ensure t)

(provide 'jake-go)
;;; jake-go.el ends here
