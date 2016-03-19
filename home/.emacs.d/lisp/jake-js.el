;;; jake-js --- Special JavaScript tweaks

;;; Commentary:
;;
;; Code from around the Internet to make editing JavaScript files better.

;;; Author: Jake McCrary


;;; Code:

(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

(defun esk-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))

;; https://github.com/dandavison/paredit-c/blob/master/paredit-c.el#L46
(defun paredit-c/singlequote (&optional n)
  "Copied from `paredit-doublequote`."
  (interactive "P")

  (if (null paredit-mode)
      (insert "'")
    (cond ((paredit-in-string-p)
           (if (eq (cdr (paredit-string-start+end-points))
                   (point))
               (forward-char) ; We're on the closing quote.
             (insert ?\\ ?\' )))
          ((paredit-in-comment-p)
           (insert ?\' ))
          ((not (paredit-in-char-p))
           (paredit-insert-pair n ?\' ?\' 'paredit-forward-for-quote)))))

(eval-after-load 'js
  '(progn (define-key js-mode-map "{" 'paredit-open-curly)
          (define-key js-mode-map "}" 'paredit-close-curly-and-newline)
          (define-key js-mode-map "'" 'paredit-c/singlequote)
          (add-hook 'js-mode-hook 'esk-paredit-nonlisp)
          (setq js-indent-level 2)
          ;; fixes problem with pretty function font-lock
          (define-key js-mode-map (kbd ",") 'self-insert-command)
          (font-lock-add-keywords
           'js-mode `(("\\(function *\\)("
                       (0 (progn (compose-region (match-beginning 1)
                                                 (match-end 1) "\u0192")
                                 nil)))))))

(provide 'jake-js)
;;; jake-js.el ends here
