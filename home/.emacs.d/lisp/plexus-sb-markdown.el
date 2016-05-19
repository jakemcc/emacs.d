;;; plexus-sb-markdown --- edit source blocks in markdown
;;;
;;; Commentary:
;;  - https://github.com/plexus/.emacs.d/blob/master/init.el#L327-L380




;;; Code:

(defvar plexus/restore-mode-map (make-sparse-keymap)
  "Keymap while plexus/restore-mode is active.")

(define-minor-mode plexus/restore-mode
  "A temporary minor mode to go back to the markdown you're editing"
  nil
  :lighter " ♻"
  plexus/restore-mode-map)

(defun plexus/edit-md-source-block ()
  (interactive)
  (let ((buffer nil))
    (save-excursion
      (re-search-backward "\n```\[a-z- \]+\n")
      (re-search-forward "\n``` *")
      (let ((lang (thing-at-point 'word))
            (md-buffer (current-buffer)))
        (forward-line)
        (let ((start (point)))
          (re-search-forward "\n```")
          (let* ((end (- (point) 4))
                 (source (buffer-substring-no-properties start end)))
            (setq buffer (get-buffer-create (concat "*markdown-" lang "*")))
            (set-buffer buffer)
            (erase-buffer)
            (insert source)
            (setq restore-start start)
            (setq restore-end end)
            (setq restore-buffer md-buffer)
            (make-local-variable 'restore-start)
            (make-local-variable 'restore-end)
            (make-local-variable 'restore-buffer)
            (funcall (intern (concat lang "-mode")))))))
    (switch-to-buffer buffer)
    (plexus/restore-mode 1)))


(defun plexus/restore-md-source-block ()
  (interactive)
  (let ((contents (buffer-string)))
    (save-excursion
      (set-buffer restore-buffer)
      (delete-region restore-start restore-end)
      (goto-char restore-start)
      (insert contents)))
  (switch-to-buffer restore-buffer))

(bind-key (kbd "C-c '") 'plexus/edit-md-source-block markdown-mode-map)
(bind-key (kbd "C-c '") 'plexus/restore-md-source-block plexus/restore-mode-map)

(provide 'plexus-sb-markdown)
;;; plexus-sb-markdown.el ends here
