;;; colibri-theme.el --- Colibri color theme (Vim → Emacs) -*- lexical-binding: t; -*-
;; Author: Blaž Hrastnik (original Vim scheme) + ChatGPT port
;; URL: https://github.com/whatever/colibri-emacs
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))
;; Keywords: faces

;;; Commentary:
;; A faithful(ish) port of the “colibri” Vim colorscheme.  The original
;; palette is built around soft purples/lilacs with mint + honey accents.
;; This file defines two themes:
;;   * `colibri-dark` – matches the original dark background.
;;   * `colibri-light` – based on the light transforms in the Vim source.
;;
;; To use:
;;   1.  Place this file somewhere in your load‑path, e.g. ~/.emacs.d/themes/.
;;   2.  Add to init.el:
;;         (add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))
;;         ;; then
;;         (load-theme 'colibri-dark t)  ;; or 'colibri-light
;;
;;; Code:

(defgroup colibri nil
  "Colibri theme options."
  :group 'faces)

;;;; Palette helpers
(defun colibri--hex (hex) hex)                ;; doc aid

;;;; Common palette (names match Vim file comments)
;; We map each symbolic name to a pair: (dark . light)
;; Light values are the adjusted hues from the Vim file.
(defconst colibri--colors
  '((white            . ("#FFFFFF" "#FFFFFF"))
    (white-lilac      . ("#f3f2fc" "#f3f2fc"))
    (white-lilac2     . ("#ebeafa" "#ebeafa"))
    (lilac            . ("#dbbfef" "#c590eb"))
    (lavender         . ("#a4a0e8" "#938fdb"))
    (comet            . ("#5a5977" "#aaaaaa"))
    (bossanova        . ("#452859" "#452859"))
    (midnight         . ("#3b224c" "#846897"))
    (revolver         . ("#281733" "#e8ae8b")) ;; only used for bg1 in dark
    (silver           . ("#CCCCCC" "#aaaaaa"))
    (sirocco          . ("#697C81" "#697C81"))
    (mint             . ("#9FF28F" "#6bc05b"))
    (almond           . ("#ECCDBA" "#e8ae8b"))
    (chamois          . ("#E8DCA0" "#d7c25b"))
    (honey            . ("#EFBA5D" "#EFBA5D"))
    (cedar            . ("#802F00" "#802F00"))
    (error            . ("#f47868" "#f47868"))
    (warning          . ("#ffcd1c" "#ffcd1c"))
    (diff-green       . ("#35BF86" "#35BF86"))
    (diff-red         . ("#F22C86" "#F22C86"))
    (diff-blue        . ("#6F44F0" "#3B0FBF")))
  "Alist of (name . (dark-hex light-hex)).")

(defun colibri-get (name variant)
  "Return hex string for NAME.  VARIANT is either :dark or :light."
  (let* ((pair (cdr (assq name colibri--colors)))
         (idx (if (eq variant :dark) 0 1)))
    (nth idx pair)))

;;;; Theme generator macro
(defmacro colibri-build-theme (variant)
  "Generate faces for VARIANT (:dark or :light)."
  `(custom-theme-set-faces
    ',(intern (format "colibri-%s" (substring (symbol-name variant) 1)))
    ;; Basics
    '(default ((t (:foreground ,(colibri-get 'lavender ,variant)
                               :background ,(if (eq ,variant :dark)
                                                (colibri-get 'revolver ,variant)
                                              (colibri-get 'white ,variant))))) )
    `(cursor  ((t (:background ,(colibri-get 'lavender ,variant)))))
    `(region  ((t (:background ,(colibri-get 'cedar ,variant)
                               :foreground ,(colibri-get 'white ,variant)))))
    `(highlight ((t (:background ,(colibri-get 'bossanova ,variant)))))
    ;; UI chrome
    `(fringe ((t (:background ,(colibri-get 'midnight ,variant)))))
    `(line-number ((t (:foreground ,(colibri-get 'comet ,variant)
                                   :background ,(colibri-get 'midnight ,variant)))))
    `(line-number-current-line ((t (:foreground ,(colibri-get 'almond ,variant)
                                                :background ,(if (eq ,variant :dark)
                                                                 (colibri-get 'revolver ,variant)
                                                               (colibri-get 'white-lilac2 ,variant))))))
    `(vertical-border ((t (:foreground ,(colibri-get 'bossanova ,variant)))))
    `(mode-line ((t (:background ,(colibri-get 'bossanova ,variant)
                                 :foreground ,(colibri-get 'white ,variant)
                                 :box nil))))
    `(mode-line-inactive ((t (:background ,(colibri-get 'midnight ,variant)
                                          :foreground ,(colibri-get 'silver ,variant)
                                          :box nil))))
    `(minibuffer-prompt ((t (:foreground ,(colibri-get 'lilac ,variant) :weight bold))))

    ;; Font lock
    `(font-lock-builtin-face       ((t (:foreground ,(colibri-get 'lavender ,variant)))))
    `(font-lock-comment-face       ((t (:foreground ,(colibri-get 'sirocco ,variant) :slant italic))))
    `(font-lock-constant-face      ((t (:foreground ,(colibri-get 'mint ,variant)))))
    `(font-lock-function-name-face ((t (:foreground ,(colibri-get 'lavender ,variant)))))
    `(font-lock-keyword-face       ((t (:foreground ,(colibri-get 'almond ,variant)))))
    `(font-lock-string-face        ((t (:foreground ,(colibri-get 'silver ,variant)))))
    `(font-lock-type-face          ((t (:foreground ,(colibri-get 'lavender ,variant)))))
    `(font-lock-variable-name-face ((t (:foreground ,(colibri-get 'lavender ,variant)))))
    `(font-lock-warning-face       ((t (:foreground ,(colibri-get 'warning ,variant) :weight bold))))

    ;; Diagnostics
    `(error   ((t (:foreground ,(colibri-get 'error ,variant)   :weight bold))))
    `(warning ((t (:foreground ,(colibri-get 'warning ,variant) :weight bold))))
    `(success ((t (:foreground ,(colibri-get 'diff-green ,variant)))))

    ;; Diff / VC
    `(diff-added   ((t (:background ,(colibri-get 'diff-green ,variant) :foreground ,(colibri-get 'white ,variant)))))
    `(diff-removed ((t (:background ,(colibri-get 'diff-red ,variant)   :foreground ,(colibri-get 'white ,variant)))))
    `(diff-changed ((t (:background ,(colibri-get 'diff-blue ,variant)  :foreground ,(colibri-get 'white ,variant)))))

    ;; Links / markup
    `(link ((t (:foreground ,(colibri-get 'mint ,variant) :underline t))))
    `(show-paren-match ((t (:background ,(colibri-get 'cedar ,variant) :weight bold))))
    ;; etc – extend as needed
    ))

;;;; Define actual themes
(deftheme colibri-dark "Colibri theme – dark background.")
(colibri-build-theme :dark)

(deftheme colibri-light "Colibri theme – light background.")
(colibri-build-theme :light)

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (let ((dir (file-name-directory load-file-name)))
    (add-to-list 'custom-theme-load-path dir)))

(provide-theme 'colibri-dark)
(provide-theme 'colibri-light)

;;; colibri-theme.el ends here
