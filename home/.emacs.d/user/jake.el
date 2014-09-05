;; -*- lexical-binding: t -*-


(install-package 'projectile)
(install-package 'flx-ido)

(projectile-global-mode)

(defun j/test-refresh ()
  (interactive)
  (find-file "~/src/jakemcc/lein-test-refresh/test-refresh/project.clj"))

(defun open-file-fn (file)
  (lambda ()
    (interactive)
    (find-file file)))

(defun create-project-shortcuts (prefix base)
  (dolist (elt (directory-files base))
    (let ((project (concat base "/" elt "/project.clj")))
      (when (file-exists-p project)
        (fset (intern (concat prefix elt)) (open-file-fn project))))))

(defvar starwood_guest "~/src/outpace/starwood/starwood_guest/")
(defvar universe (concat starwood_guest "universe/"))

(defun refresh-project-shortcuts ()
  (interactive)
  (create-project-shortcuts "j/" "~/src/jakemcc")
  (create-project-shortcuts "o/" universe)
  (create-project-shortcuts "o/" starwood_guest)
  (create-project-shortcuts "o/" "~/src/outpace/starwood/starwood_guest/offer_engine/"))

(refresh-project-shortcuts)

(defun o/universe ()
  (interactive)
  (find-file universe))

(defun o/starwood_guest ()
  (interactive)
  (find-file starwood_guest))

(defun o/scratch ()
  (interactive)
  (find-file "~/Copy/outpace/ideas.org"))

(defun o/log ()
  (interactive)
  (find-file "~/Copy/outpace/log.org"))

(defun b/insert-clojure-code (arg)
  (interactive "p")
  (insert "``` clojure
```")
  (beginning-of-line)
  (open-line arg))

(defun b/insert-console-code (arg)
  (interactive "p")
  (insert "``` console
```")
  (beginning-of-line)
  (open-line arg))
