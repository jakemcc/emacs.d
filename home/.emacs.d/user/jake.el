;; -*- lexical-binding: t -*-

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

(create-project-shortcuts "j/" "~/src/jakemcc")

(defvar universe "~/src/outpace/starwood/starwood_guest/universe/")


(create-project-shortcuts "o/" universe)
(create-project-shortcuts "o/" "~/src/outpace/starwood/starwood_guest/")
(create-project-shortcuts "o/" "~/src/outpace/starwood/starwood_guest/offer_engine/")


(defun o/universe ()
  (interactive)
  (find-file universe))

(defun o/scratch ()
  (interactive)
  (find-file "~/Copy/outpace/ideas.md"))

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

