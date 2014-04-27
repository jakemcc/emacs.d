;; -*- lexical-binding: t -*-

(defun j/bookrobot ()
  (interactive)
  (find-file "~/src/jakemcc/bf/project.clj"))

(defun j/test-refresh ()
  (find-file "~/src/jakemcc/lein-test-refresh/test-refresh/project.clj"))
  (interactive)

(defun open-file-fn (file)
  (lambda ()
    (interactive)
    (find-file file)))

(defun create-project-shortcuts (prefix base)
  (dolist (elt (directory-files base))
    (let ((project (concat base "/" elt "/project.clj")))
      (when (file-exists-p project)
        (fset (intern (concat prefix elt)) (open-file-fn project))))))

(defvar universe "~/src/outpace/starwood/starwood_guest/universe/")

(create-project-shortcuts "o/" universe)

(defun j/universe ()
  (interactive)
  (find-file universe))

