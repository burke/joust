(defvar *emacs-config-path* "~/.config.d/emacs")
(defvar *joust-path* (concat *emacs-config-path* "/joust"))
(defvar *joust-packages-path* (concat *joust-path* "/packages"))

(defun add-path (path)
  (add-to-list 'load-path (concat *joust-packages-path* "/" path "/" path)))

(defun list-packages ()
  (let ((packages (directory-files *joust-packages-path*)))
    (remove "." (remove ".." packages))))

(defun require-packages (packages)
  (if (not (eq nil packages))
    (let ((package (car packages)))
      (add-path package)
      (require (intern package))
      (require-packages (cdr packages)))))

(require-packages (list-packages))

