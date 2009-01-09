(unless (boundp '*emacs-config-path*)
  (defvar *emacs-config-path* "~/.config.d/emacs"))
(unless (boundp '*joust-path*)
  (defvar *joust-path* (concat *emacs-config-path* "/joust")))
(unless (boundp '*joust-packages-path)
  (defvar *joust-packages-path* (concat *joust-path* "/packages")))

(defun joust:add-path (path)
  (add-to-list 'load-path (concat *joust-packages-path* "/" path "/" path)))

(defun joust:list-packages ()
  (let ((packages (directory-files *joust-packages-path*)))
    (remove "." (remove ".." packages))))

(defun joust:require-packages (packages)
  (if (not (eq nil packages))
    (let ((package (car packages)))
      (joust:add-path package)
      (require (intern package))
      (joust:require-packages (cdr packages)))))

(joust:require-packages (joust:list-packages))
