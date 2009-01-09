(unless (boundp '*emacs-config-directory*)
  (defvar *emacs-config-directory* "~/.emacs.d"))
(unless (boundp '*joust-directory*)
  (defvar *joust-directory* (concat *emacs-config-directory* "/joust")))
(unless (boundp '*joust-packages-directory)
  (defvar *joust-packages-directory* (concat *joust-directory* "/packages")))

(defun joust:add-path (path)
  "Add the specified package to the load path"
  (add-to-list 'load-path (concat *joust-packages-directory* "/" path "/" path)))

(defun joust:list-packages ()
  "Return a list of all installed packages"
  (let ((packages (directory-files *joust-packages-directory*)))
    (remove "." (remove ".." packages))))

(defun joust:require-packages (packages)
  "Given a list of package names, load all of them from the packages directory"
  (if (not (eq nil packages))
    (let ((package (car packages)))
      (joust:add-path package)
      (require (intern package))
      (joust:require-packages (cdr packages)))))

(joust:require-packages (joust:list-packages))

(provide 'joust)

