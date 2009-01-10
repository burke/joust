;;; joust.el --- A better package system for emacs

;; Copyright (C) 2009 Chromium 53 <everyone@53cr.com>

;; Author: Burke Libbey <burke@53cr.com>, Ryan Neufeld <ryan@53cr.com>
;; Created: 08 Jan 2009
;; Version: 0.1
;; Keywords: tools

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Code:

(require 'cl) ;; yeah, deal with it.
;; I need it for (flet), which makes the package description DSL work.
;; Is there a better way to do this?

(defmacro joust:defvar-maybe (var val)
  `(unless (boundp ',var)
     (defvar ,var ,val)))

(joust:defvar-maybe *emacs-config-directory*      "~/.emacs.d")
(joust:defvar-maybe *joust-directory*             (concat *emacs-config-directory* "/joust"))
(joust:defvar-maybe *joust-packages-directory*    (concat *joust-directory* "/packages"))
(joust:defvar-maybe *joust-meta-directory*        (concat *joust-directory* "/meta"))
(joust:defvar-maybe *joust-system-meta-directory* (concat *joust-meta-directory* "/system"))
(joust:defvar-maybe *joust-user-meta-directory*   (concat *joust-meta-directory* "/user"))

(defun joust:package-path (package)
  (concat *joust-packages-directory* "/" package "/" package))

(defun joust:add-path (package)
  "Add the specified package to the load path"
  (add-to-list 'load-path (joust:package-path package)))

(defun joust:require-package (package)
  "Given a package name, load it from the packages directory"
  (joust:add-path package))

(defun joust:initialize ()
  "Load all packages"
  (mapcar 'joust:require-package (joust:list-installed)))

(defun joust:byte-compile-package (package)
  (byte-recompile-directory (joust:package-path package) 0 t))

(defvar joust:current-package-name nil)
(defvar joust:current-package-url nil)
(defvar joust:current-package-type nil)
(defvar joust:current-package-dependencies nil)
(defun joust:load-package-description (package)
  (flet ((name (name) (setq joust:current-package-name name))
         (url  (url)  (setq joust:current-package-url url))
         (type (type) (setq joust:current-package-type type))
         (dependencies (&rest deps) (setq joust:current-package-dependencies deps)))
    (load-file (concat *joust-system-meta-directory* "/" package ".el"))))

(defun joust:install (package)
  "Read package metadata, then fetch, install, compile, and load"
  (joust:load-package-description package)

  (let* ((package joust:current-package-name)
         (package-path (expand-file-name (concat *joust-packages-directory* "/" package))))
    (make-directory package-path t)
    (shell-command (concat "git clone \"" joust:current-package-url "\" \"" package-path "/" package "\"")))

  (joust:byte-compile-package package)
  (joust:generate-autoloads (concat *joust-packages-directory* "/" package "/" package))
  (joust:require-package package))

;; TODO: Sanitize input so evil people can't nuke ~
(defun joust:uninstall (package)
  "Remove package files."
  (let* ((package-path (expand-file-name (concat *joust-packages-directory* "/" package))))
    (shell-command (concat "rm -rf \"" package-path "\""))))

(defun joust:sync ()
  "Check the remote server for meta updates"
  (shell-command (concat "cd \"" *joust-system-meta-directory* "\"; git pull")))

(defun joust:list-installed ()
  "Return a list of all installed packages"
  (let ((packages (directory-files *joust-packages-directory*)))
    (remove "." (remove ".." packages))))

(defun joust:list-all ()
  "List uninstalled, remote packages"
  (let ((packages (directory-files *joust-system-meta-directory*)))
    (mapcar '(lambda (el) el) ;; Remove trailing .el
            (remove "." (remove ".." packages)))))

(defun joust:list-out-of-date ()
  "List out of date local packages"

  ;; Insert Magic Here.

  )


;; From Emacs 22.
(defun joust:autoload-ensure-default-file (file)
  "Make sure that the autoload file FILE exists and if not create it."
  (unless (file-exists-p file)
    (write-region
     (concat ";;; " (file-name-nondirectory file)
       " --- automatically extracted autoloads\n"
       ";;\n"
       ";;; Code:\n\n"
       "\n;; Local Variables:\n"
       ";; version-control: never\n"
       ";; no-byte-compile: t\n"
       ";; no-update-autoloads: t\n"
       ";; End:\n"
       ";;; " (file-name-nondirectory file)
       " ends here\n")
     nil file))
  file)

;; From package.el
(defun joust:generate-autoloads (pkg-dir)
  (let* ((auto-name "-autoloads.el")
   (generated-autoload-file (concat pkg-dir auto-name))
   (version-control 'never))
    ;; In Emacs 22 'update-autoloads-from-directories' does not seem
    ;; to be autoloaded...
    (require 'autoload)
    (unless (fboundp 'autoload-ensure-default-file)
      (joust:autoload-ensure-default-file generated-autoload-file))
    (update-autoloads-from-directories pkg-dir)))

(joust:initialize)
(provide 'joust)

