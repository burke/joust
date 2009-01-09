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

(require 'cl)

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

(defun joust:list-packages ()
  "Return a list of all installed packages"
  (let ((packages (directory-files *joust-packages-directory*)))
    (remove "." (remove ".." packages))))

(defun joust:require-package (package)
  "Given a package name, load it from the packages directory"
  (joust:add-path package)
  (require (intern package)))

(defun joust:initialize ()
  "Load all packages"
  (mapcar 'joust:require-package (joust:list-packages)))

(defun joust:byte-compile-package (package)
  (byte-recompile-directory (joust:package-path package) 0 t))

;;; PACKAGE DESCRIPTION LOADING ;;;;;;;;;;;;
(defun joust:load-package-description (package)
  (flet ((name (name) (setq joust:current-package-name name))
         (url  (url)  (setq joust:current-package-url url))
         (type (type) (setq joust:current-package-type type))
         (dependencies (&rest deps) (setq joust:current-package-dependencies deps)))
    (load-file (concat *joust-system-meta-directory* "/" package ".el"))))
;;; END PACKAGE DESCRIPTION LOADING ;;;;;;;;

(joust:load-package-description "gist")


(defun joust:install-package (package)
  "Read package metadata, then fetch, install, compile, and load"

  ;; Insert Magic Here.

  (joust:byte-compile-package package)
  (joust:require-package package))

(defun joust:uninstall-package (package)
  "Remove package files."

  ;; Insert Magic Here.

  )

(joust:initialize)
(provide 'joust)

