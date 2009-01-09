;;; joust.el --- A better package system for emacs

;; Copyright (C) 2009 Chromium 53 <everyone@53cr.com>

;; Author: Burke Libbey <burke@53cr.com>, Ryan Neufeld <ryan@53cr.com>
;; Created: 08 Jan 2009
;; Version: 0.1
;; Keywords: tools

;; This file is not (yet) part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:


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

