;;; asdf-bundler --- A module for copying all ASDF system dependencies to a directory.

;; Copyright (C) 2023 Mariano Montone. All rights reserved.

;; This work is licensed under the terms of the MIT license.
;; For a copy, see <https://opensource.org/licenses/MIT>.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1
;; Requires: ASDF, ALEXANDRIA

;;; Commentary:

;; This module copies all the dependencies of an ASDF system to a directory.
;; Then the ASDF and its dependencies can be loaded pointing ASDF to that directory.

;; Usage:

;; Use `copy-dependencies` to copy dependencies to a directory. Then setup ASDF to load dependencies from that directory:
;; ```lisp
;; (asdf:initialize-source-registry
;;  '(:source-registry
;;    :ignore-inherited-configuration
;;    (:tree <dependencies-directory>)))
;; ```
;; `(asdf:operate 'asdf:load-op <my-asdf-system>)`

;;; Code:

(require :asdf)
(require :uiop)
(require :alexandria)

(defpackage :asdf-bundler
  (:use :cl)
  (:export #:copy-dependencies))

(in-package :asdf-bundler)

(defun system-depends-on (asdf-system)
  ;; FIXME:
  ;; This removes (:feature (:require system)) elements from the list of dependencies
  ;; Do it in a better way?
  (remove-if-not #'stringp
                 (asdf/system:system-depends-on asdf-system)))

(defun all-dependencies (asdf-system)
  "Calculate all the dependencies for ASDF-SYSTEM and return them in a list."
  (let ((processed '())
        (to-process (list (asdf:find-system asdf-system)))
        (dependencies '()))
    (loop for asdf-system = (pop to-process)
          while asdf-system
          do
             (let ((deps (mapcar #'asdf:find-system (system-depends-on asdf-system))))
               (dolist (dep deps)
                 (when (not (member dep processed))
                   (push dep to-process)))
               (push asdf-system processed)
               (alexandria:appendf dependencies deps)))
    (remove-duplicates dependencies)))

(defun systems-source-directories (asdf-systems)
  "Get the top-level source directories for ASDF systems in ASDF-SYSTEMS."
  (let ((all-directories (mapcar #'asdf/system:system-source-directory asdf-systems))
        (top-level-directories '()))
    (dolist (dir all-directories)
      (when (not (some (lambda (d)
                         (and (not (eq d dir))
                              (uiop/pathname:subpathp dir d)))
                       all-directories))
        (when dir
          (pushnew dir top-level-directories))))
    top-level-directories))

(defun copy-dependencies (asdf-system target-directory)
  "Copy dependencies of ASDF-SYSTEM to TARGET-DIRECTORY."
  (ensure-directories-exist target-directory)
  (let ((source-directories (systems-source-directories (all-dependencies asdf-system))))
    (dolist (source-directory source-directories)
      (format t "Copying ~a to ~a~%" source-directory target-directory)
      (uiop/run-program:run-program (format nil "cp -r ~a ~a" source-directory target-directory)))))

(provide :asdf-bundler)
