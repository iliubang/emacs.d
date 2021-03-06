;;; init.el --- Liubang's configuration entry point.
;; 
;; Copyright (c) 2018 Liubang
;; 
;; Author: liubang <it.liubang@gmail.com>
;; Url: https://iliubang.cn
;; Version: 1.0
;;
;;; License
;;
;; MIT License
;;
;; Copyright (c) 2018 liubang
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; user and email
(setq user-full-name "liubang")
(setq user-mail-address "it.liubang@gmail.com")

;; the frequency of GC
(setq gc-cons-threshold 50000000)

(defvar current-user
  (getenv 
    (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "Liubang's configuration is powering up... Be patient, Master %s!" current-user)

(when (version< emacs-version "25.1")
  (error "Prelude requires GNU Emacs 25.1 or newer, but you're running %s" emacs-version))

;; *Message* buffer should be writable in 24.4+
(defadvice switch-to-buffer (after switch-to-buffer-after-hack activate) 
  (if (string= "*Messages*" (buffer-name))
    (read-only-mode -1)))

;; define some directories variables.
(defvar lg-dir (file-name-directory load-file-name))
(defvar lg-core-dir (concat lg-dir "/core"))
(defvar lg-module-dir (concat lg-dir "/module"))
(defvar lg-cache-dir (concat lg-dir "/cache"))
(defvar lg-local-dir (concat lg-dir "/local"))
(defvar lg-custom-file (concat "/custom.el"))
(defvar lg-theme-dir (concat lg-local-dir "/themes"))
;; gtd directory
(defvar lg-gtd-dir "~/Documents/cloud/org")

;; define require-module macro
(defmacro require-module (pkg)
  `(load (file-truename (format (concat lg-module-dir "/%s"), pkg))))

;; define require core macro
(defmacro require-init (pkg)
  `(load (file-truename (format (concat lg-core-dir "/%s"), pkg))))

;; add linger's directories to emacs's load-path
(add-to-list 'load-path lg-core-dir)
(add-to-list 'load-path lg-module-dir)
(add-to-list 'load-path lg-theme-dir)

;; speed up emacs start
(let ((file-name-handler-alist nil))
  (message "Loading core...")
  ;; load core...
  (require-init 'lg-const)
  (require-init 'lg-packages)
  (require-init 'lg-utils)
  (require-init 'lg-ui)
  (require-init 'lg-editor)
  (require-init 'lg-org)
  (require-init 'lg-artist)
  (require-init 'lg-company)
  (require-init 'lg-yasnippet)
  (require-init 'lg-git)
  ;; loading modules...
  (message "Loading modules...")
  ;; load extra modules
  (require-module 'lg-clang)
  (require-module 'lg-assembly)
  (require-module 'lg-php)
  (require-module 'lg-lua)
)

(if (file-exists-p lg-custom-file) (load-file lg-custom-file))
(setq custom-file (concat lg-cache-dir "/custom-set-variables.el"))
(load custom-file :no-error)

