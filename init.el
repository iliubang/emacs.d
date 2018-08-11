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

;; user and email
(setq user-full-name "liubang")
(setq user-mail-address "it.liubang@gmail.com")

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
(defvar lg-core-dir (expand-file-name "core" lg-dir))
(defvar lg-module-dir (expand-file-name "module" lg-dir))
(defvar lg-gtd-dir (expand-file-name "gtd" lg-dir))
(defvar lg-custom-file (expand-file-name "custom.el" lg-dir))

;; define require-module macro
(defmacro require-module (pkg)
  `(load (file-truename (format (concat lg-module-dir "/%s"), pkg))))

;; define require core macro
(defmacro require-init (pkg)
  `(load (file-truename (format (concat lg-core-dir "/%s"), pkg))))

;; add linger's directories to emacs's load-path
(add-to-list 'load-path lg-core-dir)
(add-to-list 'load-path lg-module-dir)

;; speed up emacs start
(let ((file-name-handler-alist nil))
  (message "Loading core...")
  ;; load core...
  (require-init 'lg-packages)
  (require-init 'lg-better)
  (require-init 'lg-ui)
  (require-init 'lg-org)
  (require-init 'lg-company)
  ;; loading modules...
  (message "Loading modules...")
  ;; load extra modules
  ;; (require-module 'lg-clang)
)

(if (file-exists-p lg-custom-file) (load-file lg-custom-file))
(setq custom-file (expand-file-name "custom-set-variables.el" lg-dir))
(load custom-file :no-error)

