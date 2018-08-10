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

;; init some directories.
(defvar lg-dir (file-name-directory load-file-name))
(defvar lg-core-dir (expand-file-name "core" lg-dir))
(defvar lg-module-dir (expand-file-name "module" lg-dir))
(defvar lg-gtd-dir (expand-file-name "gtd" lg-dir))

;; add linger's directories to emacs's load-path
(add-to-list 'load-path lg-core-dir)
(add-to-list 'load-path lg-module-dir)

(message "Loading core...")

;; load core modules
(require 'lg-packages)
(require 'lg-better)
(require 'lg-ui)
(require 'lg-org)
;; load extra modules
(require 'lg-go)
(require 'lg-clang)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("/Users/liubang/.emacs.d/gtd/inbox.org")))
 '(package-selected-packages (quote (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title ((t (:inherit default :weight bold :foreground "#bdc3ce" :font "Operator Mono Medium" :height 1.5 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#bdc3ce" :font "Operator Mono Medium" :height 1.75))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#bdc3ce" :font "Operator Mono Medium" :height 1.5))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#bdc3ce" :font "Operator Mono Medium" :height 1.25))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#bdc3ce" :font "Operator Mono Medium" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#bdc3ce" :font "Operator Mono Medium"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#bdc3ce" :font "Operator Mono Medium"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#bdc3ce" :font "Operator Mono Medium"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#bdc3ce" :font "Operator Mono Medium")))))
(put 'upcase-region 'disabled nil)
