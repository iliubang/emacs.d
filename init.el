;;; init.el --- Liubang's configuration entry point.
;; 
;; Copyright (c) 2018 Liubang
;; 
;; Author: liubang <it.liubang@gmail.com>
;; Url: https://iliubang.cn
;; Version: 1.0

;; user and email
(setq user-full-name "liubang")
(setq user-mail-address "it.liubang@gmail.com")

(defvar current-user
  (getenv 
    (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "Liubang's configuration is powering up... Be patient, Master %s!" current-user)

(when (version< emacs-version "25.1")
  (error "Prelude requires GNU Emacs 25.1 or newer, but you're running %s" emacs-version))

;; Always load newest byte code.
(setq load-prefer-newer t)

;; init some directories.
(defvar lg-dir (file-name-directory load-file-name))
(defvar lg-core-dir (expand-file-name "core" lg-dir))
(defvar lg-module-dir (expand-file-name "module" lg-dir))

;; add linger's directories to emacs's load-path
(add-to-list 'load-path lg-core-dir)
(add-to-list 'load-path lg-module-dir)

;; the frequency of GC
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger then 100MB
(setq large-file-warning-threshold 100000000)

(message "Loading core...")

;; load core modules
(require 'lg-packages)
(require 'lg-ui)
(require 'lg-core)
(require 'lg-key)

