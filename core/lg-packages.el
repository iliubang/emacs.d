;;; init.el --- Liubang's configuration entry point.
;; 
;; Copyright (c) 2018 Liubang
;; 
;; Author: liubang <it.liubang@gmail.com>
;; Url: https://iliubang.cn
;; Version: 1.0

;; a secure emacs environment
(require 'cl)
(setq tls-checktruct t)

(setq python (or (executable-find "py.ext")
                 (executable-find "python")))

(let ((trustfile
        (replace-regexp-in-string
          "\\\\" "/"
          (replace-regexp-in-string
            "\n" ""
            (shell-command-to-string (concat python " -m certifi"))))))
  (setq tls-program
        (list
          (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                  (if (eq window-system 'w32) ".exe" "") trustfile)))
  (setq gnutls-verify-error t)
  (setq gnutls-trustfiles (list trustfile)))

;; Test the settings by using the following code snippet:
;;  (let ((bad-hosts
;;         (loop for bad
;;               in `("https://wrong.host.badssl.com/"
;;                    "https://self-signed.badssl.com/")
;;               if (condition-case e
;;                      (url-retrieve
;;                       bad (lambda (retrieved) t))
;;                    (error nil))
;;               collect bad)))
;;    (if bad-hosts
;;        (error (format "tls misconfigured; retrieved %s ok" bad-hosts))
;;      (url-retrieve "https://badssl.com"
;;                    (lambda (retrieved) t))))

(require 'package)

(defvar gnu '("gnu" . "https://elpa.gnu.org/packages/"))
(defvar melpa '("melpa" . "https://melpa.org/packages/"))
(defvar melpa-stable '("melpa-stable" . "https://stable.melpa.org/packages/"))
(defvar org-elpa '("org" . "http://orgmode.org/elpa/"))

;; add marmalade to package repos
(setq package-archives nil)
(add-to-list 'package-archives melpa-stable t)
(add-to-list 'package-archives melpa t)
(add-to-list 'package-archives gnu t)
(add-to-list 'package-archives org-elpa t)

;; ini packages
(package-initialize)

(unless (and (file-exists-p (concat lg-dir "elpa/archives/gnu"))
             (file-exists-p (concat lg-dir "elpa/archives/melpa"))
             (file-exists-p (concat lg-dir "elpa/archives/melpa-stable")))
  (package-refresh-contents))

(provide 'lg-packages)
