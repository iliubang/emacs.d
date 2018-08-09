;;; lg-packages.el
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

;; a secure emacs environment
;; https://glyph.twistedmatrix.com/2015/11/editor-malware.html
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

(defvar gnu '("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
(defvar melpa '("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))
(defvar melpa-stable '("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/"))
(defvar org-elpa '("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/"))

;; add marmalade to package repos
(setq package-archives nil)
(add-to-list 'package-archives gnu)
(add-to-list 'package-archives melpa-stable t)
(add-to-list 'package-archives melpa t)
(add-to-list 'package-archives org-elpa t)

;; ini packages
(package-initialize)

(unless (and (file-exists-p (concat lg-dir "elpa/archives/org-elpa"))
             (file-exists-p (concat lg-dir "elpa/archives/melpa"))
             (file-exists-p (concat lg-dir "elpa/archives/melpa-stable")))
  (package-refresh-contents))

;; define packages-install function
(defun packages-install (&rest packages)
  (message "running packages-install")
  (mapc (lambda (package)
          (let ((name (car package))
                (repo (cdr package)))
            (when (not (package-installed-p name))
              (let ((package-archives (list repo)))
                (package-initialize)
                (package-install name)))))
        packages)
  (package-initialize)
  (delete-other-windows))

;; install missing packages
(defun init--install-packages ()
  (message "Lets install some packages")
  (packages-install
   ;; Since use-package this is the only entry here
   ;; ALWAYS try to use use-package!
   (cons 'use-package melpa)))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; install use-package plugin
(package-install 'use-package)

(provide 'lg-packages)
