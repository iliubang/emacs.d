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

(require 'package)
(require 'cl)
(setq tls-checktruct t)

(setq python (or (executable-find "py.ext") 
                 (executable-find "python")))

(let ((trustfile (replace-regexp-in-string "\\\\" "/" (replace-regexp-in-string "\n" ""
                                                                                (shell-command-to-string
                                                                                 (concat python
                                                                                         " -m certifi")))))) 
  (setq tls-program (list (format "gnutls-cli%s --x509cafile %s -p %%p %%h" (if (eq window-system
                                                                                    'w32) ".exe" "")
                                  trustfile))) 
  (setq gnutls-verify-error t) 
  (setq gnutls-trustfiles (list trustfile)))

;; add marmalade to package repos
(setq package-archives '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/") 
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/") 
                         ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/") 
                         ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
                         ("marmalade" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/marmalade/")))
;; ini packages
(package-initialize)

(unless (and (file-exists-p (concat lg-dir "elpa/archives/org-elpa")) 
             (file-exists-p (concat lg-dir "elpa/archives/melpa")) 
             (file-exists-p (concat lg-dir "elpa/archives/melpa-stable"))) 
  (package-refresh-contents))

;; Patch up annoying package.el quirks
(defadvice package-generate-autoloads (after close-autoloads (name pkg-dir) activate) 
  (let* ((path (expand-file-name (concat (if (symbolp name) 
                                             (symbol-name name) name) "-autoloads.el") pkg-dir))) 
    (with-current-buffer (find-file-existing path) 
      (kill-buffer nil))))

(defun require-package (package &optional min-version no-refresh) 
  (if (package-installed-p package min-version) t (if (or (assoc package package-archive-contents) 
                                                          no-refresh) 
                                                      (package-install package) 
                                                    (progn (package-refresh-contents) 
                                                           (require-package package min-version
                                                                            t)))))

;;;;;;;;;;;;; pcakges
(require-package 'use-package)
(setq use-package-always-ensure t)
(require-package 'diminish)

(provide 'lg-packages)
