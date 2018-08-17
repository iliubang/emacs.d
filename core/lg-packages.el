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

;; add marmalade to package repos
(setq package-archives '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
						 ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
						 ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))
;; ini packages
(package-initialize)

(unless (and (file-exists-p (concat lg-dir "elpa/archives/org-elpa"))
             (file-exists-p (concat lg-dir "elpa/archives/melpa"))
             (file-exists-p (concat lg-dir "elpa/archives/melpa-stable")))
  (package-refresh-contents))

;; Patch up annoying package.el quirks
(defadvice package-generate-autoloads (after close-autoloads (name pkg-dir) activate)
           (let* ((path (expand-file-name (concat
                                   (if (symbolp name) (symbol-name name) name)
                                   "-autoloads.el") pkg-dir)))
             (with-current-buffer (find-file-existing path)
                         (kill-buffer nil))))

(defun require-package (package &optional min-version no-refresh)
  (if (package-installed-p package min-version) 
    t
    (if (or (assoc package package-archive-contents) no-refresh)
      (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

;;;;;;;;;;;;; pcakges
(require-package 'use-package)
(require-package 'diminish)
(require-package 'avy)
;; auto complete
(require-package 'auto-yasnippet)
(require-package 'counsel-gtags)
(require-package 'yasnippet)
(require-package 'yasnippet-snippets)
(require-package 'company)
(require-package 'company-c-headers)
(require-package 'company-statistics)
;; org-mode
(require-package 'org)
(require-package 'org-bullets)
(require-package 'htmlize)
(require-package 'ox-reveal)
(require-package 'pinyinlib)
(require-package 'find-by-pinyin-dired)
(require-package 'command-log-mode)
;; autopair
;; clang
(require-package 'clang-format)
(require-package 'cmake-mode)
(require-package 'cpputils-cmake)
;; php
(require-package 'php-mode)
(require-package 'company-php)
(require-package 'ac-php)
;; ui & theme
(require-package 'dashboard)
(require-package 'zenburn-theme)
(require-package 'spaceline)
;; (require-package 'solarized-theme)

;;;;;;;;;;;;; autoload
;; (autoload 'ivy-recentf "ivy" "" t)
;; (autoload 'ivy-read "ivy")
(autoload 'counsel-etags-find-tag-at-point "counsel-etags" "" t nil)
(autoload 'counsel-etags-scan-code "counsel-etags" "" t nil)
(autoload 'counsel-etags-grep "counsel-etags" "" t nil)
(autoload 'counsel-etags-grep-symbol-at-point "counsel-etags" "" t nil)
(autoload 'counsel-etags-recent-tag "counsel-etags" "" t nil)
(autoload 'web-mode "web-mode")
(autoload 'snippet-mode "yasnippet" "")
(autoload 'clang-format-region "clang-format")
(autoload 'clang-format-buffer "clang-format")
(autoload 'php-mode "php-mode" "Major mode for editing PHP code." t)

(provide 'lg-packages)
