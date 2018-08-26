;;; lg-clang.el
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

;; cc-mode
(use-package cc-mode
  :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
         ("\\.m\\'" . c-mode)
         ("\\.c\\'" . c-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.c++\\'" . c++-mode)
         ("\\.mm\\'" . c++-mode))
  :commands (c-mode c++-mode objc-mode java-mode)
  :init
  (setq-default c-basic-offset tab-width)
  :preface
  (defun +cc-sp-point-is-template-p(id action context)
    (and (sp-in-code-p id action context)
         (sp-point-after-word-p id action context)))
  (defun +cc-sp-point-after-include-p(id action context)
    (and (sp-in-code-p id action context)
         (save-excursion
           (goto-char (line-beginning-position))
           (looking-at-p "[   ]*#include[^<]+"))))
  :config
  (c-toggle-electric-state -1)
  (c-toggle-auto-newline -1)
  (c-set-offset 'substatement-open '0) ; don't indent brackets
  (c-set-offset 'inline-open       '+)
  (c-set-offset 'block-open        '+)
  (c-set-offset 'brace-list-open   '+)
  (c-set-offset 'case-label        '+)
  (c-set-offset 'access-label      '-)
  (c-set-offset 'arglist-intro     '+)
  (c-set-offset 'arglist-close     '0)
  (setq c-tab-always-indent nil
        c-electric-flag nil
        ;; comment-style
        c-doc-comment-style '((java-mode . javadoc)
                              (pike-mode . autodoc)
                              (c-mode    . javadoc)
                              (c++-mode  . javadoc)))
  (dolist (key '("#" "{" "}" "/" "*" ";" "," ":" "(" ")"))
    (define-key c-mode-base-map key nil))
  
  (sp-with-modes '(c-mode c++-mode objc-mode java-mode)
    (sp-local-pair "<" ">" :when '(+cc-sp-point-is-template-p +cc-sp-point-after-include-p))
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "/*" "*/" :post-handlers '(("| " "SPC") ("||\n[i]" "RET")))))

;; company-c-headers
(use-package 
  company-c-headers 
  :after (company cc-mode) 
  :config (add-to-list 'company-backends 'company-c-headers)
  (setq company-c-headers-path-system '("/usr/include" "/usr/local/include"))
  (cond ((eq system-type 'darwin) (add-to-list 'company-c-headers-path-system "/usr/local/include/c++/8.2.0"))
        ((eq system-type 'gnu/linux) (add-to-list 'company-c-headers-path-system "/usr/include/c++/7"))))

;; cmake-font-lock
(use-package cmake-font-lock
  :after (cc-mode))

;; cmake-mode
(use-package 
  cmake-mode
  :after (cc-mode cmake-font-lock)
  :mode (("CMakeLists\\.txt\\'" . cmake-mode) 
         ("\\.cmake\\'" . cmake-mode)) 
  :preface (defun cmake/init-company () 
             (setq-local company-backends '(company-dabbrev-code company-keywords company-cmake)) 
             (company-mode)) 
  :hook ((cmake-mode . cmake/init-company) 
         (cmake-mode . cmake-font-lock-activate)))

;; clang-format
(use-package 
  clang-format 
  :commands(clang-format-buffer clang-format-region) 
  :bind(("C-c i" . clang-format-region) 
        ("C-c u" . clang-format-buffer)))

;; disaster
(use-package disaster
  :commands disaster)

;; elf
(use-package elf-mode
  :load-path (lambda() (concat lg-local-dir "/packages/elf-mode.git"))
  :commands (elf-mode))

;; https://github.com/ludwigpacifici/modern-cpp-font-lock
(use-package 
  modern-cpp-font-lock 
  :defer t 
  :diminish modern-c++-font-lock-mode 
  :hook ((c-mode c++-mode) . modern-c++-font-lock-mode))

(provide 'lg-clang)
