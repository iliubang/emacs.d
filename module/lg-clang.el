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
             :mode(("\\.cpp$" . c++-mode)
                   ("\\.c$" . c-mode)
                   ("\\.hpp$" . c++-mode)
                   ("\\.h$" . c-mode))
             :preface
             (defun fix-c-indent-offset-according-to-syntax-context (key val)
               (setq c-offsets-alist (delq (assoc key c-offsets-alist) c-offsets-alist))
               (add-to-list 'c-offsets-alist '(key . val)))
             (defun cc/init-company ()
               (set (make-local-variable 'company-backends) 
                    '(company-capf
                      company-clang
                      company-gtags
                      company-etags
                      company-yasnippet))
               (setq c-basic-offset 4)
               (setq c-auto-newline nil)
               (setq lazy-lock-defer-contextually t)
               (setq lazy-lock-defer-time 0) 
               (c-toggle-hungry-state 1) 
               (fix-c-indent-offset-according-to-syntax-context 'substatement 0)
               (fix-c-indent-offset-according-to-syntax-context 'func-decl-cont 0)
               ;; include path
               (setq cc-search-directories '("." 
                                             "/usr/include" 
                                             "/usr/local/include/*" 
                                             "../*/include" 
                                             "$WXWIN/include")) 
               ;; make a #define be left-aligned
               (setq c-electric-pound-behavior (quote (alignleft)))
               (when buffer-file-name
                 (flymake-mode 1)
                 (if (executable-find "cmake")
                   (if (not (or (string-match "^/usr/local/include/.*" buffer-file-name)
                                (string-match "^/usr/src/linux/include/.*" buffer-file-name)))
                     (cppcm-reload-all))))
               ;; c-style
               ;; (setq c-default-style '((java-mode . "java")
               ;;                         (awk-mode  . "awk")
               ;;                         (other     . "linux")))
               (company-mode))
             :bind
             (:map c-mode-base-map
                   ("C-c h c" . hs-toggle-hiding)
                   ("C-c h b" . hs-hide-block)
                   ("C-c h s" . hs-show-block)
                   ("C-c h a" . hs-hide-all)
                   ("C-c h d" . hs-show-all)
                   ("C-c h l" . hs-hide-level))
             :hook (((c-mode c++-mode) . cc/init-company)
                    ((c-mode c++-mode) . which-function-mode)
                    ((c-mode c++-mode) . turn-on-eldoc-mode)
                    ((c-mode c++-mode) . hs-minor-mode)))

;; company-c-headers
(use-package company-c-headers
             :ensure t
             :after company
             :init (push 'company-c-headers company-backends))

;; company-cmake
(use-package cmake-mode
             :ensure t
             :mode (("CMakeLists\\.txt\\'" . cmake-mode)
                    ("\\.cmake\\'" . cmake-mode))
             :preface
             (defun cmake/init-company ()
                (setq-local company-backends '(company-dabbrev-code company-keywords company-cmake))
                (company-mode))
             :hook ((cmake-mode . cmake/init-company)
                    (cmake-mode . cmake-font-lock-activate)))

;; https://github.com/abo-abo/elf-mode
(use-package elf-mode
             :defer t)

;; https://github.com/google/styleguide/blob/gh-pages/google-c-style.el
(use-package google-c-style
             :defer t
             :hook (((c++-mode c-mode) . google-set-c-style)
                    ((c++-mode c-mode) . google-make-newline-indent)))

;; clang-format
(use-package clang-format
             :ensure t
             :commands(clang-format-buffer clang-format-region)
             :bind(("C-c i" . clang-format-region)
                   ("C-c u" . clang-format-buffer)))

;; https://github.com/ludwigpacifici/modern-cpp-font-lock
(use-package modern-cpp-font-lock
             :defer t
             :diminish modern-c++-font-lock-mode
             :hook ((c-mode c++-mode) . modern-c++-font-lock-mode))

;; ggtags
(use-package ggtags
             :defer t
             :bind
             (:map ggtags-mode-map
                   ("C-c g s" . ggtags-find-other-symbol)
                   ("C-c g h" . ggtags-view-tag-history)
                   ("C-c g r" . ggtags-find-reference)
                   ("C-c g f" . ggtags-find-file)
                   ("C-c g c" . ggtags-create-tags)
                   ("C-c g u" . ggtags-update-tags)
                   ("C-c g e" . ggtags-save-project-settings)
                   ("C-c <"   . ggtags-prev-mark)
                   ("C-c >"   . ggtags-next-mark)
                   ("C-c M-j" . ggtags-visit-project-root)
                   ("M-,"     . pop-tag-mark))
             :hook ((c-mode c++-mode) . ggtags-mode)
             :init
             (setq large-file-warning-threshold nil)
             :config
             (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
             (setq-local eldoc-documentation-function #'ggtags-eldoc-function)
             (setq-local hippie-expand-try-functions-list
                         (cons 'ggtags-try-complete-tag hippie-expand-try-functions-list)))

;; gtags
(use-package counsel-gtags
             :ensure t
             :after counsel
             :hook ((c-mode c++-mode) . counsel-gtags-mode)
             :commands(counsel-gtags-find-definition 
                       counsel-gtags-find-reference
                       counsel-gtags-find-symbol 
                       counsel-gtags-go-backward)
             :bind(("H-t" . counsel-gtags-find-definition)
                   ("H-r" . counsel-gtags-find-reference)
                   ("H-s" . counsel-gtags-find-symbol)
                   ("H-," . counsel-gtags-go-backward)))



(provide 'lg-clang)
