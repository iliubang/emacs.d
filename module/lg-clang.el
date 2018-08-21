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

;; company-c-headers
(use-package 
  company-c-headers 
  :ensure t 
  :after company 
  :config (add-to-list 'company-backends 'company-c-headers))

;; cmake-font-lock
(use-package cmake-font-lock
  :ensure t)

;; cmake-mode
(use-package 
  cmake-mode
  :ensure t
  :after cmake-font-lock
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
  :ensure t 
  :commands(clang-format-buffer clang-format-region) 
  :bind(("C-c i" . clang-format-region) 
        ("C-c u" . clang-format-buffer)))

;; disaster
(use-package disaster
  :commands disaster)

;; https://github.com/ludwigpacifici/modern-cpp-font-lock
(use-package 
  modern-cpp-font-lock 
  :ensure t
  :defer t 
  :diminish modern-c++-font-lock-mode 
  :hook ((c-mode c++-mode) . modern-c++-font-lock-mode))

(provide 'lg-clang)
