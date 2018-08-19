;;; lg-company.el
;;
;; Copyright (c) 2018 Liubang
;;
;; Author: liubang <it.liubang@gmail.com> Url: https://iliubang.cn
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

(use-package 
  company 
  :ensure t 
  :init (global-company-mode) 
  :config (progn 
            (setq company-backends (delete 'company-ropemacs company-backends)) 
            (setq company-dabbrev-downcase nil
                  ;; make previous/next selection in the popup cycles
                  company-selection-wrap-around t company-dabbrev-ignore-case nil
                  ;; press M-number to choose candidate
                  company-show-numbers t company-idle-delay 0.1 company-minimul-prefix-length 3
                  company-clang-insert-arguments nil company-require-match nil
                  company-etags-ignore-case t) 
            (define-key company-active-map (kbd "M-n") nil) 
            (define-key company-active-map (kbd "M-p") nil) 
            (define-key company-active-map (kbd "C-n") #'company-select-next) 
            (define-key company-active-map (kbd "C-p") #'company-select-previous)))

;; company-statistics
(use-package 
  company-statistics 
  :ensure t 
  :after company 
  :config (add-hook 'after-init-hook 'company-statistics-mode))

(provide 'lg-company)
