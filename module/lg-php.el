;;; lg-php.el
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

;; php-mode
(use-package 
  php-mode 
  :ensure t 
  :mode (("\\.php$" . php-mode) 
         ("\\.phpt$" . php-mode)) 
  :config (require 'php-ext) 
  (define-key php-mode-map (kbd "C-c C--") 'php-current-class) 
  (define-key php-mode-map (kbd "C-c C-=") 'php-current-namespace))

;; company-php
(use-package 
  company-php 
  :ensure t 
  :after(ac-php php-mode) 
  :config (company-mode t) 
  (ac-php-core-eldoc-setup) ;; enable eldoc
  (add-to-list 'company-backends 'company-ac-php-backend))

;; ac-php
(use-package 
  ac-php 
  :ensure t 
  :after php-mode
  ;; :config
  ;; (auto-complete-mode t)
  ;; (define-key php-mode-map  (kbd "C-t") 'ac-php-location-stack-back)    ;go back
  ;; indent
  ;; (setq indent-tabs-mode nil)
  ;; (setq c-basic-offset 4)
  ;; (setq php-template-compatibility nil)
  ;; (setq ac-sources  '(ac-source-php))
  ;; (yas-global-mode 1)
  ;; (ac-php-core-eldoc-setup ) ;; enable eldoc
  ;; (define-key php-mode-map  (kbd "C-]") 'ac-php-find-symbol-at-point)   ;goto define
  ;; (subword-mode 1))
  )

(provide 'lg-php)
