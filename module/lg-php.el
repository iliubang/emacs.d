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
  :mode "\\.php[ts345]?$"  
  :interpreter "php"
  :hook (php-mode . ac-php-core-eldoc-setup)
  :config
  (require 'php-ext) 
  (define-key php-mode-map (kbd "C-c C--") 'php-current-class) 
  (define-key php-mode-map (kbd "C-c C-=") 'php-current-namespace)
  (sp-with-modes '(php-mode)
    (sp-local-pair "/* "    "*/" :post-handlers '(("||\n[i] " "RET") ("| " "SPC")))
    (sp-local-pair "<? "    " ?>")
    (sp-local-pair "<?php " " ?>")
    (sp-local-pair "<?="    " ?>")))

;; php-extras
(use-package php-extras
  :after php-mode)

;; php-refactor-mode
(use-package php-refactor-mode
  :hook php-mode)

;; company-php
(use-package 
  company-php 
  :after(ac-php php-mode)
  :hook php-mode
  :config (company-mode t)
  (ac-php-core-eldoc-setup) ;; enable eldoc
  (add-to-list 'company-backends 'company-ac-php-backend))

;; ac-php
(use-package 
  ac-php
  :commands (company-ac-php-backend ac-php-remake-tags ac-php-remake-tags-all ac-php-core-eldoc-setup)
  :after php-mode
  :config
  (unless (executable-find "phpctags")
    (warn "php-mode: phpctags isn't installed, auto-completion will be gimped"))
  (setq ac-php-tags-path (concat lg-cache-dir "/ac-php/")))

(provide 'lg-php)