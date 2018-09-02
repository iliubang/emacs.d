;;; lg-lua.el
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

(use-package 
  lua-mode 
  :interpreter "lua" 
  :config (defun ptrv/lua-send-region-or-current-line () 
            (interactive) 
            (if (region-active-p) 
                (lua-send-region (region-beginning) 
                                 (region-end)) 
              (lua-send-current-line))) 
  (bind-keys :map lua-mode-map ("C-c C-d" . lua-send-proc) 
             ("C-c C-c" . ptrv/lua-send-region-or-current-line) 
             ("C-c C-p" . lua-start-process))
  (use-package 
    company-lua 
    :after company 
    :init (defun ptrv/lua-mode-company-init () 
            (setq-local company-backends '((company-lua
                                            company-etags
                                            company-dabbrev-code
                                            company-yasnippet)))) 
    (add-hook 'lua-mode-hook #'ptrv/lua-mode-company-init)))

(provide 'lg-lua)
