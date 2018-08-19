;;; lg-go.el
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
  go-mode-autoloads 
  :ensure go-mode 
  :defer t 
  :mode ("\\.go\\'" . go-mode) 
  :preface (defun go/init-company () 
             (set (make-local-variable 'company-backends) 
                  '(company-capf company-yasnippet)) 
             (company-mode) 
             (use-package 
               company-go 
               :after company 
               :init (push 'company-go company-backends))) 
  :bind (:map go-mode-map
              ;; ("C-c C-a" . go-import-add)
              ("C-c C-d" . godef-describe) 
              ("C-c C-j" . godef-jump) 
              ("C-x 4 C-c C-j" . godef-jump-other-window) 
              ("C-c C-f a" . go-goto-arguments) 
              ("C-c C-f d" . go-goto-docstring) 
              ("C-c C-f f" . go-goto-function) 
              ("C-c C-f i" . go-goto-imports) 
              ("C-c C-f m" . go-goto-method-receiver) 
              ("C-c C-f n" . go-goto-function-name) 
              ("C-c C-f r" . go-goto-return-values)) 
  :commands (gofmt-before-save) 
  :hook ((go-mode . go/init-company) 
         (go-mode . flycheck-mode) 
         (go-mode . (lambda () 
                      (add-hook 'before-save-hook 'gofmt-before-save)))))

(use-package 
  lsp-go 

  :disabled 
  :defer t 
  :hook (go-mode . lsp-go-enable))

(use-package 
  go-eldoc 
  :after go-mode 
  :defer t 
  :diminish eldoc-mode 
  :hook (go-mode . go-eldoc-setup))

(use-package 
  go-guru 
  :after go-mode 
  :defer t 
  :hook (go-mode . go-guru-hl-identifier-mode))

(use-package 
  go-rename 
  :after go-mode 
  :defer t)

(use-package 
  go-playground 
  :after go-mode 
  :defer t)

(use-package 
  go-dlv 
  :after go-mode 
  :defer t)

(use-package 
  gorepl-mode 
  :defer t 
  :hook (go-mode . gorepl-mode))

(provide 'lg-go)
