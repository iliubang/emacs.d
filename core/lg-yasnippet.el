;;; lg-yasnippet.el
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

;; yasnippet
(use-package yasnippet
             :ensure t
             :mode ("\\.yasnippet\\'" . snippet-mode)
             :config
             ;; my private snippets, should be placed before enabling yasnippet
             (setq custom-yasnippets (expand-file-name "snippets" lg-dir))
             (if (and (file-exists-p custom-yasnippets) (not (member custom-yasnippets yas-snippet-dirs)))
               (add-to-list 'yas-snippet-dirs custom-yasnippets))
             (setq-default mode-require-final-newline nil)
             ;; give yas-dropdown-prompt in yas/prompt-functions a chance
             (setq yas-prompt-functions '(yas-dropdown-prompt
                                          yas-ido-prompt
                                          yas-completing-prompt))
             (defadvice yas-insert-snippet (around use-completing-prompt activate)
               (let* ((yas-prompt-functions '(yas-completing-prompt)))
                 ad-do-it))
             (yas-global-mode 1))

;; yasnippet-snippets
(use-package yasnippet-snippets
             :after yasnippet
             :ensure t)

;; auto yasnippet
;; https://github.com/abo-abo/auto-yasnippet
(use-package auto-yasnippet
             :ensure t
             :commands(aya-create aya-expand)
             :bind(("H-w" . aya-create)
                   ("H-y" . aya-expand)))

(provide 'lg-yasnippet)
