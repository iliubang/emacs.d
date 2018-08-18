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

(add-hook 'after-init-hook 'global-company-mode)

(with-eval-after-load 'company
                      ;; https://github.com/company-mode/company-mode/issues/348
                      (company-statistics-mode)
                      (add-to-list 'company-backends 'company-cmake)
                      (add-to-list 'company-backends 'company-c-headers)
                      (setq company-backends (delete 'company-ropemacs company-backends))
                      (setq company-dabbrev-downcase nil
                            ;; make previous/next selection in the popup cycles
                            company-selection-wrap-around t
                            company-dabbrev-ignore-case nil
                            ;; press M-number to choose candidate
                            company-show-numbers t
                            company-idle-delay 0.1
                            company-minimul-prefix-length 3
                            company-clang-insert-arguments nil
                            company-require-match nil
                            company-etags-ignore-case t)
                      (defadvice company-in-string-or-comment (around company-in-string-or-comment-hack activate)
                                 (if (memq major-mode '(php-mode html-mode web-mode nxml-mode))
                                   (setq ad-return-value nil)
                                 ad-do-it))

                      ;; press SPACE will accept the highlighted candidate and insert a space
                      ;; `M-x describe-variable company-auto-complete-chars` for details
                      ;; That's BAD idea.
                      (setq company-auto-complete nil)
                      ;; NOT to load company-mode for certain major modes.
                      ;; https://github.com/company-mode/company-mode/issues/29
                      ;; (setq company-global-modes
                      ;;        'not (eshell-mode comint-mode erc-mode gud-mode rcirc-mode minibuffer-inactive-mode))
                      (define-key company-active-map (kbd "M-n") nil)
                      (define-key company-active-map (kbd "M-p") nil)
                      (define-key company-active-map (kbd "C-n") #'company-select-next)
                      (define-key company-active-map (kbd "C-p") #'company-select-previous))

;; company-etags
(with-eval-after-load 'company-etags
                      ;; insert major-mode not inherited from prog-mode
                      ;; to make company-etags work.
                      (add-to-list 'company-etags-modes 'web-mode)
                      (add-to-list 'company-etags-modes 'lua-mode))

(provide 'lg-company)
