;;; lg-better.el
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

;; Always load newest byte code.
(setq load-prefer-newer t)

;; the frequency of GC
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger then 100MB
(setq large-file-warning-threshold 100000000)

;; record recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 10)

;; don't backup file
(setq make-backup-files nil)

;; set key on macos
(setq mac-option-modifier 'none)
(setq mac-command-modifier 'meta)
(setq ns-function-modifier 'hyper)

;; http://pragmaticemacs.com/emacs/dont-kill-buffer-kill-this-buffer-instead/
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; some packages to enhance emacs
;;
;; dimish modes
(use-package diminish
             :ensure t)

;; counsel
(use-package counsel
             :ensure t
             :bind
             (("M-x" . counsel-M-x)
              ("M-y" . counsel-yank-pop)
              :map ivy-minibuffer-map
              ("M-y" . ivy-next-line)))

;; swiper
(use-package swiper
             :pin melpa-stable
             :diminish ivy-mode
             :ensure t
             :bind*
             (("C-s" . swiper)
              ("C-c C-r" . ivy-resume)
              ("C-x C-f" . counsel-find-file)
              ("C-c h f" . counsel-describe-function)
              ("C-c h v" . counsel-describe-variable)
              ("C-c i u" . counsel-unicode-char)
              ("M-i" . counsel-imenu)
              ("C-x l" . counsel-locate)
              ("C-c k" . counsel-ag))
             :config
             (progn
               (ivy-mode 1)
               (setq ivy-use-virtual-buffers t)
               (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
               (ivy-set-actions
                 'counsel-find-file
                 '(("d" (lambda (x) (delete-file (expand-file-name x)))
                    "delete")))
               (ivy-set-actions
                 'ivy-switch-buffer
                 '(("k" (lambda (x) (kill-buffer x)
                          (ivy--reset-state ivy-last))
                    "kill")
                   ("j" ivy--switch-buffer-other-window-action
                    "other window")))))

(use-package ivy-hydra
             :ensure t)


;; bookmarks
(use-package bm
             :ensure t
             :bind 
             (("C-c =" . bm-toggle)
              ("C-c [" . bm-previous)
              ("C-c ]" . bm-next)))


(use-package command-log-mode
             :ensure t)

(defun live-coding ()
  (interactive)
  (set-face-attribute 'default nil :font "Hack-12")
  (add-hook 'prog-mode-hook 'command-log-mode))

(defun normal-coding ()
  (interactive)
  (set-face-attribute 'default nil :font "Hack-12"))

(use-package avy
             :ensure t
             :bind
             ("C-c j" . avy-goto-word-or-subword-1))

;; company mode
(use-package company
             :ensure t
             :bind (("C-c /" . company-complete))
             :config (global-company-mode))

;; outline
(use-package dash
             :ensure t)

(use-package outshine
             :ensure t
             :config
             (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
             (add-hook 'prog-mode-hook 'outline-minor-mode))

(provide 'lg-better)
