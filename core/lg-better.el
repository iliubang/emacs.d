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

;; set keys for Apple keyboard, for emacs in OS X
;; (setq mac-command-modifier 'meta) ; make cmd key do Meta
;; (setq mac-option-modifier 'super) ; make opt key do Super
(setq mac-control-modifier 'control)    ; make Control key do Control
(setq ns-function-modifier 'hyper)      ; make Fn key do Hyper

;; env
(if (or (eq system-type 'darwin) 
        (eq system-type 'berkeley-unix)) 
    (setq system-name (car (split-string system-name "\\."))))

(use-package 
  exec-path-from-shell 
  :ensure t 
  :config (setq exec-path-from-shell-variables '("PATH" "GOPATH" "JAVA_HOME")) 
  (when (memq window-system '(mac ns x)) 
    (exec-path-from-shell-initialize)))

;; (setenv "PATH" (concat "/usr/local/bin:"
;;                        "~/.pyenv/shims/virtualenv"
;;                        "~/.pyenv/bin"
;;                        "~/.pyenv/shims"
;;                        "~/.phpbrew/php/php-7.2.8/bin"
;;                        "/usr/bin"
;;                        (getenv "PATH")))

;; enable erase-buffer command
;; http://emacsredux.com/blog/2013/05/04/erase-buffer/
(put 'erase-buffer 'disabled nil)
(global-set-key (kbd "C-c e")  'erase-buffer)

;; Always load newest byte code.
(setq load-prefer-newer t)

;; warn when opening files bigger then 100MB
(setq large-file-warning-threshold 100000000)

;; record recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 10)

;; display time on status line
(display-time-mode 1)

;; don't backup file
(setq make-backup-files nil)

(defalias 'yes-or-no-p 'y-or-n-p)

;; default indent
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

;; http://pragmaticemacs.com/emacs/dont-kill-buffer-kill-this-buffer-instead/
(global-set-key (kbd "C-x k") 'kill-this-buffer)
;; kill buffer without my confirmation
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function
                                        kill-buffer-query-functions))

;; ibuffer mode
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; split
(global-set-key (kbd "C-c -") 'split-window-below)
(global-set-key (kbd "C-c |") 'split-window-right)

;; ivy
(use-package 
  ivy 
  :commands (ivy-recentf ivy-read) 
  :ensure t 
  :diminish (ivy-mode) 
  :bind (("C-x b" . ivy-switch-buffer)) 
  :config (ivy-mode 1) 
  (setq ivy-use-virtual-buffers t)
  ;; https://github.com/abo-abo/swiper/issues/828
  (setq ivy-display-style 'fancy)
  ;; https://github.com/abo-abo/swiper/issues/1218
  (setq ivy-dynamic-exhibit-delay-ms 250)
  ;; https://oremacs.com/2017/11/30/ivy-0.10.0/
  (setq ivy-use-selectable-prompt t))

;; swiper
(use-package 
  swiper 
  :ensure t 
  :bind (("C-s" . swiper) 
         ("C-c C-r" . ivy-resume) 
         ("M-x" . counsel-M-x) 
         ("C-x C-f" . counsel-find-file) 
         ("C-h f" . counsel-describe-function) 
         ("C-h v" . counsel-describe-variable) 
         ("C-h l" . counsel-find-library) 
         ("C-h i" . counsel-info-lookup-symbol) 
         ("C-h u" . counsel-unicode-char) 
         ("C-c g" . counsel-git) 
         ("C-c j" . counsel-git-grep) 
         ("C-c k" . counsel-ag) 
         ("C-c l" . counsel-locate)) 
  :config (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

;; avy
(use-package 
  avy 
  :ensure t 
  :bind(("M-g w" . avy-goto-word-1) 
        ("M-g f" . avy-goto-line)))

;; autopair
(use-package 
  autopair 
  :ensure t 
  :init (add-hook 'emacs-lisp-mode-hook #'(lambda () 
                                            (push '(?` . ?') 
                                                  (getf autopair-extra-pairs 
                                                        :comment)) 
                                            (push '(?` . ?') 
                                                  (getf autopair-extra-pairs 
                                                        :string)))) 
  :config (autopair-global-mode))

;; undo
(use-package 
  undo-tree 
  :ensure t 
  :init (global-undo-tree-mode 1)
  ;; make ctrl-z undo
  :bind(("C-z" . undo)) 
  :config
  ;; make ctrl-Z redo
  (defalias 'redo 'undo-tree-redo))

(provide 'lg-better)
