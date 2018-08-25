;;; lg-editor.el
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

(setq
 ;; bookmark
 bookmark-default-file (concat lg-local-dir "/bookmarks")
 ;; make control key do control
 mac-control-modifier 'control
 ;; make Fn key do Hyper
 ns-function-modifier 'hyper
 ;; makes C-n insert newlines if the point is at the end of the buffer
 next-line-add-newlines t)

;; env
(if (or (eq system-type 'darwin)
        (eq system-type 'berkeley-unix))
    (setq system-name (car (split-string system-name "\\."))))

(use-package
  exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (setq exec-path-from-shell-variables '("PATH" "GOPATH" "JAVA_HOME"))
  (exec-path-from-shell-initialize))

;; enable erase-buffer command
;; http://emacsredux.com/blog/2013/05/04/erase-buffer/
(put 'erase-buffer 'disabled nil)
(global-set-key (kbd "C-c e")  'erase-buffer)
(global-set-key (kbd "H-SPC")  'set-mark-command)

;; Always load newest byte code.
(setq load-prefer-newer t)

;; warn when opening files bigger then 100MB
(setq large-file-warning-threshold 100000000)

(setq-default
 auto-save-list-file-name (concat lg-cache-dir "/autosave"))

;; record recent files
(use-package
  recentf
  :config (setq recentf-save-file (concat lg-cache-dir "/recentf")
                recentf-max-menu-items 0
                recentf-max-saved-items 300
                recentf-filename-handlers
                '(file-truename) recentf-exclude (list "^/tmp/" "^/ssh:" "\\.?ido\\.last$"
                                                       "\\.revive$" "/TAGS$" "^/var/folders/.+$")))

;; display time on status line
;; (display-time-mode 1)

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
  :diminish (ivy-recentf ivy-read)
  :bind(("C-x b" . ivy-switch-buffer)
        ("C-c C-r" . ivy-resume))
  :config(ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  ;; https://github.com/abo-abo/swiper/issues/828
  (setq ivy-display-style 'fancy)
  ;; https://github.com/abo-abo/swiper/issues/1218
  (setq ivy-dynamic-exhibit-delay-ms 250)
  ;; https://oremacs.com/2017/11/30/ivy-0.10.0/
  (setq ivy-use-selectable-prompt t))

;; counsel
;; it looks like counsel is a requirement for swiper
(use-package
  counsel
  :bind(("M-x" . counsel-M-x)
        ("C-x C-f" . counsel-find-file)
        ("C-h f" . counsel-describe-function)
        ("C-h v" . counsel-describe-variable)
        ("C-h u" . counsel-unicode-char)
        ("C-h l" . counsel-find-library)
        ("C-c g" . counsel-git)
        ("C-c j" . counsel-git-grep)
        ("C-c k" . counsel-fzf)
        ("C-c l" . counsel-locate)))

;; swiper
(use-package
  swiper
  :after counsel
  :diminish (ivy-recentf ivy-read)
  :bind (("C-s" . swiper))
  :config (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

;; avy
(use-package
  avy
  :commands(avy-goto-char-2 avy-goto-line)
  :config (setq avy-all-windows nil avy-background t))

;; smartparens
(use-package
  smartparens
  :config (smartparens-global-mode t)
  (require 'smartparens-config)
  (setq sp-autowrap-region nil         ; let evil-surround handle this
        sp-highlight-pair-overlay nil
        sp-escape-quotes-after-insert nil
        sp-cancel-autoskip-on-backward-movement nil
        sp-show-pair-delay 0
        sp-max-pair-length 3)
  (sp-local-pair '(xml-mode nxml-mode php-mode) "<!--" "-->"
                 :post-handlers '(("| " "SPC"))))

;; expand-region
(use-package
  expand-region
  :commands(er/expand-region er/mark-word er/mark-inside-quotes er/mark-outside-quotes
                             er/mark-inside-pairs er/mark-outside-pairs)
  :bind(("C-=" . er/expand-region)
        ("C-'" . er/mark-inside-quotes)))

;; undo
(use-package
  undo-tree
  :init (global-undo-tree-mode 1)
  ;; make ctrl-z undo
  :bind(("C-z" . undo))
  :config
  ;; make ctrl-Z redo
  (defalias 'redo 'undo-tree-redo))

;; dired
(use-package dired
  :ensure nil
  :config
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'always)
  (put 'dired-find-alternate-file 'disabled nil)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

;; dired-x
(use-package dired-x
  :ensure nil
  :hook ((dired-mode . dired-omit-mode)
         (dired-mode . dired-hide-details-mode))
  :bind (("s-\\" . dired-jump-other-window)
         :package dired
         :map dired-mode-map
         (")" . dired-omit-mode)))

;; dired-k
;; dired-k kills git processes before they can release index.lock
;; https://github.com/syohex/emacs-dired-k/issues/45
;; (use-package dired-k
;;   :after dired
;;   :config
;;   (setq dired-k-style 'git)
;;   (defun liubang/dired-k-highlight (orig-fn &rest args)
;;     (unless (file-remote-p default-directory)
;;       (apply orig-fn args)))
;;   (advice-add #'dired-k--highlight :around #'liubang/dired-k-highlight)
;;   (add-hook 'dired-initial-position-hook #'dired-k)
;;   (add-hook 'dired-after-readin-hook #'dired-k-no-revert))

;; eshell
(use-package eshell
  :commands eshell-mode
  :init
  (setq eshell-directory-name (concat lg-cache-dir "/eshell")
        eshell-scroll-to-bottom-on-input 'all
        eshell-scroll-to-bottom-on-output 'all
        eshell-buffer-shorthand t
        eshell-kill-processes-on-exit t
        eshell-glob-case-insensitive t
        eshell-error-if-no-glob t))

(provide 'lg-editor)
