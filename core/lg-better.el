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

;; enable erase-buffer command
;; http://emacsredux.com/blog/2013/05/04/erase-buffer/
(put 'erase-buffer 'disabled nil)
(global-set-key (kbd "C-c e")  'erase-buffer)

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

;; display time on status line
(display-time-mode 1)

;; don't backup file
(setq make-backup-files nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

;; http://pragmaticemacs.com/emacs/dont-kill-buffer-kill-this-buffer-instead/
(global-set-key (kbd "C-x k") 'kill-this-buffer)
;; kill buffer without my confirmation
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; ibuffer mode
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; split
(global-set-key (kbd "C-c -") 'split-window-below)
(global-set-key (kbd "C-c |") 'split-window-right)

;; env
(if (or
     (eq system-type 'darwin)
     (eq system-type 'berkeley-unix))
    (setq system-name (car (split-string system-name "\\."))))

(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(push "/usr/local/bin" exec-path)

;; counsel
(require 'counsel)
(ivy-mode 1)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
(global-set-key (kbd "C-s") 'swiper)

;; https://github.com/abo-abo/swiper/issues/1218 
(setq ivy-dynamic-exhibit-delay-ms 250) 
;; https://oremacs.com/2017/11/30/ivy-0.10.0/ 
(setq ivy-use-selectable-prompt t)

(defun ivy-switch-buffer-matcher-pinyin (regexp candidates)
  (unless (featurep 'pinyinlib) (require 'pinyinlib))
  (let* ((pys (split-string regexp "[ \t]+"))
         (regexp (format ".*%s.*"
                         (mapconcat 'pinyinlib-build-regexp-string pys ".*"))))
    (ivy--switch-buffer-matcher regexp candidates)))

(defun ivy-switch-buffer-by-pinyin ()
  (interactive)
  (unless (featurep 'ivy) (require 'ivy))
  (let ((this-command 'ivy-switch-buffer))
    (ivy-read "Switch to buffer: " 'internal-complete-buffer
              :matcher #'ivy-switch-buffer-matcher-pinyin
              :preselect (buffer-name (other-buffer (current-buffer)))
              :action #'ivy--switch-buffer-action
              :keymap ivy-switch-buffer-map
              :caller 'ivy-switch-buffer)))

;; press "M-o" to choose ivy action
(ivy-set-actions
  'counsel-find-file
  '(("j" find-file-other-frame "other frame")
    ("b" counsel-find-file-cd-bookmark-action "cd bookmark")
    ("x" counsel-find-file-extern "open externally")
    ("d" delete-file "delete")
    ("r" counsel-find-file-as-root "open as root")))

;; set actions when running C-x b
;; replace "frame" with window to open in new window
(ivy-set-actions
  'ivy-switch-buffer-by-pinyin
  '(("j" switch-to-buffer-other-frame "other frame")
    ("k" kill-buffer "kill")
     ("r" ivy--rename-buffer-action "rename")))

(with-eval-after-load 'ivy
                      ;; https://github.com/abo-abo/swiper/issues/828
                      (setq ivy-display-style 'fancy))

;; autopair
(autopair-global-mode)
(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (push '(?` . ?')
                    (getf autopair-extra-pairs :comment))
              (push '(?` . ?')
                    (getf autopair-extra-pairs :string))))

(provide 'lg-better)
