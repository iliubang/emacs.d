;;; lg-utils.el
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

;; reload init file
(defun liubang/reload-init-file()
  (interactive)
  (load-file (concat lg-dir "/init.el")))

;; http://ergoemacs.org/emacs/emacs_show_key_and_command.html
(use-package
  command-log-mode
  :commands(command-log-mode global-command-log-mode clm/open-command-log-buffer)
  :config (setq command-log-mode-auto-show t command-log-mode-open-log-turns-on-mode t))

(defun liubang/live-coding ()
  (interactive)
  (clm/open-command-log-buffer))

(defun liubang/print-path()
  (interactive)
  (message (getenv "PATH")))

(use-package elisp-format 
  :commands(elisp-format-buffer elisp-format-file elisp-foramt-directory elisp-format-region elisp-format-directory-batch
                                elisp-format-dired-mark-files elisp-format-library))

(provide 'lg-utils)
