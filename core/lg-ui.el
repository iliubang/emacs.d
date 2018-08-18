;;; lg-ui.el
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

;; auto wrap
(setq truncate-lines nil)
;; encoding
;; set the default encoding system
(prefer-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8-unix
      default-file-name-coding-system 'utf-8-unix
      default-keyboard-coding-system 'utf-8-unix
      default-process-coding-system '(utf-8-unix . utf-8-unix)
      default-sendmail-coding-system 'utf-8-unix
      default-terminal-coding-system 'utf-8-unix)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; custom ui
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; show cursor position within line
(column-number-mode 1)
(global-linum-mode 1)
(setq inhibit-splash-screen 1)
;; fullscreen on startup
(setq initial-frame-alist (quote ((fullscreen . maximized))))
;; highlight current line
(global-hl-line-mode 1)

;; https://stackoverflow.com/questions/2081577/setting-emacs-split-to-horizontal
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; theme
;; (load-theme 'solarized-light t)
(use-package zenburn-theme
             :ensure t
             :config
             (load-theme 'zenburn t))

;; font
(setq fonts
      (cond ((eq system-type 'darwin)     '("Monaco"    "PingFang SC"))
            ((eq system-type 'gnu/linux)  '("Menlo"     "WenQuanYi Zen Hei"))
            ((eq system-type 'windows-nt) '("Consolas"  "Microsoft Yahei"))))

(set-face-attribute 'default nil :font
                    (format "%s:pixelsize=%d" (car fonts) 12))
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font) charset
                    (font-spec :family (car (cdr fonts)))))
;; Fix chinese font width and rescale
(setq face-font-rescale-alist '(("Microsoft Yahei" . 1.2) ("WenQuanYi Micro Hei Mono" . 1.2) ("PingFang SC". 1.2)))


;; dashboard
(use-package dashboard
             :ensure t
             :config
             (setq show-week-agenda-p t)
             (setq dashboard-banner-logo-title "Welcome to Liubang's Emacs")
             (dashboard-setup-startup-hook))

;; spaceline
(use-package spaceline
             :ensure t
             :config
             (spaceline-emacs-theme))

(provide 'lg-ui)
