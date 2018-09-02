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

;; Don't open a file in a new frame
(when (boundp 'ns-pop-up-frames)
  (setq ns-pop-up-frames nil))

;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

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
;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000)

(setq inhibit-splash-screen 1
      ;; https://stackoverflow.com/questions/2081577/setting-emacs-split-to-horizontal
      split-height-threshold nil
      split-width-threshold 0
      ;; fullscreen on startup
      initial-frame-alist (quote ((fullscreen . maximized))))

;; hl-line
(use-package hl-line
  :ensure nil
  :hook ((prog-mode text-mode conf-mode) . hl-line-mode)
  :config
  (if (version<= "26.0.50" emacs-version)
      (progn
        (add-hook 'prog-mode-hook #'display-line-numbers-mode)
        (add-hook 'text-mode-hook #'display-line-numbers-mode))
      (progn
        (add-hook 'prog-mode-hook #'linum-mode)
        (add-hook 'text-mode-hook #'linum-mode)))
  
  ;; hlinum
  (use-package hlinum
     :config
     (hlinum-activate))
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil)
  (when (boundp 'display-line-numbers)
    (defun liubang/line-range ()
      (cons (line-beginning-position)
            (cond ((save-excursion
                     (goto-char (line-end-position))
                     (and (eobp) (not (bolp))))
                   (1- (line-end-position)))
                  ((or (eobp) (save-excursion (forward-line) (eobp)))
                   (line-end-position))
                  (t
                   (line-beginning-position 2)))))
    (setq hl-line-range-function #'liubang/line-range)))

;; theme
(use-package
  doom-themes
  :config
  (load-theme 'doom-one t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))
;; (use-package
;;   zenburn-theme
;;   :config (load-theme 'zenburn t))

;; font
(if window-system
    (progn
      (setq fonts (cond ((eq system-type 'darwin)
                         '("Monaco"    "PingFang SC"))
                        ((eq system-type 'gnu/linux) 
                         '("Monaco"    "Microsoft Yahei"))))

      (setq font-size (cond ((eq system-type 'darwin) 14)
                            ((eq system-type 'gnu/linux) 14)))
      (set-face-attribute 'default nil 
                          :font (format "%s:pixelsize=%d" (car fonts) font-size))
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font) charset (font-spec :family (car (cdr fonts)))))
      ;; Fix chinese font width and rescale
      (setq face-font-rescale-alist '(("Microsoft Yahei" . 1.2)
                                      ("WenQuanYi Micro Hei Mono" . 1.2) 
                                      ("PingFang SC". 1.2)))))

;; neotree
(use-package
  neotree
  :commands(neotree-show
            neotree-hide
            neotree-toggle
            neotree-dir
            neotree-find
            neo-global--with-buffer
            neo-global--window-exists-p)
  :after all-the-icons
  :init (global-set-key [f4] 'neotree-toggle))

;; all-the-icons
(use-package
  all-the-icons)

;; dashboard
(use-package
  dashboard
  :config
  (setq show-week-agenda-p t
        dashboard-banner-logo-title "Welcome to Liubang's Emacs")
  (dashboard-setup-startup-hook))

;; doom-modeline
(use-package doom-modeline
  :defer t
  :hook (after-init . doom-modeline-init))

;; Display Time
(use-package time
  :ensure nil
  :hook (after-init . display-time-mode)
  :init
  (setq display-time-24hr-format t)
  (setq display-time-day-and-date t))

(provide 'lg-ui)
