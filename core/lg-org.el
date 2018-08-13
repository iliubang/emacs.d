;;; lg-org.el
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

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq org-bullets-bullet-list '("✙" "♱" "♰" "☥" "✞" "✟" "✝" "†" "✠" "✚" "✜" "✛" "✢" "✣" "✤" "✥"))

(require 'ox-html)
(require 'ox-publish)

;; org-reveal
(setq org-reveal-mathjax t)
(setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js@3.7.0/js/reveal.js")

;; agenda toto keywords
;; (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

;; Fast Todo Selection
(setq org-use-fast-todo-selection t)
;; allows changing todo states with S-left and S-right skipping all of the normal processing when entering or leaving a todo state.
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

;; TODO state triggers
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

;; toto keyword faces
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

(defvar lg-gtd-gtd-file (concat lg-gtd-dir "/gtd.org"))
(defvar lg-gtd-note-file (concat lg-gtd-dir "/note.org"))
(defvar lg-gtd-meeting-file (concat lg-gtd-dir "/meeting.org"))
;; for my work
(defvar lg-gtd-weibo-gtd-file (concat lg-gtd-dir "/weibo.org"))

;; set agenda files directory
(add-to-list 'org-agenda-files lg-gtd-gtd-file)
(add-to-list 'org-agenda-files lg-gtd-note-file)
(add-to-list 'org-agenda-files lg-gtd-meeting-file)
(add-to-list 'org-agenda-files lg-gtd-weibo-gtd-file)

(setq org-capture-templates
      (quote (("t" "todo" entry (file lg-gtd-gtd-file)
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("w" "todo" entry (file lg-gtd-weibo-gtd-file)
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("n" "note" entry (file lg-gtd-note-file)
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("m" "Meeting" entry (file lg-gtd-meeting-file)
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t))))

;; fontify code in code blocks
(setq org-src-fontify-natively t)
(setq org-level-color-stars-only nil)
(setq org-hide-leading-stars nil)
(setq org-hide-emphasis-markers t)
(setq org-tags-column 80)
(setq org-agenda-inhibit-startup t) ;; ~50x speedup
(setq org-agenda-use-tag-inheritance nil) ;; 3-4x speedup


;; Set Org-mode Inline Image Default Size
(setq org-image-actual-width '(600))

;; key
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-iswitchb)

;; org-indent
(eval-after-load "org-indent" '(diminish 'org-indent-mode))

(provide 'lg-org)
