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

;; org
(use-package
  org
  :commands org-mode
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . org-iswitchb)
         ("C-c C-c" . org-edit-src-exit))
  :config
  (progn
    ;; agenda toto keywords
    (setq org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                                    (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|"
                                              "CANCELLED(c@/!)" "PHONE" "MEETING"))))
    ;; Fast Todo Selection
    (setq org-use-fast-todo-selection t)
    ;; allows changing todo states with S-left and S-right skipping all of the normal processing
    ;; when entering or leaving a todo state.
    (setq org-treat-S-cursor-todo-selection-as-state-change nil)
    ;; agenda files
    (defvar lg-gtd-inbox-file (concat lg-gtd-dir "/inbox.org"))
    (defvar lg-gtd-gtd-file (concat lg-gtd-dir "/gtd.org"))
    (defvar lg-gtd-done-file (concat lg-gtd-dir "/done.org"))
    (defvar lg-gtd-note-file (concat lg-gtd-dir "/note.org"))
    (defvar lg-gtd-meeting-file (concat lg-gtd-dir "/meeting.org"))
    ;; for my work
    (defvar lg-gtd-weibo-gtd-file (concat lg-gtd-dir "/weibo.org"))
    ;; set agenda files directory
    (add-to-list 'org-agenda-files lg-gtd-inbox-file)
    (add-to-list 'org-agenda-files lg-gtd-gtd-file)
    (add-to-list 'org-agenda-files lg-gtd-done-file)
    (add-to-list 'org-agenda-files lg-gtd-note-file)
    (add-to-list 'org-agenda-files lg-gtd-meeting-file)
    (add-to-list 'org-agenda-files lg-gtd-weibo-gtd-file)
    ;; org capture templates
    (setq org-capture-templates (quote (("t" "todo" entry (file lg-gtd-inbox-file)
                                         "* TODO %?\n%U\n%a\n"
                                         :clock-in t
                                         :clock-resume t)
                                         ("T" "todo" entry (file lg-gtd-gtd-file)
                                         "* TODO %?\n%U\n%a\n"
                                         :clock-in t
                                         :clock-resume t)
                                         ("w" "todo" entry (file lg-gtd-weibo-gtd-file)
                                         "* TODO %?\n%U\n%a\n"
                                         :clock-in t
                                         :clock-resume t)
                                         ("n" "note" entry (file lg-gtd-note-file)
                                         "* %? :NOTE:\n%U\n%a\n"
                                         :clock-in t
                                         :clock-resume t)
                                         ("m" "Meeting" entry (file lg-gtd-meeting-file)
                                         "* MEETING with %? :MEETING:\n%U"
                                         :clock-in t
                                         :clock-resume t))))
    ;; fontify code in code blocks
    (setq org-src-fontify-natively t)
    (setq org-level-color-stars-only nil)
    (setq org-hide-leading-stars nil)
    (setq org-hide-emphasis-markers t)
    (setq org-tags-column 80)
    (setq org-agenda-inhibit-startup t)       ;; ~50x speedup
    (setq org-agenda-use-tag-inheritance nil) ;; 3-4x speedup
    ;; targets include this file and any file contributing to the agenda - up to 9 levels deep
    (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                     (org-agenda-files :maxlevel . 9))))
    ;; use full outline paths for refile targets - we file directly with IDO
    (setq org-refile-use-outline-path t)
    ;; set Org-mode Inline Image Default Size
    (setq org-image-actual-width '(600))
    ;; org-indent
    (eval-after-load "org-indent" '(diminish 'org-indent-mode))))

;; tps://github.com/zweifisch/ob-http
(use-package ob-http
  :after org
  :defer t)

;; https://github.com/pope/ob-go
(use-package ob-go
  :after org
  :defer t)

;; More fancy UI
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(unless sys/win32p
  (use-package org-fancy-priorities
    :diminish
    :defines org-fancy-priorities-list 
    :hook (org-mode . org-fancy-priorities-mode) 
    :config (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕"))))

;; toc-org
(use-package toc-org
  :commands toc-org-enable)

;; Preview
  (use-package org-preview-html
    :diminish org-preview-html-mode)

;; htmlize
(use-package htmlize
  :commands (htmlize-buffer
            htmlize-file
            htmlize-many-files
            htmlize-many-files-dired
            htmlize-region))

;; org-reveal
(use-package
  ox-reveal
  :defer t
  :config (setq org-reveal-mathjax t)
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js@3.7.0/js/reveal.js"))

(provide 'lg-org)
