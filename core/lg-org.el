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

(require 'ox-html)
(require 'ox-publish)

;; org-reveal
(setq org-reveal-mathjax t)
(setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js@3.7.0/js/reveal.js")

;; agenda toto keywords
(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

(defvar lg-gtd-gtd-file (concat lg-gtd-dir "/gtd.org"))
(defvar lg-gtd-inbox-file (concat lg-gtd-dir "/inbox.org"))
(defvar lg-gtd-note-file (concat lg-gtd-dir "/note.org"))

;;(setq org-agenda-files nil)
(setq org-agenda-files (directory-files-recursively lg-gtd-dir "\.org$"))

(setq org-capture-templates '(("t" "Todo [gtd]" entry
                               (file+headline lg-gtd-gtd-file "Task")
                               "* TODO %?\n  %u\n  %a")
                              ("n" "Note" entry
                               (file+headline lg-gtd-note-file "Note")
                               "* %^{标题} %t %^g\n  %?\n")
                              ("i" "Inbox" entry
                               (file+headline lg-gtd-inbox-file "Inbox")
                               "* %U - %^{标题} %^g\n %?\n")))

;; fontify code in code blocks
(setq org-src-fontify-natively t)
(setq org-level-color-stars-only nil)
(setq org-hide-leading-stars nil)
(setq org-hide-emphasis-markers t)
(setq org-tags-column 80)
(setq org-agenda-inhibit-startup t) ;; ~50x speedup
(setq org-agenda-use-tag-inheritance nil) ;; 3-4x speedup

;; Org-mode Refile
(setq org-refile-targets (list (cons nil (cons :maxlevel 6))))

;; Set Org-mode Inline Image Default Size
(setq org-image-actual-width '(600))

(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; org-indent
(eval-after-load "org-indent" '(diminish 'org-indent-mode))

(provide 'lg-org)
