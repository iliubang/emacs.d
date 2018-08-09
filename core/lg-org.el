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

(setq org-hide-emphasis-markers t)
(font-lock-add-keywords 'org-mode
                          '(("^ +\\([-*]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(use-package org-bullets
             :ensure t)

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(when (window-system)
  (let* ((variable-tuple (cond ((x-list-fonts "Monaco") '(:font "Monaco"))
                                 ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                                 ((x-list-fonts "Verdana")         '(:font "Verdana"))
                                 ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                                 (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
           (base-font-color     (face-foreground 'default nil 'default))
           (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

      (custom-theme-set-faces 'user
                              `(org-level-8 ((t (,@headline ,@variable-tuple))))
                              `(org-level-7 ((t (,@headline ,@variable-tuple))))
                              `(org-level-6 ((t (,@headline ,@variable-tuple))))
                              `(org-level-5 ((t (,@headline ,@variable-tuple))))
                              `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
                              `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
                              `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
                              `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
                              `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil)))))))


(require 'ox-html)
(require 'ox-publish)

(use-package htmlize
             :ensure t)

;; agenda toto keywords
(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

(defvar lg-gtd-inbox-file (concat lg-gtd-dir "/inbox.org"))
(defvar lg-gtd-gtd-file (concat lg-gtd-dir "/gtd.org"))
(defvar lg-gtd-note-file (concat lg-gtd-dir "/note.org"))
(defvar lg-gtd-someday-file (concat lg-gtd-dir "/someday.org"))

(add-to-list 'org-agenda-files lg-gtd-gtd-file)
(add-to-list 'org-agenda-files lg-gtd-inbox-file)
(add-to-list 'org-agenda-files lg-gtd-note-file)

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline lg-gtd-inbox-file "任务")
                               "* TODO %?\n  %u\n  %a")
                              ("n" "Note" entry
                               (file+headline lg-gtd-note-file "笔记")
                               "* %^{heading} %t %^g\n  %?\n")
                              ("i" "Inbox" entry
                               (file+headline lg-gtd-inbox-file "Inbox")
                               "* %U - %^{heading} %^g\n %?\n")))

(setq org-refile-targets '((lg-gtd-gtd-file :maxlevel . 3)
                           (lg-gtd-tickler-file :maxlevel . 2)
                           (lg-gtd-someday-file :level . 1)))

(setq org-agenda-custom-commands
        '(("b" "liubang" tags-todo "@liubang"
           ((org-agenda-overriding-header "Liubang")
            (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))))

(defun my-org-agenda-skip-all-siblings-but-first ()
    "Skip all but the first non-done entry."
    (let (should-skip-entry)
      (unless (org-current-is-todo)
        (setq should-skip-entry t))
      (save-excursion
        (while (and (not should-skip-entry) (org-goto-sibling t))
          (when (org-current-is-todo)
            (setq should-skip-entry t))))
      (when should-skip-entry
        (or (outline-next-heading)
            (goto-char (point-max))))))

(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(provide 'lg-org)
