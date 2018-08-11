;;; clang.el
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

;; set .cpp/.hpp to c++-mode, and .c/.h to c-mode
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))

(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "linux")))
;; clang-format
(global-set-key (kbd "C-c i") 'clang-format-region)
(global-set-key (kbd "C-c u") 'clang-format-buffer)

(defun fix-c-indent-offset-according-to-syntax-context (key val)
  (setq c-offsets-alist (delq (assoc key c-offsets-alist) c-offsets-alist))
  (add-to-list 'c-offsets-alist '(key . val)))

(defun liubang/cedet-hook ()
  (setq c-basic-offset 4)
  (setq c-auto-newline nil)
  (setq lazy-lock-defer-contextually t)
  (setq lazy-lock-defer-time 0) 
  (c-toggle-hungry-state 1) 
  (fix-c-indent-offset-according-to-syntax-context 'substatement 0)
  (fix-c-indent-offset-according-to-syntax-context 'func-decl-cont 0)
  (setq cc-search-directories '("." "/usr/include" "/usr/local/include/*" "../*/include" "$WXWIN/include")) 
  ;; make a #define be left-aligned
  (setq c-electric-pound-behavior (quote (alignleft)))
  (when buffer-file-name
    (flymake-mode 1)
    (if (executable-find "cmake")
      (if (not (or (string-match "^/usr/local/include/.*" buffer-file-name)
                   (string-match "^/usr/src/linux/include/.*" buffer-file-name)))
					(cppcm-reload-all)))))

;; c-mode-hook
(add-hook 'c-mode-common-hook 'liubang/cedet-hook)
(add-hook 'c-mode-hook 'liubang/cedet-hook)
(add-hook 'c++-mode-hook 'liubang/cedet-hook)

(provide 'lg-clang)
