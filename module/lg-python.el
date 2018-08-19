;;; lg-python.el
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

(use-package 
  python 
  :mode ("\\.py\\'" . python-mode) 
  :interpreter ("python" . python-mode) 
  :preface (defun py/init-company () 
             (set (make-local-variable 'company-backends) 
                  '(company-capf company-yasnippet)) 
             (company-mode t)) 
  :hook ((python-mode . py/init-company)) 
  :init (setq python-shell-interpreter "ipython" python-shell-interpreter-args "--simple-prompt -i"))

(use-package 
  anaconda-mode 
  :after python 
  :ensure t 
  :hook ((python-mode . anaconda-mode) 
         (python-mode . anaconda-eldoc-mode)))

(use-package 
  company-anaconda 
  :after (company anaconda-mode) 
  :ensure t 
  :init (push 'company-anaconda company-backends))

(use-package 
  pyvenv 
  :ensure t 
  :hook (python-mode . pyvenv-mode))

(use-package 
  pyenv-mode 
  :hook (python-mode . pyenv-mode) 
  :commands (pyenv-mode-set pyenv-mode-unset pyenv-mode-versions) 
  :init (defun projectile-pyenv-mode-set () 
          (let ((project (projectile-project-name))) 
            (if (member project (pyenv-mode-versions)) 
                (pyenv-mode-set project) 
              (pyenv-mode-unset)))) 
  (add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set))

(use-package 
  pip-requirements 
  :ensure t 
  :after python)

;; (use-package cython-mode
;;              :ensure t
;;              :after python)

(provide 'lg-python)
