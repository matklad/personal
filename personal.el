;;; personal.el --- personal stuff
;;; Commentary:

;;; Code:
(prelude-require-packages
 '(base16-theme
   coffee-mode
   company
   emmet-mode
   golden-ratio
   hideshow
   impatient-mode
   projectile
   tuareg
   whitespace
   yasnippet
   auctex
   fish-mode))

(require 'prelude-c)
(require 'prelude-clojure)
(require 'prelude-coffee)
(require 'prelude-css)
(require 'prelude-emacs-lisp)
(require 'prelude-haskell)
(require 'prelude-helm)
(require 'prelude-ido)
(require 'prelude-js)
(require 'prelude-latex)
(require 'prelude-lisp)
(require 'prelude-org)
(require 'prelude-python)
(require 'prelude-python)
(require 'prelude-scala)
(require 'prelude-scheme)
(require 'prelude-scss)
(require 'prelude-web)
(require 'prelude-web)
(require 'prelude-xml)

(require 'company)
(require 'prelude-ocaml)

(golden-ratio-mode 't)
;; (add-to-list 'default-frame-alist '(cursor-color . "#eee8d5"))
(setq initial-major-mode 'org-mode)
(setq initial-buffer-choice 'remember-notes)
(setq remember-notes-buffer-name "*scratch*")
(fringe-mode '(nil . 0))
(set-frame-font "Ubuntu Mono-13" nil 't)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)

(setq default-input-method 'russian-computer)

(scroll-bar-mode -1)

(setq whitespace-line-column 80)

(setq yas-snippet-dirs
      '("~/.emacs.d/personal/snippets"))
(yas-global-mode 1)

;; ;; Auto-Complete
;; (add-hook 'after-init-hook 'global-company-mode)
;; (eval-after-load 'company
;;   '(progn
;;      (setq company-backends (delete 'company-ropemacs company-backends))))
(setq completion-auto-help nil)

;;Projectile
(setq projectile-use-git-grep 't)

;; Confirm closing
(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed."
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (save-buffers-kill-emacs)
    (message "Canceled exit")))

(when (and window-system (eq system-type 'darwin))
  (global-set-key (kbd "C-x C-c") 'ask-before-closing))

;; Web

(add-to-list 'auto-mode-alist '("\\.hbs?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.dtl?\\'" . web-mode))

(setq js-indent-level 2)
(custom-set-variables '(coffee-tab-width 2))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(setq flycheck-scss-compass nil)

(setq prelude-web-mode-hook 'my-web-mode-hook)
(add-hook 'prelude-web-mode-hook 'prelude-web-mode-defaults)

(require 'emmet-mode)
(add-hook 'prelude-web-mode-hook 'emmet-mode)
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))
;; (define-key emmet-mode-keymap (kbd "C-j") 'emmet-expand-yas)

;; Coljure
(require 'cider)
;; (defun eval-insert-comment ()
;;   "Evaluate form at point and insert it as a comment."
;;   (interactive)
;;   (let* ((s (cider-last-sexp))
;;          (data (cider-eval-sync s))
;;          (v (plist-get data :value))
;;          (e (plist-get data :error)))
;;     (if v
;;         (progn
;;           (forward-line 1)
;;           (if (string-prefix-p ";;=> " (thing-at-point 'line))
;;               (kill-line 1))
;;           (insert ";;=> " v "\n")
;;           (forward-line -2)
;;           (end-of-line)))))

;; (define-key cider-mode-map (kbd "C-c e") 'eval-insert-comment)
;; (add-hook 'clojure-mode-hook 'smartparens-strict-mode)

;; LaTex
(require 'org)
(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 2.0))

(require 'prelude-latex)
(add-hook 'prelude-latex-mode-hook (lambda ()
                                     (progn
                                       (flycheck-mode nil)
                                       (flyspell-mode 't))))

(eval-after-load 'flycheck
  '(setq-default flycheck-disabled-checkers '(tex-chktex tex-lacheck)))
(setq TeX-view-program-selection
      '((output-pdf "PDF Viewer")))

(setq TeX-view-program-list
      `(("PDF Viewer" ,(if (eq system-type 'darwin) "open %o" "okular %o"))))

(setq-default TeX-engine 'xetex)
(setq-default TeX-PDF-mode t)
(require 'prelude-latex)
(defun run-latex ()
  (interactive)
  ;; (let ((process (TeX-active-process))) (if process (delete-process process)))
  (let ((TeX-save-query nil)) (TeX-save-document ""))
  (TeX-command-menu "LaTeX"))

(defun toggle-formula ()
  (interactive)
  (if (eq nil (search-forward "$" (+ (point) 4) 't 1))
      (progn
        (insert "$$")
        (backward-char)
        (deactivate-input-method))
    (toggle-input-method)))

(add-hook 'LaTeX-mode-hook
          (lambda () (progn
                       (local-set-key (kbd "C-x C-s") #'run-latex)
                       (local-set-key (kbd "C-c C-f") #'toggle-formula))))

;;C++
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook 'c++-mode-hook
          (lambda () (setq flycheck-clang-language-standard "c++14")))

(defconst my-cc-style
  '("bsd"
    (c-offsets-alist . ((innamespace . [0])))))
(c-add-style "my-cc-style" my-cc-style)

(setq c-default-style "my-cc-style")

;; Just The Stuff
(define-key prelude-mode-map (kbd "s-k") 'magit-status)
(define-key prelude-mode-map (kbd "M-k") 'magit-status)
(setq magit-status-buffer-switch-function 'switch-to-buffer)

(add-hook 'prelude-python-mode-hook (lambda () (hs-minor-mode 't)))
(global-set-key (kbd "<f2>") 'hs-toggle-hiding)

(defun open-line-and-scroll ()
  (interactive
   (progn
     (prelude-smart-open-line nil)
     (recenter))))

(define-key prelude-mode-map (kbd "M-o") 'open-line-and-scroll)

(load-file "/usr/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el")

;; Add opam emacs directory to the load-path
(setq opam-share
      (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
;; Load merlin-mode
(require 'merlin)
;; Start merlin on ocaml files
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t)
;; Enable auto-complete
(setq merlin-use-auto-complete-mode 'easy)
;; Use opam switch to lookup ocamlmerlin binary
(setq merlin-command 'opam)
(add-to-list 'company-backends 'merlin-company-backend)
(add-hook 'merlin-mode-hook 'company-mode)
;;; personal.el ends here
