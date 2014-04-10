;;; personal.el --- personal stuff
;;; Commentary:

;;; Code:
(prelude-require-packages '(tuareg golden-ratio company company-cider
                                   base16-theme emmet-mode yasnippet))

(load-theme 'base16-solarized)
(add-to-list 'default-frame-alist '(cursor-color . "#eee8d5"))

(require 'hl-line)
(global-hl-line-mode 0)

(scroll-bar-mode -1)

(golden-ratio-mode 1)

(require 'whitespace)
(setq whitespace-line-column 120)

(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/personal/snippets"))
(yas-global-mode 1)

;; Auto-Complete
(require 'company)
(require 'company-cider)
(setq company-idle-delay t)
(setq completion-auto-help nil)
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-cider))

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

(require 'web-mode)
(require 'prelude-web)
(require 'emmet-mode)

(add-to-list 'auto-mode-alist '("\\.hbs?\\'" . web-mode))

(setq js-indent-level 2)
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(setq prelude-web-mode-hook 'my-web-mode-hook)
(add-hook 'prelude-web-mode-hook 'prelude-web-mode-defaults)

(add-hook 'prelude-web-mode-hook 'emmet-mode)
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))

;; OCaml
(if (= 0 (shell-command "which opam"))
    (progn
        (add-to-list 'load-path
                     (concat
                      (replace-regexp-in-string
                       "\n$" ""
                       (shell-command-to-string "opam config var share"))
                      "/emacs/site-lisp"))

        (let* ((opam-prefix
                (substring (shell-command-to-string "opam config var prefix") 0 -1)))
          (with-temp-buffer
            (insert (shell-command-to-string
                     (concat opam-prefix
                             "/bin/ocp-edit-mode emacs -load-global-config")))
            (eval-buffer)))

      (require 'ocp-indent)
      (require 'ocp-index)
      (require 'tuareg)
      (setq ocp-indent-config "with_never=true")

      (autoload 'merlin-mode "merlin" "Merlin mode" t)
      (add-hook 'tuareg-mode-hook 'merlin-mode)
      (add-hook 'caml-mode-hook 'merlin-mode)
      (eval-after-load 'merlin
        '(progn (define-key merlin-mode-map (kbd "C-<up>") nil)
                (define-key merlin-mode-map (kbd "C-<down>") nil)))))

;; Coljure
(require 'cider)
(defun eval-insert-comment ()
  "Evaluate form at point and insert it as a comment."
  (interactive)
  (let* ((s (cider-last-sexp))
         (data (cider-eval-sync s))
         (v (plist-get data :value)))
    (forward-line 1)
    (kill-line 1)
    (insert ";; " v "\n")
    (forward-line -2)
    (end-of-line)))
(define-key cider-mode-map [remap cider-eval-last-sexp] 'eval-insert-comment)
(add-hook 'clojure-mode-hook 'smartparens-strict-mode)

;;; personal.el ends here
