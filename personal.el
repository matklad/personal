;;; personal.el --- personal stuff
;;; Commentary:

;;; Code:
(prelude-require-packages '(tuareg golden-ratio auto-complete ac-nrepl
                                   base16-theme emmet-mode yasnippet))

(load-theme 'base16-solarized)
(add-to-list 'default-frame-alist '(cursor-color . "#eee8d5"))

(require 'hl-line)
(global-hl-line-mode 0)


(golden-ratio-mode 1)

(require 'whitespace)
(setq whitespace-line-column 120)

(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/personal/snippets"))
(yas-global-mode 1)


;; Auto-Complete

(require 'auto-complete)
(global-auto-complete-mode t)
(setq ac-delay 0.05)


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

;;; personal.el ends here
