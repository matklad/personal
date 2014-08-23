;;; update.el --- make sure personal.el is up-to-date
;;; Commentary:

;;; Code:
(cd "~/.emacs.d/personal")
(call-process-shell-command "git pull")

;;; update.el ends here
