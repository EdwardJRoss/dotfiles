;;; init.el --- Minimal Emacs config -*- lexical-binding: t; -*-

;;; Commentary:
;; Start from a blank slate and add functionality back incrementally.

;;; Code:

(setq inhibit-startup-message t
      initial-scratch-message "")

;; Keep Customize settings separate from hand-written config.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
