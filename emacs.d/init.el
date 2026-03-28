;;; init.el --- Minimal Emacs config -*- lexical-binding: t; -*-

;;; Commentary:
;; Start from a blank slate and add functionality back incrementally.

;;; Code:

(setq inhibit-startup-message t
      initial-scratch-message "")

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(require 'use-package)

(setq use-package-always-ensure t)

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil)
  :config
  (evil-mode 1)

  ;; Space leader for window management and a few common actions.
  (defvar er/leader-map (make-sparse-keymap)
    "Personal leader keymap.")
  (define-key evil-normal-state-map (kbd "SPC") er/leader-map)
  (define-key evil-motion-state-map (kbd "SPC") er/leader-map)

  ;; Window management, mirroring the C-w prefix.
  (define-key er/leader-map (kbd "h") #'evil-window-left)
  (define-key er/leader-map (kbd "j") #'evil-window-down)
  (define-key er/leader-map (kbd "k") #'evil-window-up)
  (define-key er/leader-map (kbd "l") #'evil-window-right)
  (define-key er/leader-map (kbd "s") #'evil-window-split)
  (define-key er/leader-map (kbd "v") #'evil-window-vsplit)
  (define-key er/leader-map (kbd "r") #'evil-window-rotate-downwards)
  (define-key er/leader-map (kbd "o") #'delete-other-windows)

  ;; Common actions.
  (define-key er/leader-map (kbd "q") #'delete-window)
  (define-key er/leader-map (kbd "d") #'kill-this-buffer)
  (define-key er/leader-map (kbd "b") #'switch-to-buffer)
  (define-key er/leader-map (kbd "f") #'find-file)
  (define-key er/leader-map (kbd "/") #'isearch-forward)
  (define-key er/leader-map (kbd "w") #'save-buffer)
  (define-key er/leader-map (kbd "u") #'undo)
  (define-key er/leader-map (kbd ":") #'execute-extended-command)
  (define-key er/leader-map (kbd "x") #'execute-extended-command)
  (define-key er/leader-map (kbd "g") #'goto-line))

(use-package icomplete
  :ensure nil
  :init
  (setq icomplete-separator " · ")
  (fido-vertical-mode 1))

;; Keep Customize settings separate from hand-written config.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
