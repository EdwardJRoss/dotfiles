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
  (define-key er/leader-map (kbd "b") #'consult-buffer)
  (define-key er/leader-map (kbd "f") #'find-file)
  (define-key er/leader-map (kbd "F") #'consult-fd)
  (define-key er/leader-map (kbd "/") #'consult-line)
  (define-key er/leader-map (kbd "w") #'save-buffer)
  (define-key er/leader-map (kbd "u") #'undo)
  (define-key er/leader-map (kbd ":") #'execute-extended-command)
  (define-key er/leader-map (kbd "x") #'execute-extended-command)
  (define-key er/leader-map (kbd "g") #'consult-ripgrep)
  (define-key er/leader-map (kbd "i") #'consult-imenu)
  (define-key er/leader-map (kbd "p") #'project-switch-project)
  (define-key er/leader-map (kbd "r") #'recentf-open-files)
  (define-key er/leader-map (kbd "y") #'consult-yank-pop))

(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1))

(use-package recentf
  :ensure nil
  :init
  (setq recentf-max-saved-items 500
        recentf-auto-cleanup 'never)
  (recentf-mode 1))

(use-package project
  :ensure nil)

(use-package vertico
  :init
  (vertico-mode 1))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode 1))

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-x C-r" . recentf-open-files)
         ("M-s l" . consult-line)
         ("M-y" . consult-yank-pop))
  :config
  (setq consult-fd-args
        (cond
         ((executable-find "fd")
          "fd --color=never --full-path")
         ((executable-find "fdfind")
          "fdfind --color=never --full-path")
         (t nil))))

;; Keep Customize settings separate from hand-written config.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
