;;; init.el --- Minimal Emacs config -*- lexical-binding: t; -*-

;;; Commentary:
;; Start from a blank slate and add functionality back incrementally.

;;; Code:

(setq inhibit-startup-message t
      initial-scratch-message "")

;; Keep backup and autosave files out of project directories.
(dolist (dir '("backups/" "auto-save/"))
  (make-directory (expand-file-name dir user-emacs-directory) t))

(setq backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory)))
      auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save/" user-emacs-directory) t))
      auto-save-default t
      make-backup-files t
      delete-old-versions t
      kept-new-versions 10
      kept-old-versions 3
      version-control t
      create-lockfiles nil)

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
        evil-want-keybinding nil
        evil-symbol-word-search t)
  :config
  (evil-mode 1)

  ;; Space leader for window management and a few common actions.
  (defvar er/leader-map (make-sparse-keymap)
    "Personal leader keymap.")
  (define-key evil-normal-state-map (kbd "SPC") er/leader-map)
  (define-key evil-motion-state-map (kbd "SPC") er/leader-map)
  (define-key evil-normal-state-map (kbd "g]") #'xref-find-definitions)
  (define-key evil-normal-state-map (kbd "g[") #'xref-go-back)

  ;; Window management, mirroring the C-w prefix.
  (define-key er/leader-map (kbd "h") #'evil-window-left)
  (define-key er/leader-map (kbd "j") #'evil-window-down)
  (define-key er/leader-map (kbd "k") #'evil-window-up)
  (define-key er/leader-map (kbd "l") #'evil-window-right)
  (define-key er/leader-map (kbd "s") #'evil-window-split)
  (define-key er/leader-map (kbd "v") #'evil-window-vsplit)
  (define-key er/leader-map (kbd "r") #'evil-window-rotate-downwards)
  (define-key er/leader-map (kbd "R") #'evil-window-rotate-upwards)
  (define-key er/leader-map (kbd "+") #'evil-window-increase-height)
  (define-key er/leader-map (kbd "-") #'evil-window-decrease-height)
  (define-key er/leader-map (kbd "=") #'balance-windows)
  (define-key er/leader-map (kbd "_") #'evil-window-set-height)
  (define-key er/leader-map (kbd "o") #'delete-other-windows)

  ;; Common actions.
  (define-key er/leader-map (kbd "q") #'delete-window)
  (define-key er/leader-map (kbd "d") #'kill-current-buffer)
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
  (define-key er/leader-map (kbd ",") #'recentf-open-files)
  (define-key er/leader-map (kbd "y") #'consult-yank-pop)

  ;; Code actions and language tooling.
  (defvar er/code-map (make-sparse-keymap)
    "Personal code action keymap.")
  (define-key er/leader-map (kbd "c") er/code-map)

  ;; Text scaling commands.
  (defvar er/zoom-map (make-sparse-keymap)
    "Personal text scaling keymap.")
  (define-key er/leader-map (kbd "z") er/zoom-map)
  (define-key er/zoom-map (kbd "+") #'text-scale-increase)
  (define-key er/zoom-map (kbd "-") #'text-scale-decrease)
  (define-key er/zoom-map (kbd "0") #'text-scale-adjust)

  ;; Application launcher prefix. Keep "-" as a compatibility alias
  ;; while migrating toward the leader-based binding.
  (defvar er/app-map (make-sparse-keymap)
    "Personal application launcher keymap.")
  (define-key er/leader-map (kbd "a") er/app-map)
  (define-key evil-normal-state-map (kbd "-") er/app-map)
  (define-key er/app-map (kbd "s") #'eshell)
  (define-key er/app-map (kbd "S") #'shell)
  (define-key er/app-map (kbd "t") #'eat)
  (define-key er/app-map (kbd "T") #'term)
  (define-key er/app-map (kbd "d") #'dired))

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

(use-package corfu
  :init
  (global-corfu-mode 1)
  :custom
  (corfu-auto t)
  (corfu-cycle t))

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

(use-package eat
  :init
  (eat-eshell-mode 1)
  :commands (eat eat-other-window))

(use-package which-key
  :ensure nil
  :init
  (setq which-key-idle-delay 0.4
        which-key-idle-secondary-delay 0.05
        which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-side-window-max-height 0.25
        which-key-max-description-length 40)
  :config
  (which-key-mode 1)
  (which-key-add-key-based-replacements
    "SPC" "leader"
    "SPC a" "apps"
    "SPC c" "code"
    "SPC z" "zoom"))

(use-package eglot
  :ensure nil
  :hook ((python-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (sh-mode . eglot-ensure)
         (bash-ts-mode . eglot-ensure))
  :config
  (setq eglot-autoshutdown t)

  (define-key er/code-map (kbd "a") #'eglot-code-actions)
  (define-key er/code-map (kbd "f") #'eglot-format)
  (define-key er/code-map (kbd "r") #'eglot-rename)
  (define-key er/code-map (kbd "d") #'xref-find-definitions)
  (define-key er/code-map (kbd "n") #'flymake-goto-next-error)
  (define-key er/code-map (kbd "p") #'flymake-goto-prev-error)
  (define-key er/code-map (kbd "e") #'consult-flymake)
  (define-key er/code-map (kbd "h") #'xref-find-references))

;; Keep Customize settings separate from hand-written config.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
