;; -*-Lisp-*-
;;; init.el --- Configuration for Edward Ross

;; Author: Edward Ross <edward@skeptric.com>

;;; Commentary:

;; Personal configuration file for Vim like Emacs

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General configuration independent of packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
;; https://blog.d46.us/advanced-emacs-startup/
(setq gc-cons-threshold (* 50 1000 1000))

;; Add custom lisp
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Never use tabs - always spaces
(setq-default indent-tabs-mode nil)
;; Remove trailing whitespace on each save
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; Show risky whitespace
;; Lines shows overlong lines; sometimes useful sometimes annoying
;; Empty is too glaring when editing text, but useful when viewing
;; TODO Add toggle keys
;; TODO Make the highlighting less obnoxious
;(defvar er/whitespace-extra-style '(lines empty trailing))
(setq-default whitespace-style '(face tabs tab-mark space-before-tab
                                 space-after-tab))

(global-whitespace-mode)
;; In lieu of empty mark the empty lines
(setq-default indicate-empty-lines t)

; keys used for "application" map
(defvar er/application-evil-prefix-key (kbd "-"))
(defvar er/application-prefix-key (kbd "<f6>"))


;; My own place to keep application shortcuts
(defalias 'er/application-prefix (make-sparse-keymap))
(defvar er/application-map (symbol-function 'er/application-prefix)
  "Keymap for application launching keys.")
(define-key global-map er/application-prefix-key 'er/application-prefix)

(defvar er/leader-evil-prefix-key (kbd "<SPC>"))
(defvar er/leader-prefix-key (kbd "C-<SPC>"))

(defalias 'er/leader-prefix (make-sparse-keymap))
(defvar er/leader-map (symbol-function 'er/leader-prefix)
  "Keymap for personal shortcusts.")
(define-key global-map er/leader-prefix-key 'er/leader-prefix)


;; UTF-8 Everywhere
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;;; Turn off blinking cursor
(blink-cursor-mode 0)
;;; Stop the computer from beeping when you do something stupid
(setq ring-bell-function 'ignore)

;;; No welcome message on startup
(setq
 initial-scratch-message ""
 inhibit-startup-message t
 inhibit-startup-echo-area-message t)

;; Turn off tool/scroll bars because they take spaec
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;; Show identically named buffers by their path instead of as <2>
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Remember place in buffer between sessions
(save-place-mode 1)

;; Follow symlinks to version controlled files
(setq vc-follow-symlinks t)

;; General configuration
;; TODO: Review https://github.com/technomancy/emacs-starter-kit
(setq mouse-yank-at-point t
      save-interprogram-paste-before-kill t
      load-prefer-newer t)

;; Save backups in the user directory
(let ((backup (concat user-emacs-directory "backups"))
      (auto-save (concat user-emacs-directory "autosaves")))
  ;; Ensure backup directories exist
  (if (not (file-directory-p backup))
      (mkdir backup))
  (if (not (file-directory-p auto-save))
      (mkdir auto-save))
  (setq
   backup-directory-alist `(("." . ,backup))
   auto-save-file-name-transforms `((".*" ,(concat auto-save "/\\1") t))))

;; Show column as well as line number
(setq column-number-mode t)
;
;;; Aesthetics: This colour scheme has better visual marker
;;(load-theme 'tsdh-light)
(load-theme 'wombat)

;; Use Bing as default search browser as it renders very quickly
;; Emacs Web Wowser: https://www.gnu.org/software/emacs/manual/html_mono/eww.html
(setq eww-search-prefix "https://www.bing.com/search?q=")

;; Make eww more readabe with dark theme
(setq shr-color-visible-luminance-min 70)

;; If a file is a script (has a shebang), save it as executable
(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)


;; When using comint with Python/R/whatever scroll to bottom
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cygwin Compatibility
;; See also https://www.emacswiki.org/emacs/setup-cygwin.el
;;
;; For info to be complete see http://pipeline.lbl.gov/code/3rd_party/licenses.win/cygwin-doc-1.4/html/faq/index.html#faq.using.info-error
;; (In /usr/share/info run:
;;    for f in *.info.gz ; do install-info $f dir ; done)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (eq system-type 'cygwin)
    (progn
      ; Locally installed things (e.g. Notmuch)
      (add-to-list 'load-path "/usr/local/share/emacs/site-lisp")

      (setq shell-file-name "/usr/bin/bash")
      (setenv "PATH"
              (concat "/usr/local/bin:/usr/bin:"
                      (getenv "PATH")))
      (setq exec-path (append exec-path '("/usr/local/bin" "/usr/bin")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bootstrap use-package: https://github.com/jwiegley/use-package/
;; Should work automatically in any Emacs version since package.el (v24)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)(package-initialize)
(setq package-enable-at-startup nil)

;; Should only need to be run once on any installation
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; This will install packages if they are not
;; Comment out this line if you don't want to automatically install packages
;; Note that Evil mode below will not respect this and install anyway
(setq use-package-always-ensure t)

;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scrolling windows - should be put in a separate file
;; Need a special predicate for DocView (PDF) mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun split-window-above (&optional size)
  "Create a pane split above of SIZE lines."
  (interactive "P")
  (if size
      (split-window-below (- size))
    (split-window-below))
  (evil-window-down 1)
  )

(require 'skeptric-scroll)


;; https://www.reddit.com/r/emacs/comments/1zkj2d/advanced_usage_of_eshell/
(defun er/eshell-here ()
  "Go to eshell and set current directory to the buffer's directory"
  (interactive)
  (let ((dir (file-name-directory (or (buffer-file-name)
                                      default-directory))))
    (eshell)
    (eshell/pushd ".")
    (cd dir)
    (goto-char (point-max))
    (eshell-kill-input)
    (eshell-send-input)))

;; TODO: Make into packages with use-package

(require 'presto)
(require 'athena)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil - Vi Emulation Layer for Emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil
  :demand t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :bind (
         :map evil-normal-state-map
         ;; I find this more useful than Undo
         ("U" . 'undo-tree-redo)
         ;; M acts like zz,zb,zt
         ("M" . 'recenter-top-bottom)
         ;; Comment in/out
         ("z;" . 'comment-dwim)
         :map er/leader-map
         ("<SPC>" . 'evil-execute-in-god-state)
         (";" . 'evil-ex)

         ("a" . 'er/application-prefix)

         ("x" . 'Control-X-prefix)
         ("u" . 'universal-argument)

         ("!" . 'term)

         ("d" . 'kill-this-buffer)
         ("D" . 'kill-buffer)
         ("f" . 'find-file)
         ("w" . 'save-buffer)
         ("W" . 'rename-file-and-buffer)
         ("b" . 'switch-to-buffer)
         ("B" . 'list-buffers)
         ("q" . 'delete-window)
         ("Q" . 'kill-buffer-and-window)

         ("s" . 'evil-window-split)
         ("v" . 'evil-window-vsplit)
         ("o" . 'delete-other-windows)
         ("r" . 'evil-window-rotate-downwards)

         ("=" . 'balance-windows)
         ("+" . 'evil-window-increase-height)
         ("-" . 'evil-window-decrease-height)
         (">" . 'evil-window-increase-width)
         ("<" . 'evil-window-decrease-width)

         ;; These aren't used
         ("p" . 'previous-buffer)
         ("n" . 'next-buffer)

         ;; ("c" . 'new-frame)
         ;; ("C" . 'delete-frame)
         ;; ("n" . 'other-frame)

         ;; Motion between windows
         ("h" . 'evil-window-left)
         ("j" . 'evil-window-down)
         ("k" . 'evil-window-up)
         ("l" . 'evil-window-right)
         ;; These aren't used
         ("J" . 'split-window-below)
         ("K" . 'split-window-above)
         ("L" . 'split-window-right)

         :map er/application-map
         ("w" . 'eww)
         ("s" . 'er/eshell-here)
         ("S" . 'shell)
         ("x" . 'term)
         ("p" . 'proced)
         ("m" . 'compile)
         )

  :config
  ;; For evil-collection
  (evil-mode 1)
  (setq evil-ex-substitute-global t)
  (setq evil-shift-width 2)
  (setq-default evil-symbol-word-search t)
  (setq evil-cross-lines t)

  ;; This doesn't break emacs state because <SPC>
  ;; is not a prefix key there
  ;; We can't simply use mode-specific-command-prefix because
  ;; e.g. org-mode bindings aren't directly propogated to it
  (define-key key-translation-map
    (kbd "<SPC> c") (kbd "C-c"))
  (define-key key-translation-map
    (kbd "C-<SPC> c") (kbd "C-c"))

  (evil-define-key 'motion widget-keymap
    "TAB" 'widget-forward
    "S-TAB" 'widget-backward
    "o" 'widget-field-activate
    "O" 'widget-button-press)

  ;; Allow common emacs keybindings to be always available
  ;; This does clobber some special vim behaviour
  (define-key evil-insert-state-map "\C-e" 'end-of-line)
  (define-key evil-motion-state-map "\C-e" 'evil-end-of-line)
  (define-key evil-motion-state-map "\C-f" 'evil-forward-char)
  (define-key evil-insert-state-map "\C-f" 'evil-forward-char)
  (define-key evil-motion-state-map "\C-b" 'evil-backward-char)
  (define-key evil-insert-state-map "\C-b" 'evil-backward-char)
  (define-key evil-motion-state-map "\C-d" 'evil-delete-char)
  (define-key evil-insert-state-map "\C-d" 'evil-delete-char)
  (define-key evil-motion-state-map "\C-n" 'evil-next-line)
  (define-key evil-insert-state-map "\C-n" 'evil-next-line)
  (define-key evil-motion-state-map "\C-p" 'evil-previous-line)
  (define-key evil-insert-state-map "\C-p" 'evil-previous-line)
  (define-key evil-motion-state-map "\C-w" 'evil-delete)
  (define-key evil-insert-state-map "\C-w" 'evil-delete)
  (define-key evil-motion-state-map "\C-y" 'yank)
  (define-key evil-insert-state-map "\C-y" 'yank)
  (define-key evil-motion-state-map "\C-k" 'kill-line)
  (define-key evil-insert-state-map "\C-k" 'kill-line)
  (define-key evil-normal-state-map "Q" 'call-last-kbd-macro)
  (define-key evil-visual-state-map "Q" 'call-last-kbd-macro)


  ;; ESC actually quits
  ;; From https://stackoverflow.com/questions/8483182/evil-mode-best-practice
  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
  In Delete Selection mode, if the mark is active, just deactivate it;
  then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

  (global-set-key (kbd "C-x ESC") 'keyboard-quit)

  (define-key evil-motion-state-map er/application-evil-prefix-key 'er/application-prefix)
  (define-key evil-motion-state-map er/leader-evil-prefix-key 'er/leader-prefix)

  ;; Folding
  (evil-define-key 'normal 'global (kbd "TAB") 'evil-toggle-fold)

  (defun er/hs-hide-1 ()
    "Hide-show 1 level."
    (hs-hide-level 1))

  (defun er/hs-toggle-1 ()
    "Hide-show show 1 level or hide."
    (if (hs-already-hidden-p)
        (hs-hide-level 1)
      (hs-hide-block)))

  (setq evil-fold-list
        (cons
         '((hs-minor-mode)
           :open-all   hs-show-all
           :close-all  hs-hide-all
           :toggle     er/hs-toggle-1
           :open       er/hs-hide-1
           :open-rec   hs-show-block
           :close      hs-hide-block)
         evil-fold-list))
  )

(use-package evil-collection
  :after evil
  :config
  ;; Don't let it steal org-mode tab bindings
  (setq evil-collection-outline-bind-tab-p nil)
  (evil-collection-init))

(use-package transpose-frame
  :bind
  (:map er/leader-map
   ("R" . 'transpose-frame)))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;;; Relative line numbers for easy movement in Evil mode
(if (>= emacs-major-version 26)
    ;; In newer emacs relative line numbers are is built in
    (progn
      (setq display-line-numbers-current-absolute nil)
      (setq display-line-numbers-type 'visual)
      (global-display-line-numbers-mode)
      (define-key er/application-map "l" display-line-numbers-mode))
  (use-package linum-relative
    :demand t
    :bind
    (:map er/application-map
          ("l" . 'linum-relative-mode))
    :config
    (linum-relative-global-mode)
    (defun disable-linum-relative () (linum-relative-mode -1))
    ;; Disable relative linum in modes with a lot of folding
    ;; because it gives useless numbers and takes a long time to calculate
    (add-hook 'org-mode-hook 'disable-linum-relative)
    (add-hook 'magit-mode-hook 'disable-linum-relative)))

(use-package evil-god-state
    :bind
    (:map evil-motion-state-map
          ("<RET>" . 'evil-execute-in-god-state)
     :map evil-god-state-map
          ([escape] . 'evil-god-state-bail)))


;; Workaround for Evil God mode in Evil Visual state
;; See https://github.com/gridaphobe/evil-god-state/issues/4
(defun evil-visual-activate-hook (&optional command)
  "Enable Visual state if the region is activated."
  (unless (evil-visual-state-p)
    (evil-delay nil
        ;; the activation may only be momentary, so re-check
        ;; in `post-command-hook' before entering Visual state
        '(unless (or (evil-visual-state-p)
                     (evil-insert-state-p)
                     (evil-emacs-state-p)
                     (evil-god-state-p))
           (when (and (region-active-p)
                      (not deactivate-mark))
             (evil-visual-state)))
      'post-command-hook nil t
      "evil-activate-visual-state")))

(use-package god-mode
    :bind (("C-<RET>" . god-local-mode)))


(use-package evil-exchange
  :config
  (evil-exchange-install))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help with interaction and interactivity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package smart-mode-line
  :disabled
  ;; Pending https://github.com/Malabarba/smart-mode-line/issues/198
  (setq sml/name-width 15)
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

;; Simplify modeline
;; Hide common minor modes
(use-package diminish
  :config
  (defun er/diminish-auto-revert () (diminish 'auto-revert-mode))
  (add-hook 'auto-revert-mode-hook 'er/diminish-auto-revert)
  (defun er/diminish-undo-tree () (diminish 'undo-tree-mode))
  (add-hook 'undo-tree-mode-hook 'er/diminish-undo-tree))

(use-package which-key
    ;:bind (("C-h ?" . which-key-show-top-level))
    :diminish which-key-mode
    :config
    (which-key-mode)
    (setq which-key-show-operator-state-maps 1)
    (which-key-enable-god-mode-support))

; M-x enhancement - more interactivity
;; Ivy and friends for interactive completion
(use-package smex
  ; M-x is rebound by counsel-mode to use smex
  :bind (("M-X" . smex-major-mode-commands)))


(use-package swiper
  :bind
  (:map er/leader-map
   ("/" . 'swiper)))

(use-package counsel
  :demand t
  :bind (:map evil-normal-state-map
         ("gp" . 'counsel-yank-pop)
         :map evil-insert-state-map
         ("C-S-r" . 'counsel-yank-pop))
  :diminish counsel-mode
  :config
  ; Ignore dotfiles by default
  (setq counsel-find-file-ignore-regexp "\\`\\.")
  (counsel-mode 1)
  )

(use-package ivy
  :demand t
  :diminish ivy-mode
  :bind (:map ivy-minibuffer-map
         ("<escape>" . minibuffer-keyboard-quit))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; Allow selection of text being entered
  ;; otherwise can only select with C-M-j
  (setq ivy-use-selectable-prompt t)
  (ivy-mode 1))

; TODO: Only load on extensions that can flycheck
; Note: Appropriate packages need to be available on the system
(use-package flycheck
  :bind (:map er/application-map
              ("kn" . flycheck-next-error)
              ("kp" . flycheck-previous-error)
              ("ks" . flycheck-select-checker)
              ("kx" . flycheck-disable-checker)
              ("ki" . flycheck-manual)
              ("kh" . flycheck-disable-error-at-point)
              ("kC" . flycheck-compile)
              ("kw" . flycheck-copy-errors-as-kill)
              ("k?" . flycheck-describe-checker)
              ("kl" . flycheck-list-errors)
              ("kd" . flycheck-clear)
              ("kH" . display-local-help)
              ("kV" . flycheck-version)
              ("kc" . flycheck-buffer)
              ("ke" . flycheck-set-checker-executable)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Applications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autocompletion: Company mode is better thought out than auto-complete.
;; Comparison with auto-complete: https://github.com/company-mode/company-mode/issues/68 for comparison
(use-package company
  :defer 2
  :config
  (global-company-mode)

  ;; In eshell typing " causes a tab to be inserted and an error
  ;; https://github.com/company-mode/company-mode/issues/409
  (defun er/eshell-remove-pcomplete ()
    (remove-hook 'completion-at-point-functions #'pcomplete-completions-at-point t))

  (add-hook 'eshell-mode-hook #'er/eshell-remove-pcomplete)

  ;; Configuration alias
  ;; ... should probably just keep this in the alias file
  (defun er/alias-eshell-config () (eshell/alias "config" "git --git-dir=$HOME/.cfg/ --work-tree=$HOME $*"))

  (add-hook 'eshell-mode-hook #'er/alias-eshell-config))


;; Usage:  fci-mode shows a line at the fill-column
;(use-package fill-column-indicator
;  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package org
  :ensure org-plus-contrib
  :bind (("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         :map org-mode-map
         ("C-c C-." . org-time-stamp-inactive)
         :map er/application-map
         ("a" . org-agenda-list)
         ("A" . org-agenda)
         ("c" . org-capture)
         ("t" . org-todo-list))
  :config
  (evil-define-key 'motion org-agenda-keymap
    "j" 'org-agenda-next-line
    "k" 'org-agenda-previous-line
    "x" 'org-agenda-capture)


  ;; Don't bury the agenda by default
  (setq org-agenda-sticky 't)
  ;; Restore agenda buffer instead of regenerating it
  ;; (e.g. reordering TODOs), this doesn't load new items.
  (setq org-agenda-restore-windows-after-quit 't)


  ;; Refiling
  (setq org-agenda-files '("~/org/projects.org"))
  (setq org-default-notes-file "~/org/inbox.org")
  ;; Allow refiling accross files
  (setq org-refile-use-outline-path 'file)
  ;; Fill in the whole refile path at once
  ;; This is required for use with ido
  (setq org-outline-path-complete-in-steps nil)
  ;; Allow creating top level nodes
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-targets
        '(
          ("~/org/someday.org" :level . 1)
          ("~/org/reference.org" :level . 1)
          ("~/org/projects.org" :level . 1)
          ("~/org/reference.org" :maxlevel . 3)))
  ;; Capture
  (setq org-capture-templates
        '(
          ("n" "Note" entry
           (file "~/org/inbox.org")
           "* %? \n%u")
          ("c" "Capture" entry
           (file "~/org/inbox.org")
           "* %A\n%u")
          ("y" "Yank note from selection" entry
           (file "~/org/inbox.org")
           "* %?\n%u\n%i")
          ("p" "Paste note from kill ring" entry
           (file "~/org/inbox.org")
           "* %?\n%u\n%^C")
          ("i" "Interruption" entry
           (file "~/org/inbox.org")
           "* %?\n%u\n" :clock-in t :clock-resume t))
        )

  ;; Requires module org-id
  ;; Automatically create unique identifiers for cross links
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  (setq org-export-backends '(ascii html icalendar latex md odt org))
  (setq org-modules '(org-id org-bbdb org-bibtex org-docview org-eww org-gnus org-info org-irc org-mhe org-protocol org-w3m org-eshell org-git-link org-man org-notmuch org-registry))

  (evil-set-initial-state 'org-agenda-mode 'motion)
  ;; Open org agenda in the current window, not destroying layout
  (setq org-agenda-window-setup 'current-window)
  )


(use-package dumb-jump
  :bind
  (:map evil-motion-state-map
        ("C-]" . 'dumb-jump-go)
        ("C-}" . 'dumb-jump-go-prompt))
  :config
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-force-searcher 'rg)
  :ensure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming Language Support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Elisp
;; https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned
;; https://github.com/Fuco1/.emacs.d/blob/af82072196564fa57726bdbabf97f1d35c43b7f7/site-lisp/redef.el#L20-L94
(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq-local lisp-indent-function #'Fuco1/lisp-indent-function)))
(defun Fuco1/lisp-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
  (let ((normal-indent (current-column))
        (orig-point (point)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond
     ;; car of form doesn't seem to be a symbol, or is a keyword
     ((and (elt state 2)
           (or (not (looking-at "\\sw\\|\\s_"))
               (looking-at ":")))
      (if (not (> (save-excursion (forward-line 1) (point))
                  calculate-lisp-indent-last-sexp))
          (progn (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point)
                                     calculate-lisp-indent-last-sexp 0 t)))
      ;; Indent under the list or under the first sexp on the same
      ;; line as calculate-lisp-indent-last-sexp.  Note that first
      ;; thing on that line has to be complete sexp since we are
      ;; inside the innermost containing sexp.
      (backward-prefix-chars)
      (current-column))
     ((and (save-excursion
             (goto-char indent-point)
             (skip-syntax-forward " ")
             (not (looking-at ":")))
           (save-excursion
             (goto-char orig-point)
             (looking-at ":")))
      (save-excursion
        (goto-char (+ 2 (elt state 1)))
        (current-column)))
     (t
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state))))))))


;; YAML
(use-package yaml-mode
  :mode "\\.ya?ml\\'")


;;; Version Control - except in Cygwin where it's dog slow
(if (not (eq system-type 'cygwin))
    (use-package evil-magit
      :bind (:map er/application-map
                  ("g" . magit-status))))



;;; Python

(defun er/elpy-shell-send-region-or-statement-and-step (&optional arg)
  (interactive "P")
  (if (use-region-p)
      (elpy-shell-send-region-or-buffer-and-step)
    (elpy-shell-send-statement-and-step)))

;; Required packages: jedi importmagic autopep8 yapf flake8 pylint
;;                    jupyter ipdb
(use-package elpy
  :mode ("\\.py\\'" . python-mode)
  :config
  ;; Make more consistent with ESS
  (bind-key "C-c C-c" 'er/elpy-shell-send-region-or-statement-and-step elpy-mode-map)
  ;; Use flycheck instead of flymake
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'hs-minor-mode)
  (with-eval-after-load 'python (elpy-enable))

  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt")



  ;; Get virtualenv working in eshell
  ;; Sourced from https://www.reddit.com/r/emacs/comments/5z0w5t/bash_dependency_eshell/
  (with-eval-after-load 'eshell
    (defvar eshell-path-env)
    (dolist (hook '(pyvenv-post-activate-hooks pyvenv-post-deactivate-hooks))
      (add-hook hook                  ; eshell
                (lambda ()
                  (let ((path-env (mapconcat (lambda (x) (or x "."))
                                             exec-path
                                             path-separator)))
                    (setq-default eshell-path-env path-env)
                    (dolist (buffer (buffer-list))
                      (with-current-buffer buffer
                        (and (derived-mode-p 'eshell-mode)
                             (setq eshell-path-env path-env)))))))))
  )

(use-package py-autopep8
  :mode "\\.py\\'")

(use-package ess
  :commands R
  :mode ("\\.R\\'" . R-mode)
  :init (require 'ess-site)
  :config
  ;; There's a bug with flycheck lintr caching
  (setq flycheck-lintr-caching nil)
  ;; ESS will not print the evaluated commands, also speeds up the evaluation
  (setq ess-eval-visibly nil)
  ;; Don't prompt for directory; use cwd
  (setq ess-ask-for-ess-directory nil)
  ;; Smart S assign is more annoying than helpful
  ;; Can't invoke it directly for some reason
  (ess-toggle-underscore nil)
  ;; So that seemed to break in 17.11
  ;; Instead we just get it to replace _ with _
  (setq ess-S-assign "_")
  ;; Need to put this in a hook or else ESS won't respect it
  (defun my-ess-settings ()
    ;; ESS by default has a crazy indentation scheme where comments are indented
    ;; based on the number of # signs, inherited from lisp.
    ;; See https://stackoverflow.com/questions/780796/emacs-ess-mode-tabbing-for-comment-region
    ;; Turn this off
    (setq ess-fancy-comments nil)
    )
  (add-hook 'ess-mode-hook #'my-ess-settings))

(use-package polymode
  :mode
  ("\\.Rmd" . poly-markdown+r-mode))
(use-package poly-markdown
  :mode "\\.Rmd")

(use-package poly-R
  :mode "\\.Rmd")

; Open mmark files as markdown
(use-package markdown-mode
  :mode
  ("\\.md" . markdown-mode)
  ("\\.mmark" . markdown-mode))

;; Spelling
;; Requires aspell to be installed
;; Location of the Aspell binary for Windows (http://aspell.net/win32/)

(use-package ispell
  :commands ispell
  :bind (:map er/application-map ("i" . ispell))
  :config
  (defvar er/windows-aspell-path "C:/Program Files (x86)/Aspell/bin/")
  (if (file-exists-p er/windows-aspell-path)
      (add-to-list 'exec-path er/windows-aspell-path))
  ;; If we don't have Aussie English...
  (setq ispell-dictionary "british")
  ;; aspell is more accurate than hunspell or ispell, although a little slower
  (setq ispell-program-name "aspell")
  ;; Location of personal dictionary for customisations
  ;; The default doesn't seem to work on windows
  (setq ispell-personal-dictionary "~/.aspell.en.pws")
  ;; Save on insertion without confirmation
  (setq ispell-silently-savep 't)
  )


(defun er/scroll-right (count)
  (interactive "p")
  (right-char (* count (/ (window-width) 2))))

(defun er/scroll-left (count)
  (interactive "p")
  (left-char (* count (/ (window-width) 2))))

(use-package hydra
  :config
    (defhydra hydra-zoom (evil-normal-state-map "z")
    "zoom"
    ("=" text-scale-increase "in")
    ("-" text-scale-decrease "out")
    ("l" er/scroll-right "right")
    ("h" er/scroll-left "left")
    ("j" evil-scroll-down "down")
    ("k" evil-scroll-up "up")))

;; TODO: Buffer local?
;; TODO: ESC escapes?
(use-package caps-lock
  :bind (("C-c u" . 'caps-lock-mode)
  :map evil-normal-state-map ("zu"  . 'caps-lock-mode)))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(use-package pdf-tools
  :load-path "site-lisp/pdf-tools/lisp"
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))


(defun point-backover-newline (point)
  "If the previous point is a newline go back one character"
  (if (= (char-before point) ?\n)
      (- point 1)
    point))


(defun sql-format (start end)
  "Formats the selected sql using `sqlformat'"
  (interactive "r")
  (shell-command-on-region
   start
   ;; Exclude overwriting final newline
   (point-backover-newline end)
   "sqlformat -k upper -r -s -"
   ;; output buffer
   (current-buffer)
   ;; replace?
   t
   ;; name of the error buffer
   "*Sqlformat Error Buffer*"
   ;; show error buffer?
   t))

;; Put/save customisations through customize in a separate file
(defconst custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)



;; Make gc pauses faster by decreasing the threshold.
;; https://blog.d46.us/advanced-emacs-startup/
(setq gc-cons-threshold (* 2 1000 1000))

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))
