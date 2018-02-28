;; -*-Lisp-*-
;;; init.el --- Configuration for Edward Ross

;; Author: Edward Ross <edward@skeptric.com>

;;; Commentary:

;; Personal configuration file for Vim like Emacs

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General configuration independent of packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
(defconst application-evil-prefix "-")
(defconst application-global-prefix "<f6>")


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil - Vi Emulation Layer for Emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; General makes it nicer to define keybindings
(use-package general
  :config
  ;; See https://github.com/noctuid/general.el/issues/97
  (general-auto-unbind-keys))

(use-package evil
  :ensure t
  :demand t
  :after general
  :general
  (:states '(normal visual motion)
  ; M acts like zz,zb,zt
  "M" 'recenter-top-bottom)
  (:states 'motion
   :keymaps 'widget-keymap
   "TAB" 'widget-forward
   "S-TAB" 'widget-backward
   "o" 'widget-field-activate
   "O" 'widget-button-press
   )

  ;; I find this more useful than undo-line
  (:states 'normal
  "U" 'undo-tree-redo)

  ;; ESC always escapes
  ;(:states 'emacs
  ; [escape] 'keyboard-quit)
  (:keymaps '(minibuffer-local-map
              minibuffer-local-ns-map
              minibuffer-local-completion-map
              minibuffer-local-must-match-map
              minibuffer-local-isearch-map)
  [escape] 'abort-recursive-edit)
  ("C-x ESC" 'keyboard-quit)

  ;; Leader
  (:states '(normal insert motion emacs visual operator)
   :prefix "<SPC>"
   :global-prefix "C-<SPC>"

   "<SPC>" 'evil-execute-in-god-state
   ";" 'evil-ex

   "a" (eval `(general-simulate-key ,application-global-prefix))

   "c" (general-simulate-key "C-c")
   "x" (general-simulate-key "C-x")
   "u" 'universal-argument

   "!" 'term

   "d" 'kill-this-buffer
   "D" 'kill-buffer
   "f" 'find-file
   "w" 'save-buffer
   "b" 'switch-to-buffer
   "B" 'list-buffers
   "q" 'delete-window
   "Q" 'kill-buffer-and-window

   "s" 'evil-window-split
   "v" 'evil-window-vsplit
   "o" 'delete-other-windows
   "r" 'evil-window-rotate-downwards

   "=" 'balance-windows
   "+" 'evil-window-increase-height
   "-" 'evil-window-decrease-height
   ">" 'evil-window-increase-width
   "<" 'evil-window-decrease-width

   ;; These aren't used
   "p" 'previous-buffer
   "n" 'next-buffer

   ;; "c" 'new-frame
   ;; "C" 'delete-frame
   ;; "n" 'other-frame

   ;; Motion between windows
   "h" 'evil-window-left
   "j" 'evil-window-down
   "k" 'evil-window-up
   "l" 'evil-window-right
   ;; These aren't used
   "J" 'split-window-below
   "K" 'split-window-above
   "L" 'split-window-right

   )

  (:states '(normal insert motion emacs visual)
   :prefix application-evil-prefix
   :global-prefix application-global-prefix
    "w" 'eww
    "s" 'eshell
    "S" 'shell
    "x" 'term
    "p" 'proced
    "m" 'compile)

  :init
  (setq evil-want-integration nil)
   :config
   ;; For evil-collection
  (evil-mode 1)
  (setq evil-ex-substitute-global t)
  (setq evil-shift-width 2)
  (setq-default evil-symbol-word-search t)
  (setq evil-cross-lines t)
  )

(use-package evil-collection
  :after evil
  :ensure t
  :config
  ;; Don't let it steal org-mode tab bindings
  (setq evil-collection-outline-bind-tab-p nil)
  (evil-collection-init))

(use-package transpose-frame
  :general
  (:states '(normal insert motion emacs visual operator)
   :prefix "<SPC>"
   :global-prefix "C-<SPC>"
   "R" 'transpose-frame))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; Relative line numbers for easy movement in Evil mode
(use-package linum-relative
  :ensure t
  :demand t
  :config
  (linum-relative-global-mode)
  (defun disable-linum-relative () (linum-relative-mode -1))
  ;; Disable relative linum in modes with a lot of folding
  ;; because it gives useless numbers and takes a long time to calculate
  (add-hook 'org-mode-hook 'disable-linum-relative)
  (add-hook 'magit-mode-hook 'disable-linum-relative)
  :general
  (:states '(normal insert motion emacs visual)
   :prefix application-evil-prefix
   :global-prefix application-global-prefix
   "l" 'linum-relative-mode))



(use-package evil-god-state
    :ensure t
    :demand t
    :general
    (:states '(normal motion visual)
     "<RET>" 'evil-execute-in-god-state)
    (:states 'god
     [escape] 'evil-god-state-bail))


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
    :ensure t
    :bind (("C-<RET>" . god-local-mode)))


(use-package evil-exchange
  :demand t
  :config
  (evil-exchange-install))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help with interaction and interactivity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package smart-mode-line
  :config
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
    :demand t
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
  :general
  (:states '(normal insert motion emacs visual operator)
   :prefix "<SPC>"
   :global-prefix "C-<SPC>"
   "/" 'swiper))

(use-package counsel
  :demand t
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
  (ivy-mode 1))

; TODO: Only load on extensions that can flycheck
; Note: Appropriate packages need to be available on the system
(use-package flycheck
  :demand t
  :config
  (global-flycheck-mode)

  :general
  ;; TODO: Replace these with Vim unimpared
  ;; https://github.com/tpope/vim-unimpaired
  (:states '(normal insert motion emacs visual)
   :prefix application-evil-prefix
   :global-prefix application-global-prefix
   "kn" 'flycheck-next-error
   "kp" 'flycheck-previous-error
   "ks" 'flycheck-select-checker
   "kx" 'flycheck-disable-checker
   "ki" 'flycheck-manual
   "kh" 'flycheck-disable-error-at-point
   "kC" 'flycheck-compile
   "kw" 'flycheck-copy-errors-as-kill
   "k?" 'flycheck-describe-checker
   "kl" 'flycheck-list-errors
   "kd" 'flycheck-clear
   "kH" 'display-local-help
   "kV" 'flycheck-version
   "kc" 'flycheck-buffer
   "ke" 'flycheck-set-checker-executable
   )

  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Applications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autocompletion: Company mode is better thought out than auto-complete.
;; Comparison with auto-complete: https://github.com/company-mode/company-mode/issues/68 for comparison
(use-package company
  :ensure t
  :demand t
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
  :bind (
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)

         )
  :config
  (define-key org-mode-map (kbd "C-c C-.") 'org-time-stamp-inactive)

  ;; Refiling
  (setq org-agenda-files '("~/org/projects.org"))
  (setq org-default-notes-file "~/org/inbox.org")
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-targets
        '(
          ("~/org/someday.org" :level . 1)
          ("~/org/reference.org" :level . 1)
          ("~/org/projects.org" :maxlevel . 3)
          ("~/org/reference.org" :level . 1)))
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
  (setq org-export-backends '(ascii html icalendar latex md odt org))
  (setq org-modules '(org-bbdb org-bibtex org-docview org-eww org-gnus org-info org-irc org-mhe org-protocol org-w3m org-eshell org-git-link org-man org-notmuch org-registry))

  (evil-set-initial-state 'org-agenda-mode 'motion)
  ;; Open org agenda in the current window, not destroying layout
  (setq org-agenda-window-setup 'current-window)
  :general
  (:states '(normal insert motion emacs visual)
   :prefix application-evil-prefix
   :global-prefix application-global-prefix
   "a" 'org-agenda-list
   "A" 'org-agenda
   "c" 'org-capture
   "t" 'org-todo-list
   )
  (:keymaps 'org-agenda-keymap
   :states 'motion
   "j" 'org-agenda-next-line
   "k" 'org-agenda-previous-line
   "x" 'org-agenda-capture
   )

  )

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

;;; Version Control
(if (not (eq system-type 'cygwin))
(use-package evil-magit
  :demand
  :after evil
  :general
  (:states '(normal insert motion emacs visual)
   :prefix application-evil-prefix
   :global-prefix application-global-prefix
   "g" 'magit-status)
  )
)


;;; Python

;; Required packages: jedi importmagic autopep8 yapf flake8 pylint
;;                    jupyter ipdb
(use-package elpy
  :config
  ;; Use flycheck instead of flymake
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode)
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
  :config
  ;; Conform to PEP-8 on Save
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

(use-package ess
  :commands R
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
  (ess-toggle-underscore nil))

(use-package polymode
  :mode
  ("\\.Snw" . poly-noweb+r-mode)
  ("\\.Rnw" . poly-noweb+r-mode)
  ("\\.Rmd" . poly-markdown+r-mode))

(use-package csv-mode)

(use-package groovy-mode
  :mode "\\.groovy\\'")

(use-package geiser)

;; Spelling
;; Requires aspell to be installed
;; Location of the Aspell binary for Windows (http://aspell.net/win32/)
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
(require 'ispell)

;; Email
(use-package notmuch
  :ensure nil
  :config
  ;; Set the default text renderer to something that
  ;; can handle most html emails
  (setq mm-text-html-renderer 'gnus-w3m)


  (setq-default notmuch-archive-tags '("-inbox" "-deleted" "+archive"))
  (setq-default er/notmuch-delete-tags '("-inbox" "+deleted" "-archive"))

  (evil-set-initial-state 'notmuch-hello-mode 'motion)
  (evil-set-initial-state 'notmuch-search-mode 'motion)
  (evil-set-initial-state 'notmuch-show-mode 'motion)
  ;(evil-set-initial-state 'notmuch-tree-mode 'motion)
  ;(evil-set-initial-state 'notmuch-message-mode 'normal)

  (defun er/notmuch-inbox ()
      "Jump to notmuch tag inbox"
    (interactive)
    (notmuch-search "tag:inbox"))


  (defun er/notmuch-search-delete-thread (&optional unarchive beg end)
    "Mark selected threads as to delete"
    (interactive (cons current-prefix-arg (notmuch-search-interactive-region)))
    ;; Abuse emacs scope bindings
    (let ((notmuch-archive-tags er/notmuch-delete-tags))
      (notmuch-search-archive-thread unarchive beg end)))

  (defun er/notmuch-show-next-or-next-thread ()
    (interactive)
    (unless (notmuch-show-next-open-message)
      (notmuch-show-next-thread-show)))

  (defun er/notmuch-show-previous-or-previous-thread ()
    (interactive)
    (unless (notmuch-show-previous-open-message)
      (notmuch-show-previous-thread-show)))

  (defun er/notmuch-show-delete-message-then-next-or-next-thread ()
    "Mark current message as delete, then show the next open message in the current thread"
    (interactive)
    (let ((notmuch-archive-tags er/notmuch-delete-tags))
      (notmuch-show-archive-message-then-next-or-next-thread)))

  (defun er/notmuch-show-delete-thread-then-next ()
    (interactive)
    (let ((notmuch-archive-tags er/notmuch-delete-tags))
      (notmuch-show-archive-thread-then-next)))

  (setq message-kill-buffer-on-exit t)
  :general
  (:states '(normal insert motion emacs visual)
   :prefix application-evil-prefix
   :global-prefix application-global-prefix
   "e" 'er/notmuch-inbox
   "/" 'notmuch-search
   "n" 'notmuch-mua-new-mail
   )

  (:keymaps 'notmuch-common-keymap
   :states 'motion
   "r" 'notmuch-refresh-this-buffer
   "R" 'notmuch-poll-and-refresh-this-buffer
   "s" 'notmuch-search
   "S" 'notmuch-tree
   ;"gs" 'notmuch-jump
   ;"gS" 'notmuch-jump-search
   )

  (:keymaps 'notmuch-hello-mode-map
   :states 'motion
   "i" 'evil-insert
   "a" 'evil-append
   "I" 'evil-insert-line
   "a" 'evil-append-line
   )

  (:keymaps 'notmuch-search-mode-map
   :states 'motion
   "o" 'notmuch-search-show-thread

   "p" 'notmuch-search-previous-thread

   ;; For some reason the search-mode bindings shadow show-mode
   ;; so use capitals to avoid clashes
   ;; TODO fixme
   "D" 'er/notmuch-search-delete-thread
   "A" 'notmuch-search-archive-thread

   "u" 'notmuch-search-add-tag
   "gu" 'notmuch-search-tag-all
   "U" 'notmuch-search-remove-tag

   "i" 'notmuch-search-reply-to-thread-sender
   "I" 'notmuch-search-reply-to-thread

   "c" 'notmuch-tag-jump

   ;"x" 'notmuch-search-archive-thread

   ;notmuch-search-order
  )

  (:keymaps 'notmuch-show-mode-map
   :states 'motion
   ;"x" 'notmuch-show-archive-message-then-next-or-next-thread
   ;"x" 'notmuch-show-archive-thread-then-next

   "d" 'er/notmuch-show-delete-thread-then-next

   "a" 'notmuch-show-archive-message-then-next-or-next-thread
   "p" 'notmuch-show-previous-thread-show
   ;;"gj" 'notmuch-show-next-open-message
   "gj" 'er/notmuch-show-next-or-next-thread

   ;;"gk" 'notmuch-show-previous-open-message
   "gk" 'er/notmuch-show-previous-or-previous-thread

   "i" 'notmuch-show-reply-sender
   "I" 'notmuch-show-reply
   "gI" 'notmuch-show-forward-open-messages
   "gi" 'notmuch-show-forward-message

   "gr" 'notmuch-show-view-raw-message
   "gR" 'notmuch-show-toggle-visibility-headers

   "u" 'notmuch-show-add-tag
   "gu" 'notmuch-show-tag-all
   "U" 'notmuch-show-remove-tag

   "c" 'notmuch-tag-jump
   "C" 'notmuch-show-stash-map

   ;"gJ" 'notmuch-tree-from-show-current-query

   "o" 'notmuch-show-toggle-message
   "O" 'notmuch-show-save-attachments
   ;;todo follow link

   ;;l notmuch-show-filter-thread
   ;;t toggle-truncate-lines
   )

  )



(use-package hydra
  :config
    (defhydra hydra-zoom (evil-normal-state-map "z")
    "zoom"
    ("=" text-scale-increase "in")
    ("-" text-scale-decrease "out")
    ("l" evil-scroll-right "right")
    ("h" evil-scroll-left "left")
    ("j" evil-scroll-down "down")
    ("k" evil-scroll-up "up")))

;; TODO: Buffer local?
;; TODO: ESC escapes?
(use-package caps-lock
  :config
  (global-set-key (kbd "C-c u") 'caps-lock-mode)
  (evil-define-key 'normal 'global "zu" 'caps-lock-mode))


;; TODO: Version snippets
;; TODO: Local snippets
(use-package yasnippet
  :config
  (defun yas--downcase-key (args) ; args = (table key)
    (cl-callf downcase (nth 1 args))
    args)

  ;; Make snippet keys case insensitive
  (advice-add 'yas--fetch :filter-args #'yas--downcase-key)

  (yas-global-mode 1))


;; Put/save customisations through customize in a separate file
(defconst custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)
