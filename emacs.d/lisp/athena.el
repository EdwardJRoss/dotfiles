;;; athena.el -- A command interpreter mode for athena CLI

;; Copyright (C) 2018 Edward Ross

;; Author: Edward Ross <edward@skeptric.com>
;; Maintainer: Edward Ross <edward@skeptric.com>
;; Created: 1 Aug 2018
;; Modified: 30 Aug 2018
;; Version: 0.1
;; Package-Requires:
;; Keywords: athena
;; URL:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'comint)

;; TODO: Try to pass this directly to the process
;; with-env
(setenv "ATHENA_CLI_PAGER" "head -n 1000")


(defvar athena-cli-file-path "athena"
  "Path to the program used by `run-athena'.")

(defvar athena-cli-arguments '()
  "Commandline arguments to pass to `athena-cli'.

   Example '(\"--bucket\" \"s3://bucket/key\"
             \"--region\" \"ap-southeast-2\"
             \"--db\"     \"database-name\"
 ")


;; From https://athenadb.io/docs/current/language/reserved.html
(defconst athena-keywords
  '("alter" "and" "as" "between" "by" "case" "cast" "constraint"
  "create" "cross" "cube" "current_date" "current_path" "current_time"
  "current_timestamp" "current_user" "deallocate" "delete" "describe"
  "distinct" "drop" "else" "end" "escape" "except" "execute" "exists"
  "extract" "false" "for" "from" "full" "group" "grouping" "having"
  "in" "inner" "insert" "intersect" "into" "is" "join" "left" "like"
  "localtime" "localtimestamp" "natural" "normalize" "not" "null" "on"
  "or" "order" "outer" "prepare" "recursive" "right" "rollup" "select"
  "table" "then" "true" "uescape" "union" "unnest" "using" "values"
  "when" "where" "with"
  ;; words I had to add
  "limit" "show" "regexp_like"))

(defvar athena-font-lock-keywords
  (list
   ;; highlight all the reserved commands.
   `(,(concat "\\_<" (regexp-opt athena-keywords) "\\_>") . font-lock-keyword-face))
  "Additional expressions to highlight in `athena-mode'.")


(defvar athena-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `run-athena'.")

(defvar athena-prompt-regexp "^\\([^>]+>\\)"
  "Prompt for `run-athena'.")

(defun run-athena ()
  "Run an inferior instance of `athena-cli' inside Emacs."
  (interactive)
  (let* ((athena-program athena-cli-file-path)
         (buffer (comint-check-proc "Athena")))
    ;; pop to the "*Athena*" buffer if the process is dead, the
    ;; buffer is missing or it's got the wrong mode.
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'athena-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer "*Athena*"))
       (current-buffer)))
    ;; create the comint process if there is no buffer.
    (unless buffer
      (apply 'make-comint-in-buffer "Athena" buffer
             athena-program '() athena-cli-arguments)
      (athena-mode))))

(defun athena--initialize ()
  "Helper function to initialize Athena."
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))

(define-derived-mode athena-mode comint-mode "Athena"
  "Major mode for `run-athena'.

\\<athena-mode-map>"
  nil "Athena"
  ;; this sets up the prompt so it matches things like: [foo@bar]
  (setq comint-prompt-regexp athena-prompt-regexp)
  ;; this makes it read only; a contentious subject as some prefer the
  ;; buffer to be overwritable.
  (setq comint-prompt-read-only t)
  ;; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'font-lock-defaults) '(athena-font-lock-keywords t))
  (set (make-local-variable 'paragraph-start) athena-prompt-regexp))

;; this has to be done in a hook. grumble grumble.
(add-hook 'athena-mode-hook 'athena--initialize)



;; Filter out ANSI Control Sequences
;; Stolen from https://oleksandrmanzyuk.wordpress.com/2011/11/05/better-emacs-shell-part-i/
(defun regexp-alternatives (regexps)
  "Return the alternation of a list of regexps."
  (mapconcat (lambda (regexp)
               (concat "\\(?:" regexp "\\)"))
             regexps "\\|"))

(defvar non-sgr-control-sequence-regexp nil
  "Regexp that matches non-SGR control sequences.")

(setq non-sgr-control-sequence-regexp
      (regexp-alternatives
       '(;; icon name escape sequences
         "\033\\][0-2];.*?\007"
         ;; non-SGR CSI escape sequences
         "\033\\[\\??[0-9;]*[^0-9;m]"
         ;; noop
         "\012\033\\[2K\033\\[1F"
         )))

(defun filter-non-sgr-control-sequences-in-region (begin end)
  (save-excursion
    (goto-char begin)
    (while (re-search-forward
            non-sgr-control-sequence-regexp end t)
      (replace-match ""))))

(defun filter-non-sgr-control-sequences-in-output (ignored)
  (let ((start-marker
         (or comint-last-output-start
             (point-min-marker)))
        (end-marker
         (process-mark
          (get-buffer-process (current-buffer)))))
    (filter-non-sgr-control-sequences-in-region
     start-marker
     end-marker)))

(add-hook 'comint-output-filter-functions
          'filter-non-sgr-control-sequences-in-output)

;; Sending to output

(defun er/paragraph-extents ()
  "Return a cons cell with beginning and end of paragraph."
  (save-excursion
    (forward-paragraph)
    (let ((end (point)))
      (backward-paragraph)
      (cons (point) end))))


;; TODO: Add this to suitable shortcut
(defun athena-send ()
  "Send the current region or paragraph to Athena process."
  (interactive)
  (let ((beg-end (if (use-region-p)
                    (cons (region-beginning) (region-end))
                    (er/paragraph-extents))))
    ;; TODO: Configure destination as buffer-local-variable
    (comint-send-region "*Athena*" (car beg-end) (cdr beg-end))))



;; classpath is not a parameter
;; Do we need to not pass it somehow?

(provide 'athena)

;;; athena.el ends here
