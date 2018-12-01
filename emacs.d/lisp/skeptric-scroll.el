;;; skeptric-scroll --- Scrolling windows including in DocView (PDF) mode

;;; Commentary:
;; Scroll this or another window, even when one is in DocView mode.

;;; Code:
(require 'doc-view)

(defun next-window-major-mode ()
  "Major mode of current next window."
  (with-current-buffer (window-buffer (next-window)) major-mode))

(defun window-doc-view-p ()
  "Check whether window is in DocView (PDF) mode."
  (eq 'doc-view-mode
      (with-current-buffer (window-buffer) major-mode)))

(defun scroll-up-window (&optional count)
  "Scroll current window COUNT lines upwards (default whole screen)."
  (interactive "^P")
  (if (window-doc-view-p)
      (doc-view-scroll-up-or-next-page count)
    (scroll-up-command count))
  (redraw-frame))

(defun scroll-down-window (&optional count)
  "Scroll current window COUNT lines downwards (default whole screen)."
  (interactive "^P")
  (if (window-doc-view-p)
      (doc-view-scroll-down-or-previous-page count)
    (scroll-down-command count))
  (redraw-frame))

(defun scroll-up-other-window (&optional count)
  "Scroll next window COUNT lines upwards (default whole screen)."
  (interactive "^P")
  (with-selected-window (next-window)
    (scroll-up-window count)))

(defun scroll-down-other-window (&optional count)
  "Scroll next window COUNT lines downwards (default whole screen)."
  (interactive "^P")
  (with-selected-window (next-window)
    (scroll-down-window count)))

(provide 'skeptric-scroll)

;;; skeptric-scroll.el ends here
