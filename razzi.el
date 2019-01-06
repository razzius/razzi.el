;;; razzi.el --- Razzi's elisp functions -*- lexical-binding: t -*-
;;
;; Copyright Razzi Abuissa
;;
;; Author: Razzi Abuissa <razzi53@gmail.com>
;; URL: https://github.com/razzius/razzi.el
;; Version: 0.1.0
;; Keywords:
;; Package-Requires:

;; This file is not part of GNU Emacs.

;;; Commentary:

;; elisp functions suitable for interactive use and init.el configuration.

;;; Code:

(defgroup razzi nil
  "razzi configuration though there isn't any yet."
  :prefix "razzi-"
  :group 'convenience)

;;;###autoload
(defun razzi-transpose-previous-chars ()
  "Transpose the 2 characters before point."
  (interactive)
  (backward-char 1)
  (transpose-chars nil))

;;;###autoload
(defun razzi-insert-newline-after()
  (interactive)
  (save-excursion
    (evil-insert-newline-below)
    (forward-line -1)))

;;;###autoload
(defun razzi-insert-newline-before()
  (interactive)
  (save-excursion
    (evil-insert-newline-above)
    (forward-line)))

;;;###autoload
(defun razzi/close-all-file-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;;;###autoload
(defun razzi/put-after ()
  (interactive)
  (evil-with-single-undo
    (evil-insert-newline-below)
    (indent-for-tab-command)
    (insert (s-trim (current-kill 0)))
    (forward-line)))

;;;###autoload
(defun razzi-associate-extension-mode (extension mode)
  (let ((pattern (s-concat "\\." extension "$")))
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(provide 'razzi)
;;; razzi.el ends here
