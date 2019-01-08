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
(defun razzi-associate-extension-mode (extension mode)
  (let ((pattern (s-concat "\\." extension "$")))
    (add-to-list 'auto-mode-alist (cons pattern mode))))

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
(defun razzi-close-all-file-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;;;###autoload
(defun razzi-put-after ()
  (interactive)
  (evil-with-single-undo
    (evil-insert-newline-below)
    (indent-for-tab-command)
    (insert (s-trim (current-kill 0)))
    (forward-line)))

;;;###autoload
(defun razzi-exit-insert-and-save ()
  (interactive)
  (evil-normal-state)
  (save-buffer))

;;;###autoload
(defun razzi-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;;;###autoload
(defun razzi-put-after ()
  (interactive)
  (evil-with-single-undo
    (evil-insert-newline-below)
    (indent-for-tab-command)
    (insert (s-trim (current-kill 0)))
    (forward-line)))

;;;###autoload
(defun razzi-put-before ()
  (interactive)
  (evil-with-single-undo
    (evil-insert-newline-above)
    (indent-for-tab-command)
    (insert (s-trim (current-kill 0)))
    (forward-line)))

;;;###autoload
(defun razzi-previous-useful-buffer ()
  (interactive)
  (switch-to-buffer (nth 2 (seq-filter #'buffer-file-name (buffer-list)))))

;;;###autoload
(defun razzi-abbrev-or-add-global-abbrev ()
  (interactive)
  (if (abbrev-expansion (thing-at-point 'word))
      (progn
        (expand-abbrev)
        (message "Expanded"))
    (inverse-add-global-abbrev 1)))

;;;###autoload
(defun razzi-update-current-package ()
  (interactive)
  (straight-check-package (razzi-guess-current-package-prefix)))

;;;###autoload
(defun razzi-kill-line-and-whitespace ()
  (interactive)
  (sp-kill-hybrid-sexp nil)
  (delete-trailing-whitespace))

(defun razzi-guess-current-package-prefix ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^(defgroup \\(\\w+\\)" nil t)
    (match-string 1)))

;;;###autoload
(defun razzi-change-line ()
  "Make vim C use paredit-kill"
  (interactive)
  (sp-kill-hybrid-sexp nil)
  (evil-insert 0))

;;;###autoload
(defun razzi-expand-line ()
  (interactive)
  (let ((hippie-expand-try-functions-list
         '(try-expand-line try-expand-line-all-buffers))
    (call-interactively 'hippie-expand))))

;;;###autoload
(defun razzi-surround-with-single-quotes (start end)
  (interactive "r")
  (evil-surround-region start end nil ?'))

;;;###autoload
(defun razzi-surround-with-single-quotes (start end)
  (interactive "r")
  (evil-surround-region start end nil ?'))

;;;###autoload
(defun razzi-surround-with-backticks (start end)
  (interactive "r")
  (evil-surround-region start end nil ?`))

;;;###autoload
(defun razzi-surround-with-double-quotes (start end)
  (interactive "r")
  (evil-surround-region start end nil ?\"))

;;;###autoload
(defun razzi-surround-with-parens (start end)
  (interactive "r")
  (evil-surround-region start end nil ?\))
  (goto-char (+ 1 end)))

;;;###autoload
(defun razzi-surround-with-brackets (start end)
  (interactive "r")
  (evil-surround-region start end nil ?\])
  (goto-char (+ 1 end)))

;;;###autoload
(defun razzi-surround-with-curly-braces (start end)
  (interactive "r")
  (evil-surround-region start end nil ?})
  (goto-char (+ 1 end)))

;;;###autoload
(defun razzi-transpose-next-line ()
  "Switch the current and next lines"
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

;;;###autoload
(defun razzi-transpose-previous-line (arg)
  "Switch the current and previous lines"
  (interactive "P")
  (let ((count (or arg 1))
        (unused))
    (dotimes (number count unused)
      (progn
        (transpose-lines 1)
        (forward-line -2)))))

;;;###autoload
(defun razzi-append-comma ()
  (interactive)
  (evil-append 0 0 nil)
  (move-end-of-line nil)
  (insert ",")
  (evil-normal-state))

(defun razzi-current-line-empty-p ()
  (string-match-p "^\\s-*$" (thing-at-point 'line)))

;;;###autoload
(defun razzi-open-with-comma ()
  "Open a new line below and go into insert mode, adding a comma if needed"
  (interactive)
  (evil-with-single-undo
    (unless (razzi-current-line-empty-p)
      (move-end-of-line nil)
      (backward-char)
      (if (and (memq (string-to-char (thing-at-point 'char t))
		     '(?\" ?} ?\]))
	       (memq major-mode '(js-mode python-mode rjsx-mode)))
	  (razzi-append-comma)))
      (call-interactively 'evil-open-below)))

;;;###autoload
(defun razzi-mark-line-text ()
  (interactive)
  (move-end-of-line nil)
  (set-mark-command nil)
  (back-to-indentation))

;;;###autoload
(defun razzi-paste ()
  (interactive)
  (evil-paste-before 1)
  (right-char))

;;;###autoload
(defun razzi-flycheck-and-save-buffer ()
  (interactive)
  (if (buffer-modified-p)
      (save-buffer)
    (flycheck-buffer)))

(defun razzi-char-at-point ()
  (string-to-char (thing-at-point 'char t)))

;;;###autoload
(defun razzi-almost-end-of-line ()
  (interactive)
  (move-end-of-line 1)
  (backward-char)
  (forward-char))

;;;###autoload
(defun razzi-almost-end-of-buffer (arg)
  (interactive "P")
  (let ((inhibit-message t))
    (if (null arg)
        (progn
          (end-of-buffer)
          (previous-line))
      (evil-goto-line arg))))

;;;###autoload
(defun razzi-replay-q-macro ()
  (interactive)
  (let ((keys (evil-get-register ?q)))
    (evil-execute-macro 1 keys)))

;;;###autoload
(defun razzi-ivy-search-at-point ()
  (interactive)
  (let ((word (thing-at-point 'symbol)))
    (counsel-rg word)))

(provide 'razzi)
;;; razzi.el ends here
