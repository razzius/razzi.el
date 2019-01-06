;;; razzi.el --- Razzi's elisp functions -*- lexical-binding: t -*-
;;
;; Copyright Razzi Abuissa
;;
;; Author: Razzi Abuissa <razzi53@gmail.com>
;; URL: https://github.com/razzius/razzi.el
;; Version: 0.1.0
;; Keywords: 
;; Package-Requires: ((seq "1.11"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; elisp functions suitable for interactive use.

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

(provide 'razzi)
;;; razzi.el ends here
