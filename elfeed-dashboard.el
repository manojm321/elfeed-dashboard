;;; elfeed-dashboard.el --- A frontend for elfeed     -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Manoj Kumar Manikchand

;; Author: Manoj Kumar Manikchand <manojm321@protonmail.com>
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary: For a quick demo, open the elfeed-dashboard.org file provided
;;; with this repo and enable elfeed-dashboard-mode to activate it.

;;; Code:
(require 'elfeed-search)
(require 'org-element)
(require 'subr-x)

(defvar elfeed-dashboard--elfeed-update-timer nil)

;;;###autoload
(define-minor-mode elfeed-dashboard-mode
  "Minor mode to simulate buffer local keybindings."
  :keymap (make-sparse-keymap)
  :init-value nil
  (if (not (bound-and-true-p elfeed-dashboard-mode))
      (setq buffer-read-only nil)
    (setq buffer-read-only t)
    (elfeed-dashboard-parse-keymap)))

(defun elfeed-dashboard-query (query)
  "Set the search filter to QUERY and call elfeed."
  (elfeed-search-set-filter query)
  (elfeed)
  (goto-char (point-min)))

(defun elfeed-dashboard-update ()
  "Fetch new feeds, Optionally try reading elfeed-org configuration."
  (if (featurep 'elfeed-org)
    (elfeed-org))
  (unless elfeed-dashboard--elfeed-update-timer
    (elfeed-update)
    (setq elfeed-dashboard--elfeed-update-timer
          (run-with-timer 1 1 #'(lambda ()
                                  (if (> (elfeed-queue-count-total) 0)
                                      (message (format "elfeed: %d jobs pending.." (elfeed-queue-count-total)))
                                    (cancel-timer elfeed-dashboard--elfeed-update-timer)
                                    (setq elfeed-dashboard--elfeed-update-timer nil)
                                    (message "elfeed: Updated!")))))))

(defun elfeed-dashboard-parse-keymap ()
  "Install key binding defined as KEYMAP:VALUE.

VALUE is composed of \"keybinding | function-call\" with
keybidning begin a string describing a key sequence and a call to
an existing function. For example, to have 'q' to kill the
current buffer, the syntax would be:

#+KEYMAP: q | kill-current-buffer

This can be placed anywhere in the org file even though I advise
to group keymaps at the same place."

  (org-element-map (org-element-parse-buffer) 'keyword
    (lambda (keyword)
      (when (string= (org-element-property :key keyword) "KEYMAP")
        (let* ((value (org-element-property :value keyword))
               (key   (string-trim (nth 0 (split-string value "|"))))
               (call  (string-trim (nth 1 (split-string value "|")))))
          (define-key elfeed-dashboard-mode-map (kbd key)
            (eval (car (read-from-string
                        (format "(lambda () (interactive) (%s))" call)))))
          )))))

(provide 'elfeed-dashboard)
;;; elfeed-dashboard.el ends here
