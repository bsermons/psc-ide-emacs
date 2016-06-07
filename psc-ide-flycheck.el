;;; psc-ide-flycheck.el --- Flycheck support for the purescript language -*- lexical-binding: t -*-

;; Copyright (c) 2015 The psc-ide-emacs authors

;; Author: Brian Sermons
;;         Bodil Stokke <bodil@bodil.org>
;; Package-Requires: ((flycheck "0.24") (emacs "24.4"))
;; URL: https://github.com/epost/psc-ide-emacs

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Usage: (eval-after-load 'flycheck
;;;          '(add-hook 'flycheck-mode-hook #'psc-ide-flycheck-setup))

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'let-alist))

(require 'seq)
(require 'json)
(require 'dash)
(require 'flycheck)
(require 'psc-ide-protocol)


(flycheck-def-option-var psc-ide-flycheck-ignored-error-codes nil psc-ide
  "List of errors codes to ignore."
  :tag "Flycheck PscIde Ignored Error Codes"
  :type '(repeat string))

(defun psc-ide-flycheck-parse-errors (data checker)
  "Decode purescript json output errors from DATA with CHECKER."
  (let (errors)
    (let-alist data
      (seq-do (lambda (err)
                (let-alist err
                  (unless (member .errorCode psc-ide-flycheck-ignored-error-codes)
                    (put-text-property 0 1 :suggestion .suggestion .errorCode)
                    (put-text-property 0 1 :end-line   .position.endLine .errorCode)
                    (put-text-property 0 1 :end-column .position.endColumn .errorCode)
                    (when .suggestion
                      (setq .message (concat .message " ●")))
                    (push (flycheck-error-new-at
                           .position.startLine
                           .position.startColumn
                           'error
                           .message
                           :id .errorCode
                           :checker checker
                           :filename .filename)
                          errors))))
              .result))
    errors))

;;;###autoload
(defun psc-ide-flycheck-insert-suggestion ()
  "Replace error with suggestion from psc compiler."
  (interactive)
  (-if-let* ((flycheck-err (car (flycheck-overlay-errors-at (point))))
             (suggestion   (get-text-property 0 :suggestion (flycheck-error-id flycheck-err)))
             (end-line     (get-text-property 0 :end-line   (flycheck-error-id flycheck-err)))
             (end-column   (get-text-property 0 :end-column (flycheck-error-id flycheck-err))))
      (let* ((start (save-excursion
                      (goto-char (point-min))
                      (forward-line (- (flycheck-error-line flycheck-err) 1))
                      (move-to-column (- (flycheck-error-column flycheck-err) 1))
                      (point)))
             (end (save-excursion
                    (goto-char (point-min))
                    (forward-line (- end-line 1))
                    (move-to-column (- end-column 1))
                    (point))))
        (progn
          (kill-region start end)
          (goto-char start)
          (let ((new-end
                 (save-excursion
                   (let-alist suggestion
                     (insert .replacement))
                   (point))))
            (set-mark start)
            (goto-char new-end)
            (deactivate-mark))))
    (message "No suggestion available")))

(define-key purescript-mode-map (kbd "C-c M-s")
  'psc-ide-flycheck-insert-suggestion)

(defun psc-ide-flycheck-start (checker callback)
  "Start a psc-ide syntax check with CHECKER.

CALLBACK is the status callback passed by flycheck."
  (psc-ide-send (psc-ide-command-rebuild)
                (lambda (result)
                  (condition-case err
                      (let ((errors (psc-ide-flycheck-parse-errors result checker)))
                        (funcall callback 'finished errors))
                    (`(error debug)
                     (funcall callback 'errored (error-message-string err)))))))

(flycheck-define-generic-checker 'psc-ide
  "A purescript syntax checker using the `psc-ide' interface."
  :start #'psc-ide-flycheck-start
  :modes '(purescript-mode))


;;;###autoload
(defun psc-ide-flycheck-setup ()
  "Setup Flycheck purescript."
  (interactive)
  (add-to-list 'flycheck-checkers 'psc-ide))

(provide 'psc-ide-flycheck)
;;; psc-ide-flycheck.el ends here
