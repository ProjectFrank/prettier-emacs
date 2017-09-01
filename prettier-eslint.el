;;; prettier-js.el --- Minor mode to format JS code on file save

;; Version: 0.1.0

;; Copyright (c) 2014 The go-mode Authors. All rights reserved.
;; Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:

;; * Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;; * Redistributions in binary form must reproduce the above
;; copyright notice, this list of conditions and the following disclaimer
;; in the documentation and/or other materials provided with the
;; distribution.
;; * Neither the name of the copyright holder nor the names of its
;; contributors may be used to endorse or promote products derived from
;; this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.)

;; Author: James Long and contributors
;; Created: 10 January 2017
;; Url: https://github.com/prettier/prettier-emacs
;; Keywords: convenience wp edit js

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Formats your JavaScript code using 'prettier' on file save.

;;; Code:

(defgroup prettier-eslint nil
  "Minor mode to format JS code on file save"
  :group 'languages
  :prefix "prettier-eslint"
  :link '(url-link :tag "Repository" "https://github.com/ProjectFrank/prettier-eslint-emacs"))

(defcustom prettier-eslint-args '()
  "List of args to send to prettier command."
  :type '(repeat string)
  :group 'prettier-eslint)

(defcustom prettier-eslint-show-errors 'buffer
  "Where to display prettier error output.
It can either be displayed in its own buffer, in the echo area, or not at all.
Please note that Emacs outputs to the echo area when writing
files and will overwrite prettier's echo output if used from inside
a `before-save-hook'."
  :type '(choice
          (const :tag "Own buffer" buffer)
          (const :tag "Echo area" echo)
          (const :tag "None" nil))
  :group 'prettier-eslint)

(defun prettier-eslint--goto-line (line)
  "Move cursor to line LINE."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun prettier-eslint--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "Invalid rcs patch or internal error in prettier-eslint--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (setq line-offset (- line-offset len))
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (prettier-eslint--goto-line (- from line-offset))
                (setq line-offset (+ line-offset len))
                (kill-whole-line len)
                (setq kill-ring (cdr kill-ring))))
             (t
              (error "Invalid rcs patch or internal error in prettier-eslint--apply-rcs-patch")))))))))

(defun prettier-eslint--process-errors (filename tmpfile errorfile errbuf)
  "Process errors for FILENAME, using a TMPFILE an ERRORFILE and display the output in ERRBUF."
  (with-current-buffer errbuf
    (if (eq prettier-eslint-show-errors 'echo)
        (progn
          (message "%s" (buffer-string))
          (prettier-eslint--kill-error-buffer errbuf))
      (insert-file-contents errorfile nil nil nil)
      ;; Convert the prettier stderr to something understood by the compilation mode.
      (goto-char (point-min))
      (insert "prettier errors:\n")
      (while (search-forward-regexp (regexp-quote tmpfile) nil t)
        (replace-match (file-name-nondirectory filename)))
      (compilation-mode)
      (display-buffer errbuf))))

(defun prettier-eslint--kill-error-buffer (errbuf)
  "Kill buffer ERRBUF."
  (let ((win (get-buffer-window errbuf)))
    (if win
        (quit-window t win)
      (with-current-buffer errbuf
        (erase-buffer))
      (kill-buffer errbuf))))

(defun prettier-eslint ()
  "Format the current buffer according to the prettier tool."
  (interactive)
  (let* ((ext (file-name-extension buffer-file-name t))
         (bufferfile (make-temp-file "prettier" nil ext))
         (outputfile (make-temp-file "prettier" nil ext))
         (errorfile (make-temp-file "prettier" nil ext))
         (errbuf (if prettier-eslint-show-errors (get-buffer-create "*prettier errors*")))
         (patchbuf (get-buffer-create "*prettier patch*"))
         (eslintfile (expand-file-name
                      ".eslintrc"
                      (locate-dominating-file
                       (or (buffer-file-name) default-directory)
                       ".eslintrc")))
         (eslint (expand-file-name
                  "node_modules/eslint"
                  (locate-dominating-file
                   (or (buffer-file-name) default-directory)
                   "node_modules")))
         (prettier (expand-file-name
                    "node_modules/prettier"
                    (locate-dominating-file
                     (or (buffer-file-name) default-directory)
                     "node_modules")))
         (prettier-eslint-command (expand-file-name
                                   "node_modules/.bin/prettier-eslint"
                                   (locate-dominating-file
                                    (or (buffer-file-name) default-directory)
                                    "node_modules")))
         (coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8))
    (unwind-protect
        (save-restriction
          (widen)
          (write-region nil nil bufferfile)
          (if errbuf
              (with-current-buffer errbuf
                (setq buffer-read-only nil)
                (erase-buffer)))
          (with-current-buffer patchbuf
            (erase-buffer))
          (if (zerop (apply 'call-process
                            prettier-eslint-command
                            bufferfile (list (list :file outputfile) errorfile)
                            nil (append prettier-eslint-args
                                        (list (buffer-file-name)
                                              "--stdin"
                                              "--eslint-path"
                                              eslint
                                              "--prettier-path"
                                              prettier))))
              (progn
                (call-process-region (point-min)
                                     (point-max)
                                     "diff"
                                     nil
                                     patchbuf
                                     nil
                                     "-n"
                                     "--strip-trailing-cr"
                                     "-"
                                     outputfile)
                (prettier-eslint--apply-rcs-patch patchbuf)
                (message "Applied prettier with args `%s'" prettier-eslint-args)
                (if errbuf (prettier-eslint--kill-error-buffer errbuf)))
            (message "Could not apply prettier")
            (if errbuf
                (prettier-eslint--process-errors (buffer-file-name) bufferfile errorfile errbuf))))
      (kill-buffer patchbuf)
      (delete-file errorfile)
      (delete-file bufferfile)
      (delete-file outputfile))))

;;;###autoload
(define-minor-mode prettier-eslint-mode
  "Runs prettier on file save when this mode is turned on"
  :lighter " Prettier"
  :global nil
  (if prettier-eslint-mode
      (add-hook 'before-save-hook 'prettier-eslint nil 'local)
    (remove-hook 'before-save-hook 'prettier-eslint 'local)))

(provide 'prettier-eslint)
;;; prettier-eslint.el ends here
