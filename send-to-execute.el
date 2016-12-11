;;; send-to-execute.el --- Send buffer or region to execute with temp file -*- lexical-binding: t; -*-

;; Filename: send-to-execute.el
;; Description: Send buffer or region to execute with temp file, popup result and easily dismiss.
;; Author: James Yang <jamesyang999@gmail.com>
;; Copyright (C) 2016, James Yang, all rights reserved.
;; Time-stamp: <2016-12-09 17:37:51 James Yang>
;; Created: 2016-12-09 17:37:51
;; Version: 0.1.0
;; URL: http://github.com/futurist/send-to-execute.el
;; Keywords: send, temp, tempfile, execute
;; Package-Requires: ((emacs "24.1"))
;;

;;; This file is NOT part of GNU Emacs

;;; Commentary:

;; Send buffer or region to execute with temp file, Write it into
;; `send-to-execute-default-dir' if USE-DEFAULT-DIR or no buffer-file,
;; else write to same dir as buffer-file.  Popup result buffer, using
;; "execute@temp-file-name" as name.

;; Within the buffer, C-d to kill the buffer and delete the temp file,
;; C-o to open the temp file.

;;; Code:

;; make temp-mode-map for each popup buffer
(load (expand-file-name "temp-mode.el"))

(defvar send-to-execute-default-dir temporary-file-directory
  "The default directory to store temporary files.
Initially set to `temporary-file-directory'")

;; eval input string as args list
;; http://emacs.stackexchange.com/questions/19877/how-to-evaluate-elisp-code-contained-in-a-string
(defun send-to-execute-eval-string (str)
  "Read and evaluate all forms in STR.
Return the results of all forms as a list."
  (let ((next 0)
        ret)
    (condition-case err
        (while t
          (setq ret (cons (funcall (lambda (ret)
                                     (setq next (cdr ret))
                                     (eval (car ret)))
                                   (read-from-string str next))
                          ret)))
      (end-of-file))
    (nreverse ret)))

;;;###autoload
(defun send-to-execute (&optional execute use-default-dir &rest args)
  "EXECUTE command by insert current buffer or region into temp file.
Write it into `send-to-execute-default-dir' if USE-DEFAULT-DIR
or no buffer-file, else write to same dir as buffer-file.
ARGS will passed to EXECUTE."
  (interactive (list (read-from-minibuffer "Program to execute: ")
                     nil current-prefix-arg
                     (send-to-execute-eval-string (read-string "Arguments (quote each item, `[FILE]` as placeholder): " "\"[FILE]\""))))
  (when (and args (called-interactively-p 'any))
    (setq args (car args)))
  (let* ((buffer-name (buffer-file-name))
         (temporary-file-directory (if (or use-default-dir (not (buffer-file-name)))
                                       send-to-execute-default-dir
                                     (file-name-directory (buffer-file-name))))
         (file (make-temp-file execute nil (when buffer-name (file-name-extension buffer-name t))))
         (command-args (if args
                           (mapcar #'(lambda(item)
                                       (if (stringp item)
                                           (replace-regexp-in-string "\\[FILE\\]" file item t)
                                         (if (numberp item) (number-to-string item)
                                           (error "Arguments must be string or number"))))
                                   args)
                         (list file)))
         (start (if (use-region-p) (region-beginning) (point-min)))
         (end (if (use-region-p) (region-end) (point-max)))
         (content (buffer-substring start end))
         ;; make proc execute under the temp path
         (default-directory (file-name-directory file))
         proc name buffer)
    ;; when execute is nil
    (when (or (not (stringp execute)) (equal execute ""))
      (setq execute nil))
    (write-region content nil file)
    (setq name (concat "*" execute "@"
                       (file-name-nondirectory file) "*"))
    (setq buffer (create-file-buffer name))
    (pop-to-buffer buffer)
    (insert (format "generated below temp file for execute:\n%s" file))
    (insert (format "\n\nCommand line is:\n%s %s\n\n" execute command-args))
    ;; to make sparse key map
    (temp-mode 1)
    ;; Open the temp file in new buffer
    (define-key temp-mode-map (kbd "C-o") `(lambda() (interactive)
                                             (find-file ,file)))
    ;; C-d quickly close the buffer
    (define-key temp-mode-map (kbd "C-d") `(lambda() (interactive)
                                             (kill-this-buffer)
                                             (delete-file ,file)
                                             (winner-undo)))
    (message "C-d: close output and remove temp file.  C-o: open the temp file.")
    (if (not execute)
        (insert "file contents:\n\n" content)
      ;; only when execute non-nil, start the process
      (setq proc (apply #'start-process name buffer
                        execute
                        command-args))
      ;; without ask kill process on exit
      (set-process-query-on-exit-flag proc nil))
    ;; return temp file name
    file))

(defun send-to-node (use-default-dir)
  "Send buffer or region into temp file, pass to node to execute.
USE-DEFAULT-DIR to using `send-to-execute-default-dir' as folder."
  (interactive "P")
  (send-to-execute "node" use-default-dir))

(defun send-to-electron (use-default-dir)
  "Send buffer or region into temp file, pass to electron to execute.
USE-DEFAULT-DIR to using `send-to-execute-default-dir' as folder."
  (interactive "P")
  (send-to-execute "electron" use-default-dir))

(global-set-key (kbd "C-c C-b e") 'send-to-execute)
(global-set-key (kbd "C-c C-b l") 'send-to-electron)
(global-set-key (kbd "C-c C-b n") 'send-to-node)

(provide 'send-to-execute)
;;; send-to-execute.el ends here
