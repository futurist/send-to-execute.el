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

;; make temp-mode-map for each popup buffer
(require 'temp-mode (expand-file-name "./temp-mode.el"))

;; eval input string as args list
;; http://emacs.stackexchange.com/questions/19877/how-to-evaluate-elisp-code-contained-in-a-string
(defun send-to-execute-eval-string (str)
  "Read and evaluate all forms in str.
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
(defun send-to-execute (&optional execute console-p &rest args)
  "EXECUTE string of command with current buffer or region."
  (interactive (list (read-from-minibuffer "Program to execute: ")
                     current-prefix-arg
                     (send-to-execute-eval-string (read-string "Arguments (quote each item, `[FILE]` as placeholder): " "\"[FILE]\""))))
  (when (and args (called-interactively-p))
    (setq args (car args)))
  (let* ((buffer-name (buffer-file-name))
         (file (make-temp-file execute nil (when buffer-name (file-name-extension buffer-name t))))
         (command-args (if args
                           (mapcar #'(lambda(item)
                                      (if (stringp item)
                                          (replace-regexp-in-string "\\[FILE\\]" file item t)
                                        (if (numberp item) (number-to-string item)
                                          (error "arguments must be string or number."))))
                                   args)
                         (list file)))
         (start (if (use-region-p) (region-beginning) (point-min)))
         (end (if (use-region-p) (region-end) (point-max)))
         ;; make proc execute under the temp path
         (default-directory (file-name-directory file))
         proc name buffer)
    (when console-p
      (setq command-args (if execute
                             (append (list "/k" execute) command-args)))
      (setq execute "cmd"))
    ;; when execute is nil
    (when (or (null execute) (equal execute ""))
      (setq execute "cmd"))
    (write-region start end file)
    (setq name (concat "*" execute (format-time-string "@%H:%M:%S") "*"))
    (setq buffer (create-file-buffer name))
    (pop-to-buffer buffer)
    (insert (format "generated below temp file for execute:\n%s" file))
    (insert (format "\n\nCommand line is:\n%s %s\n\n" execute command-args))
    (setq proc (apply #'start-process name buffer
                      execute
                      command-args))
    (temp-mode 1)
    ;; without ask kill process on exit
    (set-process-query-on-exit-flag proc nil)
    ;; Open the temp file in new buffer
    (define-key temp-mode-map (kbd "C-o") `(lambda() (interactive)
                                             (find-file ,file)))
    ;; C-d quickly close the buffer
    (define-key temp-mode-map (kbd "C-d") `(lambda() (interactive)
                                             (kill-this-buffer)
                                             (delete-file ,file)
                                             (winner-undo)))
    ;; return temp file name
    file))

(defun send-to-node ()
  (interactive)
  (send-to-execute "node"))

(defun send-to-electron ()
  (interactive)
  (send-to-execute "electron"))


(global-set-key (kbd "C-c s e") 'send-to-execute)
(global-set-key (kbd "C-c s t") 'send-to-electron)
(global-set-key (kbd "C-c s n") 'send-to-node)
