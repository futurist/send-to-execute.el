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

;;;###autoload
(defun send-to-execute (&optional execute args console-p keep-output-p)
  "EXECUTE string of command with current buffer or region."
  (let* ((buffer-name (buffer-file-name))
         (file (make-temp-file execute nil (when buffer-name (file-name-extension buffer-name t))))
         (command-args (if args (replace-regexp-in-string "\\[FILENAME\\]" file args) file))
         (keep (not (null keep-output-p)))
         (start (if (use-region-p) (region-beginning) (point-min)))
         (end (if (use-region-p) (region-end) (point-max)))
         proc name buffer)
    (when console-p
      (setq command-args (if execute
                             (format "/k %s %s" execute command-args)
                           ;; empty command when no execute
                           (format "/k")))
      (setq execute "cmd"))
    (write-region start end file)
    (setq name (concat "*" execute (format-time-string "@%H:%M:%S") "*"))
    (setq buffer (create-file-buffer name))
    (pop-to-buffer buffer)
    (insert "generated below temp file for execute:\n" file "\n\n")
    (setq proc (start-process name buffer
                              (or execute "cmd")
                              command-args))
    (set-process-sentinel proc `(lambda (proc event)
                                  (when (and (not ,keep) (member (process-status proc) '(exit signal)))
                                    (when (get-buffer ,name) (kill-buffer ,name))
                                    (delete-file ,file)
                                    (winner-undo))))
    (temp-mode 1)
    ;; without ask kill process on exit
    (set-process-query-on-exit-flag proc nil)
    ;; Open the temp file in new buffer
    (define-key temp-mode-map (kbd "C-o") `(lambda() (interactive)
                                             (find-file ,file)))
    ;; C-d quickly close the buffer
    (define-key temp-mode-map (kbd "C-d") '(lambda() (interactive)
                                             (kill-this-buffer)
                                             (winner-undo)))
    ;; return temp file name
    file))

(defun send-to-node (keep)
  (interactive "P")
  (send-to-execute "node" nil t t))

(defun send-to-electron (keep)
  (interactive "P")
  (send-to-execute "electron"))


(global-set-key (kbd "C-c e e") 'send-to-electron)
(global-set-key (kbd "C-c e n") 'send-to-node)
