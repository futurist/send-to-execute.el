;;; temp-run.el --- Create temporary file from region or buffer, send it to execute using shell command. -*- lexical-binding: t; -*-

;; Filename: temp-run.el
;; Description: Create temporary file from region or buffer, send it to execute using shell command, popup output result and easily dismissed.
;; Author: James Yang <jamesyang999@gmail.com>
;; Copyright (C) 2016, James Yang, all rights reserved.
;; Time-stamp: <2016-12-09 17:37:51 James Yang>
;; Created: 2016-12-09 17:37:51
;; Version: 0.1.0
;; URL: http://github.com/futurist/temp-run.el
;; Keywords: temp, tempfile, execute, send-to
;; Package-Requires: ((emacs "24.1"))
;;

;;; This file is NOT part of GNU Emacs

;;; Commentary:

;; - `temp-run':
;; Create temporary file from region or buffer, send it to execute
;; using shell command.  Temp file will be write to
;; `temp-run-default-dir' (default value: `temporary-file-directory')
;; if USE-DEFAULT-DIR is non-nil or no buffer file,
;; else write to same directory as buffer file.
;; AFTER will applied with args (file dir filebase) after execute.

;; Popup output buffer of run result, with the buffer name as
;; "[run]@[temp-file-name]".

;; Within the buffer, turned on the minor mode `temp-run-mode',
;; with below keys binding to each buffer:

;; C-d to kill the output buffer, delete all the temp files with same file base.
;; C-o to open the temp file.

;; - `temp-run-gcc'
;; Call `temp-run' with `gcc [FILE] -o [FILEBASE]`, then run the complied program to show result.
;;
;; - `temp-run-node'
;; Call `temp-run' with `node [FILE]`, and show result.
;;
;; - `temp-run-electron'
;; Call `temp-run' with `electron [FILE]`, and show result.

;;; Code:
(require 'winner)

(defvar temp-run-default-dir temporary-file-directory
  "The default directory to store temporary files.
Initially set to `temporary-file-directory'")

(defvar temp-run-filename nil
  "Buffer local var to save current temp file name.")

(defvar temp-run-mode-map (make-sparse-keymap)
  "Buffer local var to save mode map.")

(progn
  (make-variable-buffer-local 'temp-run-filename)
  (define-key temp-run-mode-map (kbd "C-o") #'(lambda()
                                                    "Quickly open the temp file."
                                                    (interactive)
                                                    (find-file temp-run-filename)))
  (define-key temp-run-mode-map (kbd "C-d") #'(lambda()
                                                    "Quickly close the output buffer."
                                                    (interactive)
                                                    (let* ((filebase (file-name-base temp-run-filename))
                                                           (dir (file-name-directory temp-run-filename))
                                                           (files (directory-files dir t filebase t)))
                                                      (mapc #'delete-file files)
                                                      (kill-this-buffer)
                                                      (winner-undo)))))

(define-minor-mode temp-run-mode
  "Temp run minor mode with temp file."
  :init-value nil
  :lighter " TempRun"
  :keymap temp-run-mode-map
  (when temp-run-mode
    (message "C-d: close output and remove temp file.  C-o: open the temp file.")))

;; eval input string as args list
;; http://emacs.stackexchange.com/questions/19877/how-to-evaluate-elisp-code-contained-in-a-string
;; by @Tobias and @npostavs
(defun temp-run-eval-string (str)
  "Read and evaluate all forms in STR.
Return the results of all forms as a list."
  (let ((next 0)
        ret)
    (condition-case _err
        (while t
          (setq ret (cons (funcall (lambda (ret)
                                     (setq next (cdr ret))
                                     (eval (car ret)))
                                   (read-from-string str next))
                          ret)))
      (end-of-file))
    (nreverse ret)))

;;;###autoload
(defun temp-run (&optional execute use-default-dir after &rest args)
  "EXECUTE command by insert current buffer or region into temp file.
Write it into `temp-run-default-dir' if USE-DEFAULT-DIR
or no buffer-file, else write to same dir as buffer-file.
AFTER will be applied with (file dir filebase) as arguments.
ARGS will passed to EXECUTE."
  (interactive (list (read-from-minibuffer "Program to run: ")
                     nil current-prefix-arg
                     (temp-run-eval-string (read-string "Arguments (quote each item, `[FILE]` as placeholder): " "\"[FILE]\""))))
  (when (and args (called-interactively-p 'any))
    (setq args (car args)))
  (let* ((filename (buffer-file-name))
         (temporary-file-directory (if (or use-default-dir
                                           (not filename)
                                           (file-remote-p filename))
                                       temp-run-default-dir
                                     (file-name-directory filename)))
         (file (make-temp-file execute nil (when filename (file-name-extension filename t))))
         (filebase (file-name-base file))
         (command-args (if args
                           (mapcar #'(lambda(item)
                                       (if (stringp item)
                                           (replace-regexp-in-string "\\[FILEBASE\\]" filebase
                                                                     (replace-regexp-in-string "\\[FILE\\]" file item t) t)
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
    (insert (format "generated below temp file for run:\n%s" file))
    (insert (format "\n\nCommand line is:\n%s %s\n\n" execute command-args))
    ;; save filename local vars for each popup buffer
    (setq temp-run-filename file)
    ;; Open the temp file in new buffer
    (temp-run-mode 1)
    (if (not execute)
        (insert "file contents:\n\n" content)
      ;; only when execute non-nil, start the process
      (setq proc (apply #'start-process name buffer
                        execute
                        command-args))
      ;; without ask kill process on exit
      (set-process-query-on-exit-flag proc nil)
      (set-process-sentinel proc #'(lambda (proc event)
                                     (when (eq (process-status proc) 'exit)
                                       (message "temp run exit: %s" event)
                                       (when after
                                         (insert (apply after (list file default-directory filebase))))))))
    ;; return temp file name
    file))

(defun temp-run-node (use-default-dir)
  "Run `temp-run' with node, USE-DEFAULT-DIR is passed as is."
  (interactive "P")
  (temp-run "node" use-default-dir))

(defun temp-run-electron (use-default-dir)
  "Run `temp-run' with electron, USE-DEFAULT-DIR is passed as is."
  (interactive "P")
  (temp-run "electron" use-default-dir))

(defun temp-run-gcc (use-default-dir)
  "Run `temp-run' with gcc, USE-DEFAULT-DIR is passed as is."
  (interactive "P")
  (temp-run "gcc" use-default-dir
                '(lambda(file dir filebase)
                   (insert "The program result:\n")
                   (shell-command-to-string (expand-file-name (concat filebase) dir)))
                "[FILE]" "-o" "[FILEBASE]"))

(defun temp-run-rust (use-default-dir)
  "Run `temp-run' with gcc, USE-DEFAULT-DIR is passed as is."
  (interactive "P")
  (temp-run "rust" use-default-dir
                '(lambda(file dir filebase)
                   (insert "The program result:\n")
                   (shell-command-to-string (expand-file-name (concat filebase) dir)))
                "[FILE]"))

(global-set-key (kbd "C-c C-b e") 'temp-run)
(global-set-key (kbd "C-c C-b l") 'temp-run-electron)
(global-set-key (kbd "C-c C-b n") 'temp-run-node)
(global-set-key (kbd "C-c C-b g") 'temp-run-gcc)
(global-set-key (kbd "C-c C-b r") 'temp-run-rust)

(provide 'temp-run)
;;; temp-run.el ends here
