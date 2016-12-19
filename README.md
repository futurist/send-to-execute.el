# temp-run.el
Create temporary file from region or buffer, send it to run using shell command, popup output result and easily dismissed.

## Commands:

- `temp-run`
 Create temporary file from region or buffer, send it to run
 using shell command.  Temp file will be write to
 `temp-run-default-dir` (default value: `temporary-file-directory`)

 if USE-DEFAULT-DIR is non-nil or no buffer file,
 else write to same directory as buffer file.

 AFTER will applied with args (file dir filebase) after run.

Popup output buffer of run result, with the buffer name as
 "[run]@[temp-file-name]".

Within the buffer, turned on the minor mode `temp-run-mode',
 with below keys binding to each buffer:

    C-d to kill the output buffer, delete all the temp files with same file base.
    C-o to open the temp file.

- `temp-run-gcc`

Call `temp-run` with `gcc [FILE] -o [FILEBASE]`, then run the complied program to show result.

- `temp-run-node`

Call `temp-run` with `node [FILE]`, and show result.

- `temp-run-electron`

Call `temp-run` with `electron [FILE]`, and show result.


