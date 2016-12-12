# temp-execute.el
Create temporary file from region or buffer, send it to execute using shell command, popup output result and easily dismissed.

## Commands:

- `temp-execute`
 Create temporary file from region or buffer, send it to execute
 using shell command.  Temp file will be write to
 `temp-execute-default-dir` (default value: `temporary-file-directory`)

 if USE-DEFAULT-DIR is non-nil or no buffer file,
 else write to same directory as buffer file.

 AFTER will applied with args (file dir filebase) after execute.

Popup output buffer of execute result, with the buffer name as
 "[execute]@[temp-file-name]".

Within the buffer, turned on the minor mode `temp-execute-mode',
 with below keys binding to each buffer:

    C-d to kill the output buffer, delete all the temp files with same file base.
    C-o to open the temp file.

- `temp-execute-gcc`

Call `temp-execute` with `gcc [FILE] -o [FILEBASE]`, then run the complied program to show result.

- `temp-execute-node`

Call `temp-execute` with `node [FILE]`, and show result.

- `temp-execute-electron`

Call `temp-execute` with `electron [FILE]`, and show result.


