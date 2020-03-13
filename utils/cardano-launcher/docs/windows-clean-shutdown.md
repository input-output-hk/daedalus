# Windows - clean shutdown

On Windows, the only way to stop a non-gui program is by using [TerminateProcess](https://docs.microsoft.com/en-us/windows/win32/api/processthreadsapi/nf-processthreadsapi-terminateprocess).

> ### Remarks
>
> The `TerminateProcess` function is used to unconditionally cause a process to exit.
> ... This function stops execution of all threads within the process and requests cancellation of all pending I/O.
> ... A process cannot prevent itself from being terminated.

The problem is that if `cardano-wallet` is stopped like this, it will
be unable to nicely close its database, flush logs, or end any child
processes. The effect is basically the same as `kill -9` on POSIX.

## Solution

The only way to work around this is by implementing a method of
signalling to the child process that it should exit.

In the previous codebase, this was achieved (indirectly) with
`NodeIPC`. The `NodeIPC` thread in the child process reads a named
pipe, waiting for messages. If the parent process end of the named
pipe is close, the read function returns an error, and the program
shuts down.

We can achieve the same behaviour in the child process, without
`NodeIPC`, by continuously reading standard input. If there is an
error reading standard input, such as "end of file" or "broken pipe",
then the child process knows that it's time to exit. The parent
process can trigger this condition by closing the file descriptor that
it has passed as the `stdin` of the child process.

![Launch message sequence diagram](./launch.png)

## POSIX

On POSIX platforms, `cardano-wallet` can shutdown cleanly after being killed with `SIGTERM`.

However, we use identical shutdown methods on both Windows and POSIX
in an attempt to avoid platform-specific bugs.

## Timeouts

Since the child process is requested to shut itself down, it may
defectively fail to do so. This means that open files will still be
locked, etc, and will cause problems for users.

To guarantee that the child process exits, the child process ID should
be checked after a timeout period has elapsed. If it is still running
then it should be killed with `TerminateProcess`/`SIGKILL`.

## If the `cardano-launcher` process is killed

If the `cardano-launcher` (i.e. Daedalus) process itself is killed,
then it should ensure that its own child processes are killed.

In this case `stdin` of the child process will be automatically
closed, and it will exit.
