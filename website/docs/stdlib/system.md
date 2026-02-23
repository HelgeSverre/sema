---
outline: [2, 3]
---

# System

## Environment Variables

### `env`

Get the value of an environment variable. Returns `nil` if not set.

```sema
(env "HOME")       ; => "/Users/ada"
(env "PATH")       ; => "/usr/bin:/bin:..."
(env "MISSING")    ; => nil
```

### `sys/env-all`

Return all environment variables as a map.

```sema
(sys/env-all)   ; => {:HOME "/Users/ada" :PATH "..." ...}
```

### `sys/set-env`

Set an environment variable for the current process.

```sema
(sys/set-env "KEY" "value")
(env "KEY")   ; => "value"
```

## System Information

### `sys/args`

Return the command-line arguments as a list.

```sema
(sys/args)   ; => ("sema" "script.sema" "--flag")
```

### `sys/cwd`

Return the current working directory.

```sema
(sys/cwd)   ; => "/current/dir"
```

### `sys/platform`

Return the platform name.

```sema
(sys/platform)   ; => "macos" / "linux" / "windows"
```

### `sys/os`

Return the operating system name.

```sema
(sys/os)   ; => "macos"
```

### `sys/arch`

Return the CPU architecture.

```sema
(sys/arch)   ; => "aarch64" / "x86_64"
```

## Process Information

### `sys/pid`

Return the current process ID.

```sema
(sys/pid)   ; => 12345
```

### `sys/tty`

Return the TTY device path, or `nil` if not running in a terminal.

```sema
(sys/tty)   ; => "/dev/ttys003" or nil
```

### `sys/which`

Find the full path to an executable, or `nil` if not found.

```sema
(sys/which "cargo")   ; => "/Users/ada/.cargo/bin/cargo"
(sys/which "nonexistent")  ; => nil
```

### `sys/elapsed`

Return nanoseconds elapsed since the process started.

```sema
(sys/elapsed)   ; => 482937100
```

## Session Information

### `sys/interactive?`

Test if stdin is a TTY (i.e., running interactively).

```sema
(sys/interactive?)   ; => #t in REPL, #f in scripts
```

### `sys/hostname`

Return the system hostname.

```sema
(sys/hostname)   ; => "my-machine"
```

### `sys/user`

Return the current username.

```sema
(sys/user)   ; => "ada"
```

## Directory Paths

### `sys/home-dir`

Return the user's home directory.

```sema
(sys/home-dir)   ; => "/Users/ada"
```

### `sys/temp-dir`

Return the system temporary directory.

```sema
(sys/temp-dir)   ; => "/tmp"
```

## Shell & Process Control

### `shell`

Run a shell command and return its stdout as a string.

```sema
(shell "ls -la")       ; => "total 42\n..."
(shell "echo hello")   ; => "hello\n"
```

### `exit`

Exit the process with a given status code.

```sema
(exit 0)   ; exit successfully
(exit 1)   ; exit with error
```
