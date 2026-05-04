---
outline: [2, 3]
---

# Concurrency

Cooperative async concurrency with promises and channels. Tasks run on the VM's cooperative scheduler, interleaving at yield points (channel operations, `await`, `sleep`).

Async features require the VM backend (default since v1.13). The tree-walker returns an error.

## Promises

### `async/spawn`

```sema
(async/spawn thunk) → async-promise
```

Spawn a zero-argument function as an async task. Returns a promise that resolves when the task completes.

```sema
(define p (async/spawn (fn () (+ 1 2))))
(async/await p)  ; => 3
```

Usually called via the `async` special form:

```sema
(define p (async (+ 1 2)))
(await p)  ; => 3
```

### `async/await`

```sema
(async/await promise) → value
```

Wait for a promise to resolve. Inside an async task, yields to the scheduler. At the top level, runs the scheduler inline until the promise resolves. Raises an error if the promise was rejected.

### `async/all`

```sema
(async/all promises) → list
```

Run all promises to completion and return a list of their results. Takes a list or vector of promises.

```sema
(let ((p1 (async 10))
      (p2 (async 20))
      (p3 (async 30)))
  (async/all (list p1 p2 p3)))  ; => (10 20 30)
```

### `async/race`

```sema
(async/race promises) → value
```

Return the value of the first promise to resolve. Takes a list or vector of promises.

### `async/resolved`

```sema
(async/resolved value) → async-promise
```

Create an already-resolved promise wrapping `value`.

### `async/rejected`

```sema
(async/rejected message) → async-promise
```

Create an already-rejected promise with `message`.

### `async/run`

```sema
(async/run)
```

Run all pending async tasks to completion.

### `async/sleep`

```sema
(async/sleep ms)
```

Inside an async task, yield for at least `ms` milliseconds (real timing — the scheduler will not wake the task until the deadline elapses). Outside async, calls `thread::sleep`.

### `async/timeout`

```sema
(async/timeout ms promise) → value
```

Wait for `promise` to resolve, but raise an error if it takes longer than `ms` milliseconds. The underlying task is **not** automatically cancelled; pair with `async/cancel` if you need to free its resources.

```sema
(async/timeout 100 (async (do-slow-work)))
;; raises: async/timeout: operation timed out
```

`ms = 0` causes an immediate timeout if the promise has not already resolved.

### `async/cancel`

```sema
(async/cancel promise)
```

Request cancellation of a spawned task. The next time the task hits a yield point, it rejects with `"cancelled"`. Errors if called on a non-spawned promise (e.g., `async/resolved`). Always pair with cleanup that doesn't depend on the task finishing.

### `async/cancelled?`

```sema
(async/cancelled? promise) → bool
```

`#t` if `promise` was cancelled (rejected with `"cancelled"`).

### Promise predicates

| Function | Description |
| --- | --- |
| `(async/promise? x)` | Is `x` an async promise? |
| `(async/resolved? p)` | Is promise `p` resolved? |
| `(async/rejected? p)` | Is promise `p` rejected? |
| `(async/pending? p)` | Is promise `p` still pending? |
| `(async/cancelled? p)` | Was promise `p` cancelled? |

## Channels

Bounded FIFO channels for communication between async tasks.

### `channel/new`

```sema
(channel/new)         → channel  ; capacity 1
(channel/new capacity) → channel
```

Create a bounded channel. Default capacity is 1. Capacity must be at least 1.

### `channel/send`

```sema
(channel/send ch value)
```

Send a value to the channel. If the channel is full and inside an async task, yields until space is available. Outside async context, raises an error if full. Raises an error if the channel is closed.

### `channel/recv`

```sema
(channel/recv ch) → value
```

Receive a value from the channel. If the channel is empty and inside an async task, yields until data is available. Outside async context, raises an error if empty. Returns `nil` if the channel is closed and empty.

### `channel/try-recv`

```sema
(channel/try-recv ch) → value | nil
```

Non-blocking receive. Returns the next value or `nil` if the channel is empty.

### `channel/close`

```sema
(channel/close ch)
```

Close the channel. Subsequent sends will error. Blocked receivers will wake with `nil`.

### Channel predicates

| Function | Description |
| --- | --- |
| `(channel? x)` | Is `x` a channel? |
| `(channel/closed? ch)` | Is the channel closed? |
| `(channel/empty? ch)` | Is the channel buffer empty? |
| `(channel/full? ch)` | Is the channel buffer at capacity? |
| `(channel/count ch)` | Number of values in the buffer |

## Examples

### Producer/Consumer

```sema
(let ((ch (channel/new 1)))
  (let ((producer (async
          (channel/send ch 10)
          (channel/send ch 20)
          (channel/send ch 30)
          (channel/close ch)))
        (consumer (async
          (let loop ((sum 0))
            (let ((val (channel/recv ch)))
              (if (nil? val)
                sum
                (loop (+ sum val))))))))
    (await consumer)))  ; => 60
```

### Parallel computation

```sema
(let ((p1 (async (fib 30)))
      (p2 (async (fib 31))))
  (+ (await p1) (await p2)))
```

Note: tasks are cooperative, not parallel. They interleave at yield points, not at arbitrary instructions. CPU-bound tasks without yield points run to completion before other tasks get a turn.
