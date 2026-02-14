---
outline: [2, 3]
---

# Standard Library

Sema ships with **350+ built-in functions** across **17 modules**, covering everything from string manipulation and file I/O to HTTP requests, regex, and cryptographic hashing.

## Naming Conventions

Sema's stdlib follows consistent naming patterns:

| Pattern | Convention | Example |
|---|---|---|
| `module/function` | Slash-namespaced | `string/trim`, `file/read`, `math/gcd` |
| `legacy-name` | Scheme compatibility | `string-append`, `string-length` |
| `type->type` | Arrow conversions | `string->symbol`, `list->vector` |
| `predicate?` | Predicate suffix | `null?`, `list?`, `even?` |

## Quick Reference

### [Math & Arithmetic](./math)

| Function | Description |
|---|---|
| `+`, `-`, `*`, `/`, `mod` | Basic arithmetic |
| `<`, `>`, `<=`, `>=`, `=` | Comparison |
| `abs`, `min`, `max`, `pow`, `sqrt`, `log` | Numeric utilities |
| `floor`, `ceil`, `round`, `truncate` | Rounding |
| `sin`, `cos`, `math/tan` | Trigonometry |
| `math/asin`, `math/acos`, `math/atan`, `math/atan2` | Inverse trig |
| `math/sinh`, `math/cosh`, `math/tanh` | Hyperbolic |
| `math/exp`, `math/log10`, `math/log2` | Exponential & logarithmic |
| `math/gcd`, `math/lcm`, `math/quotient`, `math/remainder` | Integer math |
| `math/random`, `math/random-int` | Random numbers |
| `math/clamp`, `math/sign`, `math/lerp`, `math/map-range` | Interpolation & clamping |
| `math/degrees->radians`, `math/radians->degrees` | Angle conversion |
| `even?`, `odd?`, `positive?`, `negative?`, `zero?` | Numeric predicates |
| `math/nan?`, `math/infinite?` | Float predicates |
| `pi`, `e`, `math/infinity`, `math/nan` | Constants |
| `bit/and`, `bit/or`, `bit/xor`, `bit/not`, `bit/shift-left`, `bit/shift-right` | Bitwise operations |

### [Strings & Characters](./strings)

| Function | Description |
|---|---|
| `string-append`, `string-length`, `string-ref`, `substring` | Core string ops |
| `str`, `format` | Conversion & formatting |
| `string/split`, `string/join`, `string/trim` | Split, join, trim |
| `string/upper`, `string/lower`, `string/capitalize`, `string/title-case` | Case conversion |
| `string/contains?`, `string/starts-with?`, `string/ends-with?` | Search predicates |
| `string/replace`, `string/index-of`, `string/reverse` | Manipulation |
| `string/chars`, `string/repeat`, `string/pad-left`, `string/pad-right` | Utilities |
| `string/map`, `string/number?` | Higher-order & predicates |
| `char->integer`, `integer->char`, `char-alphabetic?`, ... | Character operations |
| `string->number`, `number->string`, `string->symbol`, ... | Type conversions |

### [Lists](./lists)

| Function | Description |
|---|---|
| `list`, `cons`, `car`, `cdr`, `first`, `rest` | Construction & access |
| `cadr`, `caddr`, `last`, `nth` | Positional access |
| `length`, `append`, `reverse`, `range` | Basic operations |
| `map`, `filter`, `foldl`, `foldr`, `reduce` | Higher-order functions |
| `sort`, `sort-by`, `apply`, `for-each` | Ordering & application |
| `take`, `drop`, `flatten`, `zip`, `partition` | Sublists |
| `member`, `any`, `every`, `list/index-of`, `list/unique` | Searching |
| `list/group-by`, `list/interleave`, `list/chunk`, `frequencies` | Grouping |
| `list/sum`, `list/min`, `list/max` | Aggregation |
| `list/shuffle`, `list/pick` | Random |
| `list/repeat`, `make-list`, `iota` | Construction |
| `list/split-at`, `list/take-while`, `list/drop-while` | Splitting |
| `assoc`, `assq`, `assv` | Association lists |
| `interpose` | Interleaving |

### [Vectors](./vectors)

| Function | Description |
|---|---|
| `vector` | Create a vector |
| `vector->list`, `list->vector` | Conversion |

### [Maps & HashMaps](./maps)

| Function | Description |
|---|---|
| `hash-map`, `get`, `assoc`, `dissoc`, `merge` | Core map ops |
| `keys`, `vals`, `contains?`, `count` | Inspection |
| `map/entries`, `map/from-entries` | Entry conversion |
| `map/map-vals`, `map/map-keys`, `map/filter` | Higher-order |
| `map/select-keys`, `map/update` | Selection & update |
| `hashmap/new`, `hashmap/get`, `hashmap/assoc`, ... | HashMap operations |

### [Predicates & Type Checking](./predicates)

| Function | Description |
|---|---|
| `null?`, `nil?`, `empty?`, `list?`, `pair?` | Collection predicates |
| `number?`, `integer?`, `float?`, `string?`, `symbol?`, `keyword?` | Type predicates |
| `char?`, `record?`, `bytevector?`, `bool?`, `fn?` | More type predicates |
| `map?`, `vector?` | Container predicates |
| `promise?`, `promise-forced?` | Promise predicates |
| `eq?`, `=`, `zero?`, `even?`, `odd?`, `positive?`, `negative?` | Equality & numeric |
| `prompt?`, `message?`, `conversation?`, `tool?`, `agent?` | LLM type predicates |

### [File I/O & Paths](./file-io)

| Function | Description |
|---|---|
| `display`, `println`, `print`, `newline`, `read-line` | Console I/O |
| `file/read`, `file/write`, `file/append` | File read/write |
| `file/read-lines`, `file/write-lines` | Line-based I/O |
| `file/delete`, `file/rename`, `file/copy` | File management |
| `file/exists?`, `file/is-file?`, `file/is-directory?`, `file/is-symlink?` | File predicates |
| `file/list`, `file/mkdir`, `file/info` | Directory operations |
| `path/join`, `path/dirname`, `path/basename`, `path/extension`, `path/absolute` | Path manipulation |

### [HTTP & JSON](./http-json)

| Function | Description |
|---|---|
| `http/get`, `http/post`, `http/put`, `http/delete`, `http/request` | HTTP methods |
| `json/encode`, `json/encode-pretty`, `json/decode` | JSON serialization |

### [Regex](./regex)

| Function | Description |
|---|---|
| `regex/match?`, `regex/match`, `regex/find-all` | Matching |
| `regex/replace`, `regex/replace-all`, `regex/split` | Replacement & splitting |

### [CSV, Crypto & Encoding](./csv-encoding)

| Function | Description |
|---|---|
| `csv/parse`, `csv/parse-maps`, `csv/encode` | CSV operations |
| `uuid/v4` | UUID generation |
| `base64/encode`, `base64/decode` | Base64 encoding |
| `hash/sha256` | Hashing |

### [Date & Time](./datetime)

| Function | Description |
|---|---|
| `time/now`, `time-ms` | Current time |
| `time/format`, `time/parse` | Formatting & parsing |
| `time/date-parts` | Date decomposition |
| `time/add`, `time/diff` | Arithmetic |
| `sleep` | Delay execution |

### [System](./system)

| Function | Description |
|---|---|
| `env`, `sys/env-all`, `sys/set-env` | Environment variables |
| `sys/args`, `sys/cwd`, `sys/platform`, `sys/os`, `sys/arch` | System info |
| `sys/pid`, `sys/tty`, `sys/which`, `sys/elapsed` | Process info |
| `sys/interactive?`, `sys/hostname`, `sys/user` | Session info |
| `sys/home-dir`, `sys/temp-dir` | Directory paths |
| `shell` | Run shell commands |
| `exit` | Exit process |

### [Bytevectors](./bytevectors)

| Function | Description |
|---|---|
| `bytevector`, `make-bytevector` | Construction |
| `bytevector-length`, `bytevector-u8-ref`, `bytevector-u8-set!` | Access & mutation |
| `bytevector-copy`, `bytevector-append` | Copy & append |
| `bytevector->list`, `list->bytevector` | List conversion |
| `utf8->string`, `string->utf8` | String conversion |

### [Records](./records)

| Function | Description |
|---|---|
| `define-record-type` | Define a record type |
| `record?` | Record predicate |
| `type` | Get record type tag |

### [Playground & WASM](./playground)

| Function | Description |
|---|---|
| `web/user-agent` | Browser user agent string (WASM only) |
| `web/user-agent-data` | Structured browser info map (Chromium only, WASM only) |
