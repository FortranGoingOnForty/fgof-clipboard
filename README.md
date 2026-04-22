# fgof-clipboard

Clipboard text helpers for modern Fortran.

`fgof-clipboard` is intended to be a small standalone library for text clipboard
get or set flows with explicit backend and error reporting.

It is part of the [FortranGoingOnForty lib-modules](https://github.com/FortranGoingOnForty/lib-modules)
catalog, but it is intended to stand on its own as a normal `fpm` package.

## Status

Sprint 01 is in place.

Tracked today:

- stable clipboard result type
- reported backend identity on clipboard operations
- backend and error naming helpers
- working `get_clipboard_text()` and `set_clipboard_text()` entry points
- macOS `pbcopy` / `pbpaste` backend
- Linux `wl-copy` / `wl-paste`, `xclip`, and `xsel` command backends
- backend-aware roundtrip coverage in `fpm test`

Current public types:

- `clipboard_result`

Current public procedures:

- `clear_clipboard_result`
- `get_clipboard_text`
- `set_clipboard_text`
- `clipboard_backend_name`
- `clipboard_error_name`

Current semantics:

- the package builds and tests cleanly on macOS and Ubuntu
- `clipboard_backend_name()` auto-detects `pbcopy`, `wl-clipboard`, `xclip`, `xsel`, or `unavailable`
- `get_clipboard_text()` and `set_clipboard_text()` return the detected backend in `clipboard_result%backend`
- when no supported backend is available, both operations report `unavailable`
- text flows are command-backed today and focused on clipboard text, not arbitrary binary payloads

## Build And Test

```bash
fpm test
```

## License

MIT
