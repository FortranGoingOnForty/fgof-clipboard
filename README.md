# fgof-clipboard

Clipboard text helpers for modern Fortran.

`fgof-clipboard` is intended to be a small standalone library for text clipboard
get or set flows with explicit backend and error reporting.

It is part of the [FortranGoingOnForty lib-modules](https://github.com/FortranGoingOnForty/lib-modules)
catalog, but it is intended to stand on its own as a normal `fpm` package.

## Status

Sprint 02 is in place.

Tracked today:

- stable clipboard result type
- reported backend identity on clipboard operations
- backend and error naming helpers
- working `get_clipboard_text()` and `set_clipboard_text()` entry points
- macOS `pbcopy` / `pbpaste` backend
- Linux `wl-copy` / `wl-paste`, `xclip`, and `xsel` command backends
- backend fallback hardening for stale detection probes
- mocked clipboard roundtrip, edge, and failure coverage in `fpm test`

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
- get and set flows try the preferred detected backend first, then fall through to later viable backends when launch-time probing was stale
- failed backend launches and runtime command failures report `io` rather than being silently downgraded to `unavailable`
- `get_clipboard_text()` and `set_clipboard_text()` return the detected backend in `clipboard_result%backend`
- when no supported backend is available, both operations report `unavailable`
- empty clipboard writes are treated as normal successful text operations when a backend exists
- text flows are command-backed today and focused on clipboard text, not arbitrary binary payloads
- CI uses an internal mock clipboard backend so test runs do not mutate the runner's real clipboard

## Build And Test

```bash
fpm test
```

## License

MIT
