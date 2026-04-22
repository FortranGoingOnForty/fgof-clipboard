# fgof-clipboard

Clipboard text helpers for modern Fortran.

`fgof-clipboard` is intended to be a small standalone library for text clipboard
get or set flows with explicit backend and error reporting.

It is part of the [FortranGoingOnForty lib-modules](https://github.com/FortranGoingOnForty/lib-modules)
catalog, but it is intended to stand on its own as a normal `fpm` package.

## Status

Initial scaffold is in place.

Tracked today:

- stable clipboard result type
- backend and error naming helpers
- scaffold `get_clipboard_text()` and `set_clipboard_text()` entry points
- focused scaffold coverage in `fpm test`

## Build And Test

```bash
fpm test
```

## License

MIT
