#!/bin/sh
set -eu

store_path="${FGOF_CLIPBOARD_TEST_STORE:?}"
fail_path="${FGOF_CLIPBOARD_TEST_FAIL_PASTE:-}"

if [ -n "${fail_path}" ] && [ -e "${fail_path}" ]; then
  printf '%s\n' "mock clipboard paste failure" >&2
  exit 29
fi

if [ -e "${store_path}" ]; then
  cat "${store_path}"
fi
