#!/bin/sh
set -eu

store_path="${FGOF_CLIPBOARD_TEST_STORE:?}"
fail_path="${FGOF_CLIPBOARD_TEST_FAIL_COPY:-}"

if [ -n "${fail_path}" ] && [ -e "${fail_path}" ]; then
  printf '%s\n' "mock clipboard copy failure" >&2
  exit 23
fi

cat > "${store_path}"
