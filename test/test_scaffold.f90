program test_scaffold
  use fgof_clipboard, only : &
    FGOF_CLIPBOARD_ERR_INTERNAL, &
    FGOF_CLIPBOARD_ERR_INVALID_OPTIONS, &
    FGOF_CLIPBOARD_ERR_IO, &
    FGOF_CLIPBOARD_ERR_UNAVAILABLE, &
    FGOF_CLIPBOARD_OK, &
    clear_clipboard_result, &
    clipboard_backend_name, &
    clipboard_error_name, &
    get_clipboard_text, &
    set_clipboard_text
  use fgof_clipboard_types, only : clipboard_result
  implicit none

  type(clipboard_result) :: result_value

  result_value = clear_clipboard_result()
  if (result_value%success) error stop "clipboard result should start unsuccessful"
  if (result_value%error_code /= FGOF_CLIPBOARD_OK) error stop "clipboard result should start ok"
  if (result_value%text /= "") error stop "clipboard result should start with empty text"
  if (result_value%error_message /= "") error stop "clipboard result should start with empty error_message"

  if (clipboard_backend_name() /= "scaffold") error stop "backend helper should describe scaffold backend"
  if (clipboard_error_name(FGOF_CLIPBOARD_OK) /= "ok") error stop "error helper should map ok"
  if (clipboard_error_name(FGOF_CLIPBOARD_ERR_INVALID_OPTIONS) /= "invalid-options") error stop "error helper should map invalid options"
  if (clipboard_error_name(FGOF_CLIPBOARD_ERR_UNAVAILABLE) /= "unavailable") error stop "error helper should map unavailable"
  if (clipboard_error_name(FGOF_CLIPBOARD_ERR_IO) /= "io") error stop "error helper should map io"
  if (clipboard_error_name(FGOF_CLIPBOARD_ERR_INTERNAL) /= "internal") error stop "error helper should map internal"
  if (clipboard_error_name(999) /= "unknown") error stop "error helper should map unknown codes"

  result_value = get_clipboard_text()
  if (result_value%error_code /= FGOF_CLIPBOARD_ERR_UNAVAILABLE) error stop "scaffold get should report unavailable"
  if (result_value%success) error stop "scaffold get should not report success"

  result_value = set_clipboard_text("hello")
  if (result_value%error_code /= FGOF_CLIPBOARD_ERR_UNAVAILABLE) error stop "scaffold set should report unavailable"
  if (result_value%text /= "hello") error stop "scaffold set should preserve attempted text in result"
  if (result_value%success) error stop "scaffold set should not report success"
end program test_scaffold
