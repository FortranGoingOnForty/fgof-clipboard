program test_clipboard_failures
  use fgof_clipboard, only : FGOF_CLIPBOARD_ERR_IO, get_clipboard_text, set_clipboard_text
  use clipboard_test_support, only : &
    delete_file_if_exists, &
    mock_clipboard_enabled, &
    required_test_env, &
    touch_file
  use fgof_clipboard_types, only : clipboard_result
  implicit none

  type(clipboard_result) :: result_value
  character(len=:), allocatable :: fail_copy_path
  character(len=:), allocatable :: fail_paste_path

  if (.not. mock_clipboard_enabled()) stop

  fail_copy_path = required_test_env("FGOF_CLIPBOARD_TEST_FAIL_COPY")
  fail_paste_path = required_test_env("FGOF_CLIPBOARD_TEST_FAIL_PASTE")

  call delete_file_if_exists(fail_copy_path)
  call delete_file_if_exists(fail_paste_path)

  call touch_file(fail_copy_path)
  result_value = set_clipboard_text("fgof-clipboard failure")
  if (result_value%success) error stop "copy failures should not report success"
  if (result_value%error_code /= FGOF_CLIPBOARD_ERR_IO) error stop "copy failures should report io"
  if (result_value%backend == "unavailable") error stop "copy failures should preserve attempted backend"
  if (index(result_value%error_message, "clipboard write failed") == 0) error stop "copy failures should report write context"
  call delete_file_if_exists(fail_copy_path)

  result_value = set_clipboard_text("fgof-clipboard recovery")
  if (.not. result_value%success) error stop "clipboard writes should recover after copy failure"

  call touch_file(fail_paste_path)
  result_value = get_clipboard_text()
  if (result_value%success) error stop "paste failures should not report success"
  if (result_value%error_code /= FGOF_CLIPBOARD_ERR_IO) error stop "paste failures should report io"
  if (result_value%backend == "unavailable") error stop "paste failures should preserve attempted backend"
  if (index(result_value%error_message, "clipboard read failed") == 0) error stop "paste failures should report read context"
  call delete_file_if_exists(fail_paste_path)
end program test_clipboard_failures
