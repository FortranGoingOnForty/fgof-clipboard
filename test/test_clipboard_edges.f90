program test_clipboard_edges
  use fgof_clipboard, only : get_clipboard_text, set_clipboard_text
  use clipboard_test_support, only : mock_clipboard_enabled
  use fgof_clipboard_types, only : clipboard_result
  implicit none

  type(clipboard_result) :: result_value
  character(len=:), allocatable :: failure

  if (.not. mock_clipboard_enabled()) stop

  failure = ""

  result_value = set_clipboard_text("")
  if (.not. result_value%success .and. len(failure) == 0) failure = "empty clipboard writes should succeed when a backend exists"
  if (result_value%text /= "" .and. len(failure) == 0) failure = "empty clipboard writes should preserve empty attempted text"

  result_value = get_clipboard_text()
  if (.not. result_value%success .and. len(failure) == 0) failure = "empty clipboard reads should succeed when a backend exists"
  if (result_value%text /= "" .and. len(failure) == 0) failure = "empty clipboard roundtrip should preserve empty text"

  if (len(failure) > 0) error stop failure
end program test_clipboard_edges
