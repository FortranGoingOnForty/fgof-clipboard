program test_clipboard_edges
  use fgof_clipboard, only : clipboard_backend_name, get_clipboard_text, set_clipboard_text
  use fgof_clipboard_types, only : clipboard_result
  implicit none

  type(clipboard_result) :: original
  type(clipboard_result) :: result_value
  character(len=:), allocatable :: backend
  character(len=:), allocatable :: failure

  backend = clipboard_backend_name()
  failure = ""

  if (backend == "unavailable") then
    result_value = get_clipboard_text()
    if (result_value%backend /= "unavailable" .and. len(failure) == 0) failure = "unavailable get should report unavailable backend"
    if (result_value%success .and. len(failure) == 0) failure = "unavailable get should not report success"
    if (result_value%text /= "" .and. len(failure) == 0) failure = "unavailable get should keep text empty"

    result_value = set_clipboard_text("")
    if (result_value%backend /= "unavailable" .and. len(failure) == 0) failure = "unavailable set should report unavailable backend"
    if (result_value%success .and. len(failure) == 0) failure = "unavailable set should not report success"
    if (result_value%text /= "" .and. len(failure) == 0) failure = "unavailable set should preserve empty attempted text"
  else
    original = get_clipboard_text()
    result_value = set_clipboard_text("")
    if (.not. result_value%success .and. len(failure) == 0) failure = "empty clipboard writes should succeed when a backend exists"

    result_value = get_clipboard_text()
    if (.not. result_value%success .and. len(failure) == 0) failure = "empty clipboard reads should succeed when a backend exists"
    if (result_value%text /= "" .and. len(failure) == 0) failure = "empty clipboard roundtrip should preserve empty text"

    if (original%success) then
      result_value = set_clipboard_text(original%text)
      if (.not. result_value%success .and. len(failure) == 0) failure = "clipboard restore should succeed after empty roundtrip"
    end if
  end if

  if (len(failure) > 0) error stop failure
end program test_clipboard_edges
