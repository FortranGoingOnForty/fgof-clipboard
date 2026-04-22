program test_clipboard_fallback
  use fgof_clipboard, only : clipboard_backend_name, get_clipboard_text, set_clipboard_text
  use clipboard_test_support, only : configure_fallback_mock_clipboard
  use fgof_clipboard_types, only : clipboard_result
  implicit none

  type(clipboard_result) :: written
  type(clipboard_result) :: loaded
  character(len=:), allocatable :: backend
  character(len=:), allocatable :: failure
  character(len=:), allocatable :: test_text

  call configure_fallback_mock_clipboard()

  backend = clipboard_backend_name()
  if (backend /= "pbcopy") error stop "fallback test should keep pbcopy as the preferred backend"

  failure = ""
  test_text = "fgof-clipboard fallback"

  written = set_clipboard_text(test_text)
  if (.not. written%success .and. len(failure) == 0) failure = "fallback write should succeed through a later backend: backend=" // written%backend // " error=" // written%error_message
  if (written%backend /= "xclip" .and. len(failure) == 0) failure = "fallback write should report the backend that actually succeeded: got=" // written%backend

  loaded = get_clipboard_text()
  if (.not. loaded%success .and. len(failure) == 0) failure = "fallback read should succeed through a later backend: backend=" // loaded%backend // " error=" // loaded%error_message
  if (loaded%backend /= "xclip" .and. len(failure) == 0) failure = "fallback read should report the backend that actually succeeded"
  if (loaded%text /= test_text .and. len(failure) == 0) failure = "fallback read should preserve exact text: got=" // loaded%text

  if (len(failure) > 0) error stop failure
end program test_clipboard_fallback
