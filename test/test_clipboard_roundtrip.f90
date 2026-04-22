program test_clipboard_roundtrip
  use fgof_clipboard, only : clipboard_backend_name, get_clipboard_text, set_clipboard_text
  use clipboard_test_support, only : ensure_mock_clipboard, is_known_backend
  use fgof_clipboard_types, only : clipboard_result
  implicit none

  type(clipboard_result) :: written
  type(clipboard_result) :: loaded
  character(len=:), allocatable :: backend
  character(len=:), allocatable :: failure
  character(len=:), allocatable :: test_text

  call ensure_mock_clipboard()

  backend = clipboard_backend_name()
  if (backend == "unavailable") error stop "mock clipboard backend should be available"

  failure = ""
  test_text = "fgof-clipboard sprint 02" // new_line("a") // "roundtrip"

  written = set_clipboard_text(test_text)
  if (.not. written%success .and. len(failure) == 0) failure = "set_clipboard_text should succeed when a backend is available: backend=" // written%backend // " error=" // written%error_message
  if (.not. is_known_backend(written%backend) .and. len(failure) == 0) failure = "set_clipboard_text should report a known backend"
  if (written%backend == "unavailable" .and. len(failure) == 0) failure = "set_clipboard_text should use a working backend"

  loaded = get_clipboard_text()
  if (.not. loaded%success .and. len(failure) == 0) failure = "get_clipboard_text should succeed after writing clipboard text: backend=" // loaded%backend // " error=" // loaded%error_message
  if (.not. is_known_backend(loaded%backend) .and. len(failure) == 0) failure = "get_clipboard_text should report a known backend"
  if (loaded%backend == "unavailable" .and. len(failure) == 0) failure = "get_clipboard_text should use a working backend"
  if (loaded%text /= test_text .and. len(failure) == 0) failure = "clipboard roundtrip should preserve exact text"

  if (len(failure) > 0) error stop failure
end program test_clipboard_roundtrip
