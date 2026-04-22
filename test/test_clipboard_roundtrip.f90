program test_clipboard_roundtrip
  use fgof_clipboard, only : clipboard_backend_name, get_clipboard_text, set_clipboard_text
  use fgof_clipboard_types, only : clipboard_result
  implicit none

  type(clipboard_result) :: original
  type(clipboard_result) :: written
  type(clipboard_result) :: loaded
  type(clipboard_result) :: restored
  character(len=:), allocatable :: backend
  character(len=:), allocatable :: failure
  character(len=:), allocatable :: test_text

  backend = clipboard_backend_name()
  if (backend == "unavailable") stop

  failure = ""
  original = get_clipboard_text()
  test_text = "fgof-clipboard sprint 01" // new_line("a") // "roundtrip"

  written = set_clipboard_text(test_text)
  if (.not. written%success .and. len(failure) == 0) failure = "set_clipboard_text should succeed when a backend is available"
  if (written%backend /= backend .and. len(failure) == 0) failure = "set_clipboard_text should preserve backend identity"

  loaded = get_clipboard_text()
  if (.not. loaded%success .and. len(failure) == 0) failure = "get_clipboard_text should succeed after writing clipboard text"
  if (loaded%backend /= backend .and. len(failure) == 0) failure = "get_clipboard_text should preserve backend identity"
  if (loaded%text /= test_text .and. len(failure) == 0) failure = "clipboard roundtrip should preserve exact text"

  if (original%success) then
    restored = set_clipboard_text(original%text)
    if (.not. restored%success .and. len(failure) == 0) failure = "clipboard restore should succeed after roundtrip"
  else
    restored = set_clipboard_text("")
    if (.not. restored%success .and. len(failure) == 0) failure = "clipboard cleanup should succeed after roundtrip"
  end if

  if (len(failure) > 0) error stop failure
end program test_clipboard_roundtrip
