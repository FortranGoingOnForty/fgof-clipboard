module fgof_clipboard
  use fgof_process, only : command, run, shell
  use fgof_clipboard_types, only : &
    FGOF_CLIPBOARD_ERR_INTERNAL, &
    FGOF_CLIPBOARD_ERR_INVALID_OPTIONS, &
    FGOF_CLIPBOARD_ERR_IO, &
    FGOF_CLIPBOARD_ERR_UNAVAILABLE, &
    FGOF_CLIPBOARD_OK, &
    clipboard_result
  use fgof_process_types, only : FGOF_PROCESS_OK, process_command, process_options, process_result
  implicit none
  private

  character(len=*), parameter :: BACKEND_PBCOPY = "pbcopy"
  character(len=*), parameter :: BACKEND_WL_CLIPBOARD = "wl-clipboard"
  character(len=*), parameter :: BACKEND_XCLIP = "xclip"
  character(len=*), parameter :: BACKEND_XSEL = "xsel"
  character(len=*), parameter :: BACKEND_UNAVAILABLE = "unavailable"

  public :: &
    FGOF_CLIPBOARD_ERR_INTERNAL, &
    FGOF_CLIPBOARD_ERR_INVALID_OPTIONS, &
    FGOF_CLIPBOARD_ERR_IO, &
    FGOF_CLIPBOARD_ERR_UNAVAILABLE, &
    FGOF_CLIPBOARD_OK, &
    clear_clipboard_result, &
    clipboard_backend_name, &
    clipboard_error_name, &
    clipboard_result, &
    get_clipboard_text, &
    set_clipboard_text

contains

  function clear_clipboard_result() result(result_value)
    type(clipboard_result) :: result_value

    result_value%success = .false.
    result_value%error_code = FGOF_CLIPBOARD_OK
    result_value%backend = ""
    result_value%text = ""
    result_value%error_message = ""
  end function clear_clipboard_result

  function get_clipboard_text() result(result_value)
    type(clipboard_result) :: result_value
    type(process_command) :: cmd
    type(process_options) :: options
    type(process_result) :: process_outcome
    character(len=len(BACKEND_WL_CLIPBOARD)), allocatable :: backends(:)
    integer :: i

    result_value = clear_clipboard_result()
    backends = candidate_backends()

    if (size(backends) == 0) then
      result_value%backend = BACKEND_UNAVAILABLE
      result_value%error_code = FGOF_CLIPBOARD_ERR_UNAVAILABLE
      result_value%error_message = unavailable_message()
      return
    end if

    do i = 1, size(backends)
      result_value%backend = trim(backends(i))
      if (handle_test_clipboard_get(result_value%backend, result_value)) then
        if (result_value%success) return
        cycle
      end if
      options = process_options()
      options%capture_stdout = .true.
      options%capture_stderr = .true.
      call configure_paste_backend(result_value%backend, cmd, options)
      process_outcome = run(cmd, options)
      if (process_completed_successfully(process_outcome)) then
        result_value%success = .true.
        result_value%error_code = FGOF_CLIPBOARD_OK
        result_value%text = process_outcome%stdout
        result_value%error_message = ""
        return
      end if

      call set_process_error(result_value, FGOF_CLIPBOARD_ERR_IO, "clipboard read failed", process_outcome)
    end do
  end function get_clipboard_text

  function set_clipboard_text(text) result(result_value)
    character(len=*), intent(in) :: text
    type(clipboard_result) :: result_value
    type(process_command) :: cmd
    type(process_options) :: options
    type(process_result) :: process_outcome
    character(len=len(BACKEND_WL_CLIPBOARD)), allocatable :: backends(:)
    integer :: i

    result_value = clear_clipboard_result()
    result_value%text = text
    backends = candidate_backends()

    if (size(backends) == 0) then
      result_value%backend = BACKEND_UNAVAILABLE
      result_value%error_code = FGOF_CLIPBOARD_ERR_UNAVAILABLE
      result_value%error_message = unavailable_message()
      return
    end if

    do i = 1, size(backends)
      result_value%backend = trim(backends(i))
      if (handle_test_clipboard_set(result_value%backend, text, result_value)) then
        if (result_value%success) return
        cycle
      end if
      options = process_options()
      options%stdin = text
      options%capture_stderr = .true.
      call configure_copy_backend(result_value%backend, cmd, options, text)
      process_outcome = run(cmd, options)
      if (process_completed_successfully(process_outcome)) then
        result_value%success = .true.
        result_value%error_code = FGOF_CLIPBOARD_OK
        result_value%error_message = ""
        return
      end if

      call set_process_error(result_value, FGOF_CLIPBOARD_ERR_IO, "clipboard write failed", process_outcome)
    end do
  end function set_clipboard_text

  function clipboard_backend_name() result(name)
    character(len=:), allocatable :: name

    name = preferred_backend_name()
  end function clipboard_backend_name

  function clipboard_error_name(error_code) result(name)
    integer, intent(in) :: error_code
    character(len=:), allocatable :: name

    select case (error_code)
    case (FGOF_CLIPBOARD_OK)
      name = "ok"
    case (FGOF_CLIPBOARD_ERR_INVALID_OPTIONS)
      name = "invalid-options"
    case (FGOF_CLIPBOARD_ERR_UNAVAILABLE)
      name = "unavailable"
    case (FGOF_CLIPBOARD_ERR_IO)
      name = "io"
    case (FGOF_CLIPBOARD_ERR_INTERNAL)
      name = "internal"
    case default
      name = "unknown"
    end select
  end function clipboard_error_name

  function preferred_backend_name() result(name)
    character(len=:), allocatable :: name
    character(len=len(BACKEND_WL_CLIPBOARD)), allocatable :: backends(:)

    backends = candidate_backends()
    if (size(backends) == 0) then
      name = BACKEND_UNAVAILABLE
    else
      name = trim(backends(1))
    end if
  end function preferred_backend_name

  function candidate_backends() result(backends)
    character(len=len(BACKEND_WL_CLIPBOARD)), allocatable :: backends(:)
    character(len=:), allocatable :: display
    character(len=:), allocatable :: test_backend
    character(len=:), allocatable :: test_backends
    character(len=:), allocatable :: wayland_display
    character(len=:), allocatable :: xdg_session_type

    allocate(backends(0))

    test_backends = getenv_text("FGOF_CLIPBOARD_TEST_BACKENDS")
    if (allocated(test_backends)) then
      call append_test_backends(backends, test_backends)
      if (size(backends) > 0) return
    end if

    test_backend = getenv_text("FGOF_CLIPBOARD_TEST_BACKEND")
    if (valid_backend_name(test_backend)) then
      call append_backend(backends, test_backend)
      return
    end if

    if (command_available("pbcopy") .and. command_available("pbpaste")) then
      call append_backend(backends, BACKEND_PBCOPY)
    end if

    wayland_display = getenv_text("WAYLAND_DISPLAY")
    xdg_session_type = getenv_text("XDG_SESSION_TYPE")
    if (allocated(wayland_display) .or. text_equals(xdg_session_type, "wayland")) then
      if (command_available("wl-copy") .and. command_available("wl-paste")) then
        call append_backend(backends, BACKEND_WL_CLIPBOARD)
      end if
    end if

    display = getenv_text("DISPLAY")
    if (allocated(display) .or. text_equals(xdg_session_type, "x11")) then
      if (command_available("xclip")) call append_backend(backends, BACKEND_XCLIP)
      if (command_available("xsel")) call append_backend(backends, BACKEND_XSEL)
    end if
  end function candidate_backends

  logical function valid_backend_name(name) result(valid)
    character(len=:), allocatable, intent(in) :: name

    valid = .false.
    if (.not. allocated(name)) return

    valid = name == BACKEND_PBCOPY .or. &
            name == BACKEND_WL_CLIPBOARD .or. &
            name == BACKEND_XCLIP .or. &
            name == BACKEND_XSEL
  end function valid_backend_name

  subroutine append_test_backends(backends, backend_names)
    character(len=len(BACKEND_WL_CLIPBOARD)), allocatable, intent(inout) :: backends(:)
    character(len=*), intent(in) :: backend_names
    character(len=:), allocatable :: entry
    integer :: cursor
    integer :: next_comma

    cursor = 1
    do while (cursor <= len(backend_names))
      next_comma = index(backend_names(cursor:), ",")
      if (next_comma == 0) then
        entry = trim(adjustl(backend_names(cursor:)))
        cursor = len(backend_names) + 1
      else
        entry = trim(adjustl(backend_names(cursor:cursor + next_comma - 2)))
        cursor = cursor + next_comma
      end if

      if (len(entry) == 0) cycle
      if (.not. valid_backend_name(entry)) cycle
      call append_backend(backends, entry)
    end do
  end subroutine append_test_backends

  subroutine append_backend(backends, backend)
    character(len=len(BACKEND_WL_CLIPBOARD)), allocatable, intent(inout) :: backends(:)
    character(len=*), intent(in) :: backend
    character(len=len(BACKEND_WL_CLIPBOARD)), allocatable :: grown(:)
    integer :: count

    if (backend_already_listed(backends, backend)) return

    count = size(backends)
    allocate(grown(count + 1))
    if (count > 0) grown(:count) = backends
    grown(count + 1) = backend
    call move_alloc(grown, backends)
  end subroutine append_backend

  logical function backend_already_listed(backends, backend) result(found)
    character(len=len(BACKEND_WL_CLIPBOARD)), intent(in) :: backends(:)
    character(len=*), intent(in) :: backend
    integer :: i

    found = .false.
    do i = 1, size(backends)
      if (trim(backends(i)) == backend) then
        found = .true.
        return
      end if
    end do
  end function backend_already_listed

  logical function text_equals(value, expected) result(matches)
    character(len=:), allocatable, intent(in) :: value
    character(len=*), intent(in) :: expected

    matches = allocated(value)
    if (matches) matches = value == expected
  end function text_equals

  logical function command_available(command_name) result(found)
    character(len=*), intent(in) :: command_name
    type(process_result) :: process_outcome

    process_outcome = run(shell("command -v " // command_name // " >/dev/null 2>&1"))
    found = process_completed_successfully(process_outcome)
  end function command_available

  subroutine configure_copy_backend(backend, cmd, options, text_value)
    character(len=*), intent(in) :: backend
    type(process_command), intent(out) :: cmd
    type(process_options), intent(inout) :: options
    character(len=*), intent(in) :: text_value

    select case (backend)
    case (BACKEND_PBCOPY)
      cmd = command("pbcopy")
    case (BACKEND_WL_CLIPBOARD)
      cmd = command("wl-copy")
    case (BACKEND_XCLIP)
      cmd = command("xclip", [character(len=10) :: "-selection", "clipboard"])
    case (BACKEND_XSEL)
      cmd = command("xsel", [character(len=11) :: "--clipboard", "--input"])
    case default
      cmd = command("")
    end select
  end subroutine configure_copy_backend

  subroutine configure_paste_backend(backend, cmd, options)
    character(len=*), intent(in) :: backend
    type(process_command), intent(out) :: cmd
    type(process_options), intent(inout) :: options

    select case (backend)
    case (BACKEND_PBCOPY)
      cmd = command("pbpaste", [character(len=7) :: "-Prefer", "txt"])
    case (BACKEND_WL_CLIPBOARD)
      cmd = command("wl-paste", [character(len=12) :: "--no-newline"])
    case (BACKEND_XCLIP)
      cmd = command("xclip", [character(len=10) :: "-selection", "clipboard", "-o"])
    case (BACKEND_XSEL)
      cmd = command("xsel", [character(len=11) :: "--clipboard", "--output"])
    case default
      cmd = command("")
    end select
  end subroutine configure_paste_backend

  function test_env_value(backend, variable_name) result(value)
    character(len=*), intent(in) :: backend
    character(len=*), intent(in) :: variable_name
    character(len=:), allocatable :: value
    character(len=:), allocatable :: backend_specific_name

    backend_specific_name = backend_override_variable_name(backend, variable_name)
    if (allocated(backend_specific_name)) then
      value = getenv_text(backend_specific_name)
      if (allocated(value)) return
    end if

    value = getenv_text("FGOF_CLIPBOARD_TEST_" // variable_name)
  end function test_env_value

  function backend_override_variable_name(backend, variable_name) result(name)
    character(len=*), intent(in) :: backend
    character(len=*), intent(in) :: variable_name
    character(len=:), allocatable :: name
    character(len=:), allocatable :: suffix

    suffix = backend_variable_suffix(backend)
    if (.not. allocated(suffix)) return

    name = "FGOF_CLIPBOARD_TEST_" // suffix // "_" // variable_name
  end function backend_override_variable_name

  function backend_variable_suffix(backend) result(suffix)
    character(len=*), intent(in) :: backend
    character(len=:), allocatable :: suffix

    select case (backend)
    case (BACKEND_PBCOPY)
      suffix = "PBCOPY"
    case (BACKEND_WL_CLIPBOARD)
      suffix = "WL_CLIPBOARD"
    case (BACKEND_XCLIP)
      suffix = "XCLIP"
    case (BACKEND_XSEL)
      suffix = "XSEL"
    end select
  end function backend_variable_suffix

  logical function process_completed_successfully(process_outcome) result(ok)
    type(process_result), intent(in) :: process_outcome

    ok = process_outcome%error_code == FGOF_PROCESS_OK .and. &
         process_outcome%completed .and. &
         process_outcome%exited_normally .and. &
         process_outcome%exit_code == 0
  end function process_completed_successfully

  subroutine set_process_error(result_value, error_code, context, process_outcome)
    type(clipboard_result), intent(inout) :: result_value
    integer, intent(in) :: error_code
    character(len=*), intent(in) :: context
    type(process_result), intent(in) :: process_outcome

    result_value%success = .false.
    result_value%error_code = error_code
    result_value%error_message = process_failure_message(context, process_outcome)
  end subroutine set_process_error

  function process_failure_message(context, process_outcome) result(message)
    character(len=*), intent(in) :: context
    type(process_result), intent(in) :: process_outcome
    character(len=:), allocatable :: message
    character(len=32) :: exit_text

    if (process_outcome%error_code /= FGOF_PROCESS_OK) then
      if (allocated(process_outcome%error_message)) then
        if (len(process_outcome%error_message) > 0) then
          message = context // ": " // process_outcome%error_message
          return
        end if
      end if
      message = context // ": process launch failed"
      return
    end if

    write(exit_text, "(i0)") process_outcome%exit_code
    message = context // " (exit=" // trim(exit_text) // ")"
    if (allocated(process_outcome%stderr)) then
      if (len(process_outcome%stderr) > 0) message = message // ": " // process_outcome%stderr
    end if
  end function process_failure_message

  function unavailable_message() result(message)
    character(len=:), allocatable :: message

    message = "no supported clipboard backend is available"
  end function unavailable_message

  logical function handle_test_clipboard_get(backend, result_value) result(handled)
    character(len=*), intent(in) :: backend
    type(clipboard_result), intent(inout) :: result_value
    character(len=:), allocatable :: store_path
    character(len=:), allocatable :: fail_path

    handled = .false.
    store_path = test_env_value(backend, "STORE")
    if (.not. allocated(store_path)) return

    handled = .true.
    fail_path = test_env_value(backend, "FAIL_PASTE")
    if (path_exists(fail_path)) then
      result_value%success = .false.
      result_value%error_code = FGOF_CLIPBOARD_ERR_IO
      result_value%error_message = "clipboard read failed: mock clipboard paste failure"
      return
    end if

    result_value%success = .true.
    result_value%error_code = FGOF_CLIPBOARD_OK
    result_value%text = read_text_file(store_path)
    result_value%error_message = ""
  end function handle_test_clipboard_get

  logical function handle_test_clipboard_set(backend, text, result_value) result(handled)
    character(len=*), intent(in) :: backend
    character(len=*), intent(in) :: text
    type(clipboard_result), intent(inout) :: result_value
    character(len=:), allocatable :: store_path
    character(len=:), allocatable :: fail_path

    handled = .false.
    store_path = test_env_value(backend, "STORE")
    if (.not. allocated(store_path)) return

    handled = .true.
    fail_path = test_env_value(backend, "FAIL_COPY")
    if (path_exists(fail_path)) then
      result_value%success = .false.
      result_value%error_code = FGOF_CLIPBOARD_ERR_IO
      result_value%error_message = "clipboard write failed: mock clipboard copy failure"
      return
    end if

    if (.not. write_text_file(store_path, text)) then
      result_value%success = .false.
      result_value%error_code = FGOF_CLIPBOARD_ERR_IO
      result_value%error_message = "clipboard write failed: mock clipboard store error"
      return
    end if

    result_value%success = .true.
    result_value%error_code = FGOF_CLIPBOARD_OK
    result_value%error_message = ""
  end function handle_test_clipboard_set

  logical function path_exists(path) result(exists)
    character(len=:), allocatable, intent(in) :: path

    exists = .false.
    if (.not. allocated(path)) return
    if (len(path) == 0) return
    inquire(file=path, exist=exists)
  end function path_exists

  logical function write_text_file(path, text) result(ok)
    character(len=*), intent(in) :: path
    character(len=*), intent(in) :: text
    integer :: unit
    integer :: io

    ok = .false.
    open(newunit=unit, file=path, status="replace", access="stream", form="unformatted", action="write", iostat=io)
    if (io /= 0) return

    if (len(text) > 0) then
      write(unit, iostat=io) text
    else
      io = 0
    end if
    close(unit, iostat=io)
    if (io /= 0) return

    ok = .true.
  end function write_text_file

  function read_text_file(path) result(text)
    character(len=*), intent(in) :: path
    character(len=:), allocatable :: text
    integer :: unit
    integer :: io
    integer :: file_size

    open(newunit=unit, file=path, status="old", access="stream", form="unformatted", action="read", iostat=io)
    if (io /= 0) then
      text = ""
      return
    end if

    inquire(unit=unit, size=file_size)
    if (file_size <= 0) then
      text = ""
      close(unit, iostat=io)
      return
    end if

    allocate(character(len=file_size) :: text)
    read(unit, iostat=io) text
    if (io /= 0) text = ""
    close(unit, iostat=io)
  end function read_text_file

  function getenv_text(name) result(value)
    character(len=*), intent(in) :: name
    character(len=:), allocatable :: value
    integer :: length
    integer :: status

    call get_environment_variable(name, length=length, status=status)
    if (status /= 0 .or. length <= 0) return

    allocate(character(len=length) :: value)
    call get_environment_variable(name, value, status=status)
    if (status /= 0) deallocate(value)
  end function getenv_text

end module fgof_clipboard
