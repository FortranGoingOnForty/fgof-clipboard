module fgof_clipboard
  use fgof_process, only : run, shell
  use fgof_clipboard_types, only : &
    FGOF_CLIPBOARD_ERR_INTERNAL, &
    FGOF_CLIPBOARD_ERR_INVALID_OPTIONS, &
    FGOF_CLIPBOARD_ERR_IO, &
    FGOF_CLIPBOARD_ERR_UNAVAILABLE, &
    FGOF_CLIPBOARD_OK, &
    clipboard_result
  use fgof_process_types, only : FGOF_PROCESS_OK, process_options, process_result
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
    type(process_options) :: options
    type(process_result) :: process_outcome
    character(len=:), allocatable :: backend
    character(len=:), allocatable :: command_line

    result_value = clear_clipboard_result()
    backend = detected_backend()
    result_value%backend = backend

    if (backend == BACKEND_UNAVAILABLE) then
      result_value%error_code = FGOF_CLIPBOARD_ERR_UNAVAILABLE
      result_value%error_message = unavailable_message()
      return
    end if

    options%capture_stdout = .true.
    options%capture_stderr = .true.
    command_line = paste_command_for_backend(backend)
    process_outcome = run(shell(command_line), options)
    if (.not. process_completed_successfully(process_outcome)) then
      call set_process_error(result_value, FGOF_CLIPBOARD_ERR_IO, "clipboard read failed", process_outcome)
      return
    end if

    result_value%success = .true.
    result_value%error_code = FGOF_CLIPBOARD_OK
    result_value%text = process_outcome%stdout
    result_value%error_message = ""
  end function get_clipboard_text

  function set_clipboard_text(text) result(result_value)
    character(len=*), intent(in) :: text
    type(clipboard_result) :: result_value
    type(process_options) :: options
    type(process_result) :: process_outcome
    character(len=:), allocatable :: backend
    character(len=:), allocatable :: command_line

    result_value = clear_clipboard_result()
    backend = detected_backend()
    result_value%backend = backend
    result_value%text = text

    if (backend == BACKEND_UNAVAILABLE) then
      result_value%error_code = FGOF_CLIPBOARD_ERR_UNAVAILABLE
      result_value%error_message = unavailable_message()
      return
    end if

    options%stdin = text
    options%capture_stderr = .true.
    command_line = copy_command_for_backend(backend)
    process_outcome = run(shell(command_line), options)
    if (.not. process_completed_successfully(process_outcome)) then
      call set_process_error(result_value, FGOF_CLIPBOARD_ERR_IO, "clipboard write failed", process_outcome)
      return
    end if

    result_value%success = .true.
    result_value%error_code = FGOF_CLIPBOARD_OK
    result_value%error_message = ""
  end function set_clipboard_text

  function clipboard_backend_name() result(name)
    character(len=:), allocatable :: name

    name = detected_backend()
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

  function detected_backend() result(name)
    character(len=:), allocatable :: name
    character(len=:), allocatable :: display
    character(len=:), allocatable :: wayland_display

    if (command_available("pbcopy") .and. command_available("pbpaste")) then
      name = BACKEND_PBCOPY
      return
    end if

    wayland_display = getenv_text("WAYLAND_DISPLAY")
    if (allocated(wayland_display)) then
      if (command_available("wl-copy") .and. command_available("wl-paste")) then
        name = BACKEND_WL_CLIPBOARD
        return
      end if
    end if

    display = getenv_text("DISPLAY")
    if (allocated(display)) then
      if (command_available("xclip")) then
        name = BACKEND_XCLIP
        return
      end if

      if (command_available("xsel")) then
        name = BACKEND_XSEL
        return
      end if
    end if

    name = BACKEND_UNAVAILABLE
  end function detected_backend

  logical function command_available(command_name) result(found)
    character(len=*), intent(in) :: command_name
    type(process_result) :: process_outcome

    process_outcome = run(shell("command -v " // command_name // " >/dev/null 2>&1"))
    found = process_completed_successfully(process_outcome)
  end function command_available

  function copy_command_for_backend(backend) result(command_line)
    character(len=*), intent(in) :: backend
    character(len=:), allocatable :: command_line

    select case (backend)
    case (BACKEND_PBCOPY)
      command_line = "pbcopy"
    case (BACKEND_WL_CLIPBOARD)
      command_line = "wl-copy"
    case (BACKEND_XCLIP)
      command_line = "xclip -selection clipboard"
    case (BACKEND_XSEL)
      command_line = "xsel --clipboard --input"
    case default
      command_line = ""
    end select
  end function copy_command_for_backend

  function paste_command_for_backend(backend) result(command_line)
    character(len=*), intent(in) :: backend
    character(len=:), allocatable :: command_line

    select case (backend)
    case (BACKEND_PBCOPY)
      command_line = "pbpaste -Prefer txt"
    case (BACKEND_WL_CLIPBOARD)
      command_line = "wl-paste --no-newline"
    case (BACKEND_XCLIP)
      command_line = "xclip -selection clipboard -o"
    case (BACKEND_XSEL)
      command_line = "xsel --clipboard --output"
    case default
      command_line = ""
    end select
  end function paste_command_for_backend

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
