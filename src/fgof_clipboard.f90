module fgof_clipboard
  use fgof_clipboard_types, only : &
    FGOF_CLIPBOARD_ERR_INTERNAL, &
    FGOF_CLIPBOARD_ERR_INVALID_OPTIONS, &
    FGOF_CLIPBOARD_ERR_IO, &
    FGOF_CLIPBOARD_ERR_UNAVAILABLE, &
    FGOF_CLIPBOARD_OK, &
    clipboard_result
  implicit none
  private

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
    result_value%text = ""
    result_value%error_message = ""
  end function clear_clipboard_result

  function get_clipboard_text() result(result_value)
    type(clipboard_result) :: result_value

    result_value = clear_clipboard_result()
    result_value%error_code = FGOF_CLIPBOARD_ERR_UNAVAILABLE
    result_value%error_message = "clipboard backend is not implemented yet"
  end function get_clipboard_text

  function set_clipboard_text(text) result(result_value)
    character(len=*), intent(in) :: text
    type(clipboard_result) :: result_value

    result_value = clear_clipboard_result()
    result_value%text = text
    result_value%error_code = FGOF_CLIPBOARD_ERR_UNAVAILABLE
    result_value%error_message = "clipboard backend is not implemented yet"
  end function set_clipboard_text

  function clipboard_backend_name() result(name)
    character(len=:), allocatable :: name

    name = "scaffold"
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

end module fgof_clipboard
