module fgof_clipboard_types
  implicit none
  private

  integer, parameter, public :: FGOF_CLIPBOARD_OK = 0
  integer, parameter, public :: FGOF_CLIPBOARD_ERR_INVALID_OPTIONS = 10
  integer, parameter, public :: FGOF_CLIPBOARD_ERR_UNAVAILABLE = 20
  integer, parameter, public :: FGOF_CLIPBOARD_ERR_IO = 30
  integer, parameter, public :: FGOF_CLIPBOARD_ERR_INTERNAL = 99

  type, public :: clipboard_result
    logical :: success = .false.
    integer :: error_code = FGOF_CLIPBOARD_OK
    character(len=:), allocatable :: backend
    character(len=:), allocatable :: text
    character(len=:), allocatable :: error_message
  end type clipboard_result

end module fgof_clipboard_types
