module clipboard_test_support
  implicit none
  private

  public :: &
    delete_file_if_exists, &
    is_known_backend, &
    mock_clipboard_enabled, &
    required_test_env, &
    touch_file

contains

  logical function mock_clipboard_enabled() result(enabled)
    character(len=:), allocatable :: test_backend

    test_backend = getenv_text("FGOF_CLIPBOARD_TEST_BACKEND")
    enabled = allocated(test_backend)
    if (enabled) enabled = len(test_backend) > 0
  end function mock_clipboard_enabled

  logical function is_known_backend(name) result(known)
    character(len=*), intent(in) :: name

    known = name == "pbcopy" .or. &
            name == "wl-clipboard" .or. &
            name == "xclip" .or. &
            name == "xsel" .or. &
            name == "unavailable"
  end function is_known_backend

  function required_test_env(name) result(value)
    character(len=*), intent(in) :: name
    character(len=:), allocatable :: value

    value = getenv_text(name)
    if (.not. allocated(value)) error stop "required test environment variable is missing"
    if (len(value) == 0) error stop "required test environment variable is empty"
  end function required_test_env

  subroutine touch_file(path)
    character(len=*), intent(in) :: path
    integer :: unit
    integer :: io

    open(newunit=unit, file=path, status="replace", action="write", iostat=io)
    if (io /= 0) error stop "failed to create sentinel file"
    close(unit, iostat=io)
    if (io /= 0) error stop "failed to close sentinel file"
  end subroutine touch_file

  subroutine delete_file_if_exists(path)
    character(len=*), intent(in) :: path
    logical :: exists
    integer :: unit
    integer :: io

    inquire(file=path, exist=exists)
    if (.not. exists) return

    open(newunit=unit, file=path, status="old", action="readwrite", iostat=io)
    if (io /= 0) error stop "failed to reopen sentinel file for deletion"
    close(unit, status="delete", iostat=io)
    if (io /= 0) error stop "failed to delete sentinel file"
  end subroutine delete_file_if_exists

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

end module clipboard_test_support
