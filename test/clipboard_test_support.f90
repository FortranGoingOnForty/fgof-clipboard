module clipboard_test_support
  use, intrinsic :: iso_c_binding, only : c_char, c_int, c_null_char
  implicit none
  private

  public :: &
    configure_fallback_mock_clipboard, &
    delete_file_if_exists, &
    ensure_mock_clipboard, &
    is_known_backend, &
    mock_clipboard_enabled, &
    required_test_env, &
    touch_file

  interface
    integer(c_int) function c_getpid() bind(C, name="getpid")
      import :: c_int
    end function c_getpid

    integer(c_int) function c_unsetenv(name) bind(C, name="unsetenv")
      import :: c_char, c_int
      character(kind=c_char), intent(in) :: name(*)
    end function c_unsetenv

    integer(c_int) function c_setenv(name, value, overwrite) bind(C, name="setenv")
      import :: c_char, c_int
      character(kind=c_char), intent(in) :: name(*)
      character(kind=c_char), intent(in) :: value(*)
      integer(c_int), value :: overwrite
    end function c_setenv
  end interface

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

  subroutine ensure_mock_clipboard()
    character(len=:), allocatable :: store_path
    character(len=:), allocatable :: fail_copy_path
    character(len=:), allocatable :: fail_paste_path

    call prepare_mock_clipboard()
    store_path = mock_store_path()
    fail_copy_path = mock_fail_copy_path()
    fail_paste_path = mock_fail_paste_path()

    call set_mock_env("FGOF_CLIPBOARD_TEST_BACKEND", "pbcopy")
    call unset_mock_env("FGOF_CLIPBOARD_TEST_BACKENDS")
    call set_mock_env("FGOF_CLIPBOARD_TEST_STORE", store_path)
    call set_mock_env("FGOF_CLIPBOARD_TEST_FAIL_COPY", fail_copy_path)
    call set_mock_env("FGOF_CLIPBOARD_TEST_FAIL_PASTE", fail_paste_path)
    call set_mock_env("FGOF_CLIPBOARD_TEST_PBCOPY_STORE", store_path)
    call set_mock_env("FGOF_CLIPBOARD_TEST_PBCOPY_FAIL_COPY", fail_copy_path)
    call set_mock_env("FGOF_CLIPBOARD_TEST_PBCOPY_FAIL_PASTE", fail_paste_path)
    call unset_mock_env("FGOF_CLIPBOARD_TEST_XCLIP_STORE")
    call unset_mock_env("FGOF_CLIPBOARD_TEST_XCLIP_FAIL_COPY")
    call unset_mock_env("FGOF_CLIPBOARD_TEST_XCLIP_FAIL_PASTE")
  end subroutine ensure_mock_clipboard

  subroutine configure_fallback_mock_clipboard()
    character(len=:), allocatable :: store_path
    character(len=:), allocatable :: fail_copy_path
    character(len=:), allocatable :: fail_paste_path
    character(len=:), allocatable :: preferred_fail_copy_path
    character(len=:), allocatable :: preferred_fail_paste_path

    call prepare_mock_clipboard()
    store_path = mock_store_path()
    fail_copy_path = mock_fail_copy_path()
    fail_paste_path = mock_fail_paste_path()
    preferred_fail_copy_path = mock_preferred_fail_copy_path()
    preferred_fail_paste_path = mock_preferred_fail_paste_path()

    call touch_file(preferred_fail_copy_path)
    call touch_file(preferred_fail_paste_path)

    call unset_mock_env("FGOF_CLIPBOARD_TEST_BACKEND")
    call set_mock_env("FGOF_CLIPBOARD_TEST_BACKENDS", "pbcopy,xclip")
    call set_mock_env("FGOF_CLIPBOARD_TEST_STORE", store_path)
    call set_mock_env("FGOF_CLIPBOARD_TEST_FAIL_COPY", fail_copy_path)
    call set_mock_env("FGOF_CLIPBOARD_TEST_FAIL_PASTE", fail_paste_path)
    call set_mock_env("FGOF_CLIPBOARD_TEST_PBCOPY_STORE", store_path)
    call set_mock_env("FGOF_CLIPBOARD_TEST_PBCOPY_FAIL_COPY", preferred_fail_copy_path)
    call set_mock_env("FGOF_CLIPBOARD_TEST_PBCOPY_FAIL_PASTE", preferred_fail_paste_path)
    call set_mock_env("FGOF_CLIPBOARD_TEST_XCLIP_STORE", store_path)
    call set_mock_env("FGOF_CLIPBOARD_TEST_XCLIP_FAIL_COPY", fail_copy_path)
    call set_mock_env("FGOF_CLIPBOARD_TEST_XCLIP_FAIL_PASTE", fail_paste_path)
  end subroutine configure_fallback_mock_clipboard

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

  subroutine prepare_mock_clipboard()
    character(len=:), allocatable :: mock_dir_path
    integer :: exitstat

    mock_dir_path = mock_dir()
    call execute_command_line("mkdir -p " // mock_dir_path, exitstat=exitstat)
    if (exitstat /= 0) error stop "failed to create mock clipboard directory"

    call delete_file_if_exists(mock_store_path())
    call delete_file_if_exists(mock_fail_copy_path())
    call delete_file_if_exists(mock_fail_paste_path())
    call delete_file_if_exists(mock_preferred_fail_copy_path())
    call delete_file_if_exists(mock_preferred_fail_paste_path())
  end subroutine prepare_mock_clipboard

  subroutine set_mock_env(name, value)
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: value
    integer(c_int) :: status

    status = c_setenv(trim(name) // c_null_char, trim(value) // c_null_char, 1_c_int)
    if (status /= 0_c_int) error stop "failed to set mock clipboard environment variable"
  end subroutine set_mock_env

  subroutine unset_mock_env(name)
    character(len=*), intent(in) :: name
    integer(c_int) :: status

    status = c_unsetenv(trim(name) // c_null_char)
    if (status /= 0_c_int) error stop "failed to unset mock clipboard environment variable"
  end subroutine unset_mock_env

  function mock_dir() result(path)
    character(len=:), allocatable :: path
    character(len=:), allocatable :: cwd
    character(len=32) :: pid_text

    write(pid_text, "(i0)") c_getpid()
    cwd = getenv_text("PWD")
    if (allocated(cwd)) then
      path = cwd // "/build/mock-clipboard-" // trim(pid_text)
    else
      path = "build/mock-clipboard-" // trim(pid_text)
    end if
  end function mock_dir

  function mock_store_path() result(path)
    character(len=:), allocatable :: path

    path = mock_dir() // "/clipboard.txt"
  end function mock_store_path

  function mock_fail_copy_path() result(path)
    character(len=:), allocatable :: path

    path = mock_dir() // "/fail-copy"
  end function mock_fail_copy_path

  function mock_fail_paste_path() result(path)
    character(len=:), allocatable :: path

    path = mock_dir() // "/fail-paste"
  end function mock_fail_paste_path

  function mock_preferred_fail_copy_path() result(path)
    character(len=:), allocatable :: path

    path = mock_dir() // "/preferred-fail-copy"
  end function mock_preferred_fail_copy_path

  function mock_preferred_fail_paste_path() result(path)
    character(len=:), allocatable :: path

    path = mock_dir() // "/preferred-fail-paste"
  end function mock_preferred_fail_paste_path

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
