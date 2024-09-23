program main
    use :: f90getopt
    use :: eval
    implicit none
    character(len=*), parameter :: VERSION = '0.1'
    character(len=:), allocatable :: string
    type(option_s) :: opts(2)
    integer :: arg_len
    type(token_t), allocatable :: tokens(:)
    
    opts(1) = option_s('version', .false., 'v')
    opts(2) = option_s('help', .false., 'h')

    do
        select case (getopt('hv', opts))
            case (char(0))
                exit
            case ('h')
                call print_help()
                stop
            case ('v')
                print '(A, A3)', 'version ', VERSION
                stop
            case ('?')
                stop
        end select
    end do

    if (optind > command_argument_count()) then
        print '(A)', 'ERROR: No string provided for evaluation'
        stop
    end if

    call get_command_argument(optind, length=arg_len)
    allocate(character(len=arg_len) :: string)
    allocate(tokens(arg_len))
    call get_command_argument(optind, string)

    print '(A)', string
    tokens = tokenizer(arg_len, string)

    call print_tokens(tokens)

contains
    subroutine print_help()
        print '(A)', 'TODO: Write help'
    end subroutine print_help
end program main
