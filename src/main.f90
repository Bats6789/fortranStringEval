program main
    use :: f90getopt
    use :: eval
    implicit none
    character(len=*), parameter :: VERSION = '0.1'
    character(len=:), allocatable :: string
    type(option_s) :: opts(2)
    type(ast_t) :: ast
    type(token_t), allocatable :: tokens(:)
    integer :: arg_len
    integer :: tokens_len
    integer :: status
    
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

    ! print '(A)', string
    tokens = tokenizer(arg_len, string=string, tokens_len=tokens_len, status=status)

    if (status /= 0) then
        print *, 'ERROR: Something went wrong'
        stop
    end if

    ! call print_tokens(tokens)

    ! print *, ' '
    ast = tokens_to_AST(tokens, tokens_len)

contains
    subroutine print_help()
        print '(A)', 'TODO: Write help'
    end subroutine print_help
end program main
