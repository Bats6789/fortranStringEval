program eval
    use :: f90getopt
    implicit none
    character(len=*), parameter :: VERSION = '0.1'
    type(option_s) :: opts(2)
    
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

    print '(A)', 'Hello world'

contains
    subroutine print_help()
        print '(A)', 'TODO: Write help'
    end subroutine print_help
end program eval
