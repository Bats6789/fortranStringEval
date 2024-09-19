program eval
    use :: f90getopt
    implicit none
    character(len=*), parameter :: VERSION = '0.1'
    type(option_s) :: opts(1)
    
    opts(1) = option_s('version', .false., 'v')

    do
        select case (getopt('a:v', opts))
            case (char(0))
                exit
            case ('v')
                print '(A, A3)', 'version ', VERSION
                stop
        end select
    end do

    print '(A)', 'Hello world'
end program eval
