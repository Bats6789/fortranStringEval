module eval

    private
    public :: tokenizer, token_t, print_tokens

    type token_t
        character :: type  ! e for end, o for operator, v for value, u for unknown
        character :: char
        real :: number
    end type token_t

    type ast_t
        type(token_t) :: token
        type(ast_t), pointer :: left => null()
        type(ast_t), pointer :: right => null()
    end type ast_t

contains

    subroutine print_tokens(tokens)
        type(token_t), intent(in) :: tokens(:)
        
        integer :: count

        count = 1

        do while (tokens(count)%type /= 'e')
           print '(5A, F0.7, A)', &
            '{type=', tokens(count)%type, &
            ',char=', tokens(count)%char, &
            ',number=', tokens(count)%number, '}'
            count = count + 1
        end do
    end subroutine print_tokens

    function str_to_value(str) result(token)
        implicit none
        character(len=*), intent(in) :: str
        type(token_t) :: token

        real :: tmp
        
        read(str, *) tmp
        token%number = tmp
        token%type = 'v'
        token%char = ' '
    end function str_to_value

    function char_to_op(c) result(token)
        implicit none
        character :: c
        type(token_t) :: token

        token%char = c
        token%type = 'o'
        token%number = 0.0
    end function char_to_op

    function tokenizer(n, string, tokens_len, status) result(tokens)
        implicit none

        integer, intent(in) :: n
        character(len=n), intent(in) :: string
        type(token_t), allocatable :: tokens(:)
        integer, optional, intent(out) :: tokens_len
        integer, optional, intent(out) :: status

        character :: c
        character(len=n) :: buf
        integer :: i
        integer :: paren_count
        integer :: token_count
        integer :: state
        logical :: building_number
        real :: tmp

        allocate(tokens(n + 1))

        token_count = 0
        tmp = 0.0
        building_number = .false.
        paren_count = 0
        state = 0
        buf = ''

        do i = 1, n
            c = string(i:i)
            select case (c)
                case ('0':'9', '.')
                    buf = trim(buf)//c
                    building_number = .true.
                case ('(', ')')
                    if (building_number) then
                        token_count = token_count + 1
                        tokens(token_count) = str_to_value(buf)
                        buf = ''
                    end if

                    if (c == '(') then
                        paren_count = paren_count + 1
                    else
                        paren_count = paren_count - 1
                        if (paren_count < 0) then
                            state = -1
                            if (present(status)) then
                                status = state
                            end if
                            return
                        end if
                    end if
                    token_count = token_count + 1
                    tokens(token_count) = char_to_op(c)
                case ('+', '-', '*', '/', '^')
                    if (building_number) then
                        token_count = token_count + 1
                        tokens(token_count) = str_to_value(buf)
                        buf = ''
                        building_number = .false.
                    end if
                    
                    token_count = token_count + 1
                    tokens(token_count) = char_to_op(c)
                    
            end select
        end do

        ! Process number if one was being built
        if (building_number) then
            token_count = token_count + 1
            tokens(token_count) = str_to_value(buf)
        end if

        ! Set ending token
        token_count = token_count + 1
        tokens(token_count)%type = 'e'

        ! Set optional arguments
        if (present(tokens_len)) then
            tokens_len = token_count
        end if

        if (present(status)) then
            status = state
        end if
    end function tokenizer

    function tokens_to_AST(tokens, token_count) result(ast)
        type(token_t), allocatable, intent(in) :: tokens
        type(ast_t) :: ast
        integer, intent(in) :: token_count

        type(token_t) :: left(token_count)
        type(token_t) :: shunt(token_count)
        integer :: left_count
        integer :: shunt_count

        left_count = 0
        shunt_count = 0

        forall (i=1:token_count)
            select case (tokens(i)%type)
                case ('v')
                    left_count = left_count + 1
                    left(left_count) = tokens(i)
                case ('o')
                    if (shunt_count == 0) then
                        shunt_count = shunt_count + 1
                        shunt(shunt_count) = tokens(i)
                    else
                        do while (precedence(tokens(i), shunt(shunt_count)) <= 0 .and. shunt_count > 0)
                            left_count = left_count + 1
                            left(left_count) = shunt(shunt_count)
                            shunt_count = shunt_count - 1
                        end do
                        shunt_count = shunt_count + 1
                        shunt(shunt_count) = tokens(i)
                    end if
            end select
        end forall

    end function tokens_to_AST
end module
