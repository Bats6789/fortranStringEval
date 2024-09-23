module eval

    private
    public :: tokenizer, token_t, print_tokens, tokens_to_AST, ast_t

    type token_t
        character :: type  ! e for end, o for operator, v for value, u for unknown
        character :: op
        real :: number
    end type token_t

    type ast_t
        type(token_t) :: token
        type(ast_t), pointer :: left => null()
        type(ast_t), pointer :: right => null()
    end type ast_t

contains

    subroutine print_tokens(tokens, n)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in), optional :: n

        integer :: count

        if (present(n)) then
            do count = 1, n
                print '(5A, F0.7, A)', &
                    '{type=', tokens(count)%type, &
                    ',op=', tokens(count)%op, &
                    ',number=', tokens(count)%number, '}'
            end do
        else
            count = 1

            do while (tokens(count)%type /= 'e')
                print '(5A, F0.7, A)', &
                    '{type=', tokens(count)%type, &
                    ',op=', tokens(count)%op, &
                    ',number=', tokens(count)%number, '}'
                count = count + 1
            end do
        end if
    end subroutine print_tokens

    function eval_op(op, a, b) result(val)
        implicit none

        character, intent(in) :: op
        real, intent(in) :: a
        real, intent(in) :: b
        real :: val

        select case (op)
            case ('+')
                val = a + b
            case ('-')
                val = a - b
            case ('*')
                val = a * b
            case ('/')
                val = a / b
            case ('^')
                val = a**b
        end select
    end function

    function str_to_value(str) result(token)
        implicit none
        character(len=*), intent(in) :: str
        type(token_t) :: token

        real :: tmp

        read (str, *) tmp
        token%number = tmp
        token%type = 'v'
        token%op = ' '
    end function str_to_value

    function char_to_op(c) result(token)
        implicit none
        character :: c
        type(token_t) :: token

        token%op = c
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

        allocate (tokens(n + 1))

        token_count = 0
        tmp = 0.0
        building_number = .false.
        paren_count = 0
        state = 0
        buf = ''
        i = 0

        ! print *, n

        do i = 1, n, 1
            c = string(i:i)
            select case (c)
            case ('0':'9', '.')
                buf = trim(buf)//c
                building_number = .true.
            case ('(', ')')
                if (i > n) then
                    exit
                end if
                if (building_number) then
                    token_count = token_count + 1
                    tokens(token_count) = str_to_value(buf)
                    buf = ''
                    building_number = .false.
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

    function precedence_map(op) result(map)
        implicit none
        character, intent(in) :: op
        integer :: map

        select case (op)
        case ('+', '-')
            map = 1
        case ('*', '/')
            map = 2
        case ('^')
            map = 3
        case ('(', ')')
            map = 4
        case default
            map = -1
        end select
    end function precedence_map

    function precedence(left, right) result(compare)
        implicit none
        type(token_t), intent(in) :: left
        type(token_t), intent(in) :: right
        integer :: compare

        compare = precedence_map(left%op) - precedence_map(right%op)

        if (left%op == '(' .or. right%op == '(') then
            compare = 1
        end if
    end function precedence

    function tokens_to_AST(tokens, token_count) result(ast)
        implicit none
        type(token_t), intent(in) :: tokens(token_count)
        type(ast_t) :: ast
        integer, intent(in) :: token_count

        type(token_t) :: left(token_count)
        type(token_t) :: shunt(token_count)
        type(token_t) :: stack(token_count)
        integer :: left_count
        integer :: shunt_count
        integer :: i
        integer :: stack_count
        real :: result

        left_count = 0
        shunt_count = 0
        stack_count = 0
        result = 0.0

        do i = 1, token_count
            select case (tokens(i)%type)
            case ('v')
                left_count = left_count + 1
                left(left_count) = tokens(i)
            case ('o')
                if (shunt_count == 0) then
                    shunt_count = shunt_count + 1
                    shunt(shunt_count) = tokens(i)
                else if (tokens(i)%op == ')') then
                    do while (shunt(shunt_count)%op /= '(')
                        left_count = left_count + 1
                        left(left_count) = shunt(shunt_count)
                        shunt_count = shunt_count - 1
                    end do
                    shunt_count = shunt_count - 1
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
        end do

        do while (shunt_count > 0)
            left_count = left_count + 1
            left(left_count) = shunt(shunt_count)
            shunt_count = shunt_count - 1
        end do

        do i = 1, left_count
            stack_count = stack_count + 1;
            stack(stack_count) = left(i)

            if (stack(stack_count)%type == 'o') then
                result = eval_op(stack(stack_count)%op, stack(stack_count - 2)%number, stack(stack_count - 1)%number)
                stack_count = stack_count - 2
                stack(stack_count)%type = 'v'
                stack(stack_count)%number = result
                stack(stack_count)%op = ''
            end if
        end do

        result = stack(stack_count)%number
        print '(F0.7)', result

    end function tokens_to_AST
end module
