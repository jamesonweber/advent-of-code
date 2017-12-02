program main
implicit none 

    integer i, firstNumber, currentNumber, nextNumber, total
    character(len=4) :: numberList

    numberList = '1111'
    total = 0

    do i = 1, 3
        if (i == 1) firstNumber = currentNumber
        read(numberList(i:i), '(i1)') currentNumber
        read(numberList(i+1:i+1), '(i1)') nextNumber
        if ( currentNumber == nextNumber ) total = total + currentNumber
    enddo
    if (nextNumber == firstNumber) total = total + firstNumber

    print *, total

end program main
