program main
implicit none 

    integer i, firstNumber, currentNumber, nextNumber, total
    character(len=2196) :: numberList

    open (unit = 7, file = "day1.txt")
    read (7,*) numberList
    close(7)

    total = 0

    do i = 1, 2195
        read(numberList(i:i), '(i1)') currentNumber
        read(numberList(i+1:i+1), '(i1)') nextNumber
        if (i == 1) then
            firstNumber = currentNumber
        endif
        if (currentNumber == nextNumber) then
            total = total + currentNumber
        endif
    enddo
    if (nextNumber == firstNumber) total = total + firstNumber

    print *, total

end program main
