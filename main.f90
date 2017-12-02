program main
implicit none 

    ! call dayOnePartOne
    ! call dayOnePartTwo

    call dayTwoPartOne

end program main


! ----------------------------------------------------------
subroutine dayTwoPartOne
    integer checksum, numberOfColumns, numberOfRows, maxElement, minElement, rowDifference
    integer, allocatable :: row(:)
    
    numberOfColumns = 16
    numberOfRows = 16
    checksum = 0

    open(7, file='dayTwo.txt')
    allocate(row(numberOfColumns))
    do j = 1, numberOfRows
        read(7,*) row
        maxElement = row(1)
        minElement = row(1)
        do i = 2, numberOfColumns
            if (row(i) > maxElement) maxElement = row(i)
            if (row(i) < minElement) minElement = row(i)
        enddo
        rowDifference = maxElement - minElement
        checksum = checksum + rowDifference   
    enddo
    deallocate(row)
    close(7)
    

    call printResult('Two', 3, '2', 1, checksum)
    return
end subroutine dayTwoPartOne


! ----------------------------------------------------------
subroutine dayOnePartTwo
    integer i, j, fileLength, currentNumber, middleNumber, total
    character(len=2196) :: numberList

    fileLength = 2196

    open (unit = 7, file = "dayOne.txt")
    read (7,*) numberList
    close(7)

    total = 0
    j = (fileLength / 2) + 1

    do i = 1, fileLength
        if (j == fileLength+1) then
            j = 1
        endif
        read(numberList(i:i), '(i1)') currentNumber
        read(numberList(j:j), '(i1)') middleNumber
        if (currentNumber == middleNumber) then
            total = total + currentNumber
        endif
        j = j+1
    enddo

    call printResult('One', 3, '2', 1, total)
    return
end subroutine dayOnePartTwo


! ----------------------------------------------------------
subroutine dayOnePartOne
    integer i, fileLength, firstNumber, currentNumber, nextNumber, total
    character(len=2196) :: numberList

    fileLength = 2196

    open (unit = 7, file = "dayOne.txt")
    read (7,*) numberList
    close(7)

    total = 0

    do i = 1, fileLength-1
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

    call printResult('One', 3, '1', 1, total)
    return
end subroutine dayOnePartOne


! ----------------------------------------------------------
subroutine printResult (day, dayLength, part, partLength, result)
    integer dayLength, partLength, result
    character(len=dayLength) day
    character(len=partLength) part
    
    print *, '-----------------------------------'
    print *, 'Day ', day, ', Part ', part, ' Result: '
    print *, '-----------------------------------'
    print *, result
    return
end subroutine printResult
