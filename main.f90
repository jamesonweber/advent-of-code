program main
implicit none 

    ! call dayOnePartOne
    ! call dayOnePartTwo

    ! call dayTwoPartOne
    ! call dayTwoPartTwo

    ! call dayThreePartOne
    ! call dayThreePartTwo

    call dayFivePartOne

end program main


! ----------------------------------------------------------
subroutine dayFivePartOne
    integer numberOfRows, move, index, stepCounter
    integer, dimension(1059) :: steps

    numberOfRows = 1059
    stepCounter = 0

    open(7, file='day5.txt')
    do j = 1, numberOfRows
        read(7,*) steps(j)
    end do
    close(7)

    index = 0
    move = 1
    currentStep = 1
    do while (index < numberOfRows)
        
        move = steps(index+1)
        steps(index+1) = steps(index+1) + 1
        index = move + index

        !print *, steps
        !print *, index

        stepCounter = stepCounter + 1
    enddo

    print *, stepCounter


end subroutine dayFivePartOne


! ----------------------------------------------------------
subroutine dayThreePartTwo
    integer distance, matrixLength, matrixSize, x, y, originalX, originalY, spiralNumber, spiralNumberPath
    character currentMoveDirection
    integer, allocatable :: matrix(:,:)

    ! 277678
    spiralNumber = 277678
    spiralNumberPath = 1
    distance = 0
    matrixLength = ceiling(sqrt(spiralNumber * 1.0))
    matrixSize = matrixLength * matrixLength
    currentMoveDirection = 'D'

    if (mod(matrixLength, 2) == 0) then
        x = (matrixLength / 2) + 1
        y = matrixLength / 2
    else
        x = ceiling(matrixLength / 2.0)
        y = ceiling(matrixLength / 2.0)
    endif

    originalX = x
    originalY = y

    ! Well, this is going to use a stupid amount more memory than in needs.
    ! But hey, requirements dicate finding the fastest solution is more important
    ! than resource usage. Something something space–time trade off - given more  
    ! space, computation problems can be solved faster, so this is fine /s.
    allocate(matrix(matrixLength, matrixLength))

        do l = 1, matrixLength
            do m = 1, matrixLength
                matrix(l, m) = 0
            enddo
        enddo

        matrix(x,y) = spiralNumberPath
        currentMoveDirection = 'R'
        do while (spiralNumberPath < spiralNumber)
            if (currentMoveDirection == 'R') then
                if (matrix(x,y+1) == 0) then
                    y = y + 1
                    call sumOfAdjacentSquares( &
                        matrix(x,y+1), &
                        matrix(x,y-1), &
                        matrix(x+1,y), &
                        matrix(x+1,y-1), & 
                        matrix(x+1,y+1), &
                        matrix(x-1,y), &
                        matrix(x-1,y-1), & 
                        matrix(x-1,y+1), &
                        spiralNumberPath)
                    matrix(x,y) = spiralNumberPath
                    currentMoveDirection = 'U'
                else 
                    x = x + 1
                    call sumOfAdjacentSquares( &
                        matrix(x,y+1), &
                        matrix(x,y-1), &
                        matrix(x+1,y), &
                        matrix(x+1,y-1), & 
                        matrix(x+1,y+1), &
                        matrix(x-1,y), &
                        matrix(x-1,y-1), & 
                        matrix(x-1,y+1), &
                        spiralNumberPath)
                    matrix(x,y) = spiralNumberPath
                endif

            else if (currentMoveDirection == 'U') then
                if (matrix(x-1,y) == 0) then
                    x = x - 1
                    call sumOfAdjacentSquares( &
                        matrix(x,y+1), &
                        matrix(x,y-1), &
                        matrix(x+1,y), &
                        matrix(x+1,y-1), & 
                        matrix(x+1,y+1), &
                        matrix(x-1,y), &
                        matrix(x-1,y-1), & 
                        matrix(x-1,y+1), &
                        spiralNumberPath)
                    matrix(x,y) = spiralNumberPath
                    currentMoveDirection = 'L'
                else 
                    y = y + 1
                    call sumOfAdjacentSquares( &
                        matrix(x,y+1), &
                        matrix(x,y-1), &
                        matrix(x+1,y), &
                        matrix(x+1,y-1), & 
                        matrix(x+1,y+1), &
                        matrix(x-1,y), &
                        matrix(x-1,y-1), & 
                        matrix(x-1,y+1), &
                        spiralNumberPath)
                    matrix(x,y) = spiralNumberPath
                endif

            else if (currentMoveDirection == 'L') then
                if (matrix(x,y-1) == 0) then
                    y = y - 1
                    call sumOfAdjacentSquares( &
                        matrix(x,y+1), &
                        matrix(x,y-1), &
                        matrix(x+1,y), &
                        matrix(x+1,y-1), & 
                        matrix(x+1,y+1), &
                        matrix(x-1,y), &
                        matrix(x-1,y-1), & 
                        matrix(x-1,y+1), &
                        spiralNumberPath)
                    currentMoveDirection = 'D'
                    matrix(x,y) = spiralNumberPath
                else 
                    x = x - 1
                    call sumOfAdjacentSquares( &
                        matrix(x,y+1), &
                        matrix(x,y-1), &
                        matrix(x+1,y), &
                        matrix(x+1,y-1), & 
                        matrix(x+1,y+1), &
                        matrix(x-1,y), &
                        matrix(x-1,y-1), & 
                        matrix(x-1,y+1), &
                        spiralNumberPath)
                    matrix(x,y) = spiralNumberPath
                endif

            else if (currentMoveDirection == 'D') then
                if (matrix(x+1,y) == 0) then
                    x = x + 1
                    call sumOfAdjacentSquares( &
                        matrix(x,y+1), &
                        matrix(x,y-1), &
                        matrix(x+1,y), &
                        matrix(x+1,y-1), & 
                        matrix(x+1,y+1), &
                        matrix(x-1,y), &
                        matrix(x-1,y-1), & 
                        matrix(x-1,y+1), &
                        spiralNumberPath)
                    currentMoveDirection = 'R'
                    matrix(x,y) = spiralNumberPath
                else 
                    y = y - 1
                    call sumOfAdjacentSquares( &
                        matrix(x,y+1), &
                        matrix(x,y-1), &
                        matrix(x+1,y), &
                        matrix(x+1,y-1), & 
                        matrix(x+1,y+1), &
                        matrix(x-1,y), &
                        matrix(x-1,y-1), & 
                        matrix(x-1,y+1), &
                        spiralNumberPath)
                    matrix(x,y) = spiralNumberPath
                endif

            endif
        enddo

        call printResult('Three', 5, '2', 1, matrix(x,y))
        
        ! do i = 1, matrixLength
        !     print *, matrix(i, :)      
        ! enddo 
    deallocate(matrix)

end subroutine dayThreePartTwo


! ----------------------------------------------------------
subroutine sumOfAdjacentSquares(c1, c2, c3, c4, c5, c6, c7, c8, result)
    integer c1, c2, c3, c4, c5, c6, c7, c8, result
    result = c1 + c2 + c3 + c4 + c5 + c6 + c7 + c8
    return
end subroutine sumOfAdjacentSquares


! ----------------------------------------------------------
subroutine dayThreePartOne
    integer distance, matrixLength, matrixSize, x, y, originalX, originalY, spiralNumber, spiralNumberPath
    character currentMoveDirection
    integer, allocatable :: matrix(:,:)

    ! 277678
    spiralNumber = 277678
    spiralNumberPath = 1
    distance = 0
    matrixLength = ceiling(sqrt(spiralNumber * 1.0))
    matrixSize = matrixLength * matrixLength
    currentMoveDirection = 'D'

    if (mod(matrixLength, 2) == 0) then
        x = (matrixLength / 2) + 1
        y = matrixLength / 2
    else
        x = ceiling(matrixLength / 2.0)
        y = ceiling(matrixLength / 2.0)
    endif

    originalX = x
    originalY = y

    allocate(matrix(matrixLength, matrixLength))

        do l = 1, matrixLength
            do m = 1, matrixLength
                matrix(l, m) = 0
            enddo
        enddo

        matrix(x,y) = spiralNumberPath
        currentMoveDirection = 'R'
        spiralNumberPath = spiralNumberPath + 1
        do j = 1, spiralNumber-1
            if (currentMoveDirection == 'R') then
                if (matrix(x,y+1) == 0) then
                    y = y + 1
                    matrix(x,y) = spiralNumberPath
                    spiralNumberPath = spiralNumberPath + 1
                    currentMoveDirection = 'U'
                else 
                    x = x + 1
                    matrix(x,y) = spiralNumberPath
                    spiralNumberPath = spiralNumberPath + 1
                endif

            else if (currentMoveDirection == 'U') then
                if (matrix(x-1,y) == 0) then
                    x = x - 1
                    matrix(x,y) = spiralNumberPath
                    spiralNumberPath = spiralNumberPath + 1
                    currentMoveDirection = 'L'
                else 
                    y = y + 1
                    matrix(x,y) = spiralNumberPath
                    spiralNumberPath = spiralNumberPath + 1
                endif

            else if (currentMoveDirection == 'L') then
                if (matrix(x,y-1) == 0) then
                    y = y - 1
                    matrix(x,y) = spiralNumberPath
                    spiralNumberPath = spiralNumberPath + 1
                    currentMoveDirection = 'D'
                else 
                    x = x - 1
                    matrix(x,y) = spiralNumberPath
                    spiralNumberPath = spiralNumberPath + 1
                endif

            else if (currentMoveDirection == 'D') then
                if (matrix(x+1,y) == 0) then
                    x = x + 1
                    matrix(x,y) = spiralNumberPath
                    spiralNumberPath = spiralNumberPath + 1
                    currentMoveDirection = 'R'
                else 
                    y = y - 1
                    matrix(x,y) = spiralNumberPath
                    spiralNumberPath = spiralNumberPath + 1
                endif

            endif
        enddo

        distance = (abs(x - originalX) + abs(y - originalY))
        call printResult('Three', 5, '1', 1, distance)
        
        ! do i = 1, matrixLength
        !     print *, matrix(i, :)      
        ! enddo 
    deallocate(matrix)

end subroutine dayThreePartOne


! ----------------------------------------------------------
subroutine dayTwoPartTwo
    integer checksum, numberOfColumns, numberOfRows, currentNumber, comparisonNumber, rowDifference
    integer, allocatable :: row(:)
    
    numberOfColumns = 16
    numberOfRows = 16
    checksum = 0

    open(7, file='dayTwo.txt')
    allocate(row(numberOfColumns))
    do j = 1, numberOfRows
        read(7,*) row
        rowDifference = -1
        do i = 1, numberOfColumns
            currentNumber = row(i)
            do k = i+1, numberOfColumns
                comparisonNumber = row(k)
                if (mod(currentNumber, comparisonNumber) == 0) then
                    rowDifference = currentNumber / comparisonNumber
                    exit
                else if (mod(comparisonNumber, currentNumber) == 0) then
                    rowDifference = comparisonNumber / currentNumber
                    exit
                endif
            enddo
            if (rowDifference /= -1) exit
        enddo
        checksum = checksum + rowDifference   
    enddo
    deallocate(row)
    close(7)
    
    call printResult('Two', 3, '2', 1, checksum)
    return
end subroutine dayTwoPartTwo


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
    
    call printResult('Two', 3, '1', 1, checksum)
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
