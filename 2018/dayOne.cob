       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADVENTOFCODE.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
               SELECT DAYONE ASSIGN TO 'day1.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
           FD DAYONE.
               01 DAYONE-FILE.
                  05 FREQUENCY PIC X(6).

           WORKING-STORAGE SECTION.
           01 WS-FREQUENCY PIC 9(5).
           01 WS-FREQUENCYRES PIC S9(10).
           01 WS-FREQUENCYSUM PIC S9(10).
           01 VARSWAP PIC X(5) JUSTIFIED RIGHT.
           01 WS-DAYONE.
               05 WS-NEGATION PIC A(1).
               *> This could be 9(5) if we had a source with left padded
               *> zeros. Instead we must programically add them.
               05 WS-FREQUENCYSTRING PIC A(5).
           01 WS-EOF PIC A(1).

       PROCEDURE DIVISION.
           MAIN.
               SET WS-FREQUENCYSUM TO 0.
    
               OPEN INPUT DAYONE.
                   PERFORM UNTIL WS-EOF='Y'
                   READ DAYONE INTO WS-DAYONE
                       AT END MOVE 'Y' TO WS-EOF
                       NOT AT END
                           PERFORM CONVERT-WHITESPACES-TO-ZEROES
                           IF WS-NEGATION IS EQUAL TO "-"
                               SUBTRACT WS-FREQUENCY
                                   FROM WS-FREQUENCYSUM
                                   GIVING WS-FREQUENCYRES
                               MOVE WS-FREQUENCYRES TO WS-FREQUENCYSUM
                           ELSE
                              ADD WS-FREQUENCY
                                   TO WS-FREQUENCYSUM
                                   GIVING WS-FREQUENCYRES
                               MOVE WS-FREQUENCYRES TO WS-FREQUENCYSUM
                           END-IF
                   END-READ
                   END-PERFORM.
               CLOSE DAYONE.
    
               DISPLAY "Part 1: " WS-FREQUENCYSUM.
           STOP RUN.

           *> Imported strings are right padded with spaces.
           *> Move those spaces to the left of the content and replace
           *> them with zeroes. 
           CONVERT-WHITESPACES-TO-ZEROES.
               UNSTRING WS-FREQUENCYSTRING DELIMITED BY ' ' INTO VARSWAP
               INSPECT VARSWAP REPLACING LEADING SPACE BY '0'
               MOVE VARSWAP TO WS-FREQUENCY
               .

       END PROGRAM ADVENTOFCODE.
