       IDENTIFICATION DIVISION.
       PROGRAM-ID. FLOW.
       DATA DIVISION.
           WORKING-STORAGE SECTION.
               01 alpha PIC X(5).
               01 beta  PIC 9(4).
               01 gamma PIC 9(4).
       PROCEDURE DIVISION.
           PERFORM main-loop UNTIL alpha = '1   '.
           DISPLAY "Bye!".
           STOP RUN.

           main-loop.
           DISPLAY "type something> " WITH NO ADVANCING
           ACCEPT alpha
           DISPLAY alpha
           EVALUATE alpha
               WHEN SPACE
                   CONTINUE
               WHEN LOW-VALUE
                   CONTINUE
               WHEN "beta"
                   DISPLAY "beta =" WITH NO ADVANCING
                   ACCEPT beta
                   DISPLAY beta
                   IF beta <> 1 THEN
                       DISPLAY "beta is not 1"
                   ELSE
                       DISPLAY "beta is 1"
                   END-IF
               WHEN "gamma"
                   DISPLAY "gamma =" WITH NO ADVANCING
                   ACCEPT gamma
                   DISPLAY gamma
                   IF gamma > 25 THEN
                       MOVE 25 TO gamma
                   END-IF
                   PERFORM big-gamma gamma TIMES
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

           big-gamma.
           DISPLAY "GAMMA! ".
