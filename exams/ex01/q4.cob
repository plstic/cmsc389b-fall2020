      *> >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Q4.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
           WORKING-STORAGE SECTION.
      *> main data
           01 WS-IDX PIC  9(5) VALUE 1.
           01 WS-CO1 PIC S9(5).
           01 WS-CO2 PIC S9(5).
           01 WS-N   PIC  9(5).
           01 WS-LIST OCCURS 999 TIMES.
               02 WS-LIST-COL PIC S9(5).
      *> extra data

       PROCEDURE DIVISION.
           DISPLAY "a_{n-1} coeff = " WITH NO ADVANCING.
           ACCEPT WS-CO1.
           DISPLAY "a_{n-2} coeff = " WITH NO ADVANCING.
           ACCEPT WS-CO2.
           DISPLAY "list length = " WITH NO ADVANCING.
           ACCEPT WS-N.
           PERFORM accept-integer UNTIL WS-IDX > WS-N.
           PERFORM check-array.
           STOP RUN.

           accept-integer.
           ACCEPT WS-LIST(WS-IDX).
           ADD 1 TO WS-IDX.

      *> YOUR CODE HERE!
      *>   can just print out "true" or "false"
           check-array.
           DISPLAY "false".
