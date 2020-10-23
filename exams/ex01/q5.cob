      *> >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Q5.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
           WORKING-STORAGE SECTION.
      *> main data
           01 WS-IDX PIC  9(5) VALUE 1.
           01 WS-N   PIC  9(5).
           01 WS-LIST OCCURS 999 TIMES.
               02 WS-LIST-COL PIC S9(5).
      *> extra data

       PROCEDURE DIVISION.
           DISPLAY "list length = " WITH NO ADVANCING.
           ACCEPT WS-N.
           PERFORM accept-integer UNTIL WS-IDX > WS-N.
           PERFORM prune-array.
           MOVE 1 TO WS-IDX.
           PERFORM print-list-col UNTIL WS-IDX > WS-N.
           STOP RUN.

           accept-integer.
           ACCEPT WS-LIST(WS-IDX).
           ADD 1 TO WS-IDX.

      *> YOUR CODE HERE!
      *>   no need to return anything
      *>   just modify the list (and the list length accordingly)
           prune-array.
           DISPLAY "modify me".

           print-list-col.
           DISPLAY WS-LIST(WS-IDX) ", " WITH NO ADVANCING.
           ADD 1 TO WS-IDX.
