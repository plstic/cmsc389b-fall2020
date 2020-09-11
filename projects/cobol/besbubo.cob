      *> >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BESBUBO.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
           WORKING-STORAGE SECTION.
      *> main data
           01 WS-BREAK PIC 9(1) VALUE 1.
           01 WS-INPUT PIC X(21).
           01 WS-IDX   PIC 9(4) VALUE 0.
           01 WS-JDX   PIC 9(4) VALUE 0.
           01 WS-SCHEDULE.
               02 WS-NUM-ROWS PIC 9(3) VALUE 18.
               02 WS-NUM-DAYS PIC 9(3) VALUE 5.
               02 WS-SCHEDULE-ROW OCCURS 18 TIMES.
                   03 WS-SCHOOL PIC A(4). *> may delete
                   03 WS-CODE   PIC X(3). *> may delete
                   03 WS-DAYS PIC X(7) VALUE '       ' OCCURS 5 TIMES.
           01 WS-SUCCESS PIC 9(1) VALUE 1.
           01 WS-Q PIC 9(2).
           01 WS-R PIC 9(2).
      *> command-line flag stuffs
           01 ARGV PIC X(100) VALUE SPACES.
               88 INT VALUE "-i", "-I", "--interactive".
           01 CMDSTATUS PIC X VALUE SPACES.
               88 LASTCMD VALUE "l".
           01 WS-SHELL-FLAG PIC 9(1) VALUE 0.
       PROCEDURE DIVISION.
           *> process cmd-line args
           PERFORM UNTIL LASTCMD
               MOVE LOW-VALUES TO ARGV
               ACCEPT ARGV FROM ARGUMENT-VALUE
               IF ARGV > LOW-VALUES
                  PERFORM process-argv
               ELSE
                  MOVE "l" TO CMDSTATUS
               END-IF
           END-PERFORM.
           *> begin repl
           IF WS-SHELL-FLAG = 1
               DISPLAY "Welcome to Besbubo"
               DISPLAY "Type '\h' for help"
           END-IF.
           PERFORM console-loop UNTIL WS-BREAK = 0.
           IF WS-SHELL-FLAG = 1
               DISPLAY "Bye"
           END-IF.
           *> finished
           STOP RUN.

           process-argv.
           EVALUATE TRUE
               WHEN INT
                   IF WS-SHELL-FLAG = 0
                       MOVE 1 TO WS-SHELL-FLAG
                   ELSE
                       DISPLAY " Duplicate arg: " ARGV
                   END-IF
               WHEN OTHER
                   DISPLAY " Invalid arg: " ARGV
           END-EVALUATE.

           console-loop.
           IF WS-SHELL-FLAG = 1
               DISPLAY "besbubo> " WITH NO ADVANCING
           END-IF
           ACCEPT WS-INPUT
           EVALUATE WS-INPUT
               WHEN SPACE
                   CONTINUE
               WHEN LOW-VALUE
                   CONTINUE
               WHEN "\h"
                   DISPLAY " \h -- displays help"
                   DISPLAY " \q -- quits shell"
                   DISPLAY " \p -- prints current schedule"
                   DISPLAY " \c -- clears current schedule"
                   DISPLAY " \a [...] -- adds provided course"
               WHEN "\q"
                   PERFORM set-break-stop
               WHEN "\p"
                   PERFORM print-schedule
               WHEN "\c"
                   PERFORM clear-schedule
               WHEN OTHER
                   PERFORM add-class
           END-EVALUATE.

           add-class.
           IF WS-INPUT(1:2) <> "\a"
               DISPLAY " - unknown command " WS-INPUT
               EXIT PARAGRAPH
           END-IF
           PERFORM insert-class
           EVALUATE WS-SUCCESS
               WHEN 0
                   DISPLAY " - added " WS-INPUT(4:18)
               WHEN 1
                   DISPLAY " - overlapping input"
               WHEN 2
                   DISPLAY " - invalid start time "
               WHEN OTHER *> doesn't really happen
                   DISPLAY " - expected: AAAA999 99999 9999"
           END-EVALUATE.

           set-break-stop.
           *> YOUR CODE HERE
           *> --------------

           insert-class.
           *> YOUR CODE HERE
           *> --------------

           print-schedule.
           *> YOUR CODE HERE
           *> --------------

           clear-schedule.
           PERFORM do-schedule-clear.
           DISPLAY " - cleared schedule".

           do-schedule-clear.
           *> YOUR CODE HERE
           *> --------------
