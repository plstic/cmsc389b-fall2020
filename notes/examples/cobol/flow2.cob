       IDENTIFICATION DIVISION.
       PROGRAM-ID. FLOW.
       DATA DIVISION.
           WORKING-STORAGE SECTION.
               01 alpha PIC 9(4).
               01 beta  PIC 9(4).
               01 gamma PIC 9(4).
       PROCEDURE DIVISION.
           func1.
           DISPLAY "Hello, World!".
       
           small-disp.
           DISPLAY alpha " " beta " " gamma.
           EXIT PARAGRAPH.
       
           func-c.
           MOVE 5 TO alpha.
           MOVE 3 TO beta.
           PERFORM small-disp.
       
           func-a.
           MOVE 2 TO gamma.
           PERFORM small-disp.
       
           func-b.
           MULTIPLY alpha BY beta GIVING gamma.
           PERFORM small-disp.
           *> play around with moving the  STOP RUN
           STOP RUN.
       
           never-called.
           DISPLAY "this is not called".

           PERFORM func1.
           PERFORM
               DISPLAY "in-line perform"
               DISPLAY "another continuation line"
           END-PERFORM.
           PERFORM func-c THRU func-b.
