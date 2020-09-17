       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATAMANIPULATION.
       DATA DIVISION.
           WORKING-STORAGE SECTION.
               01 some-data PIC 9(6).
               01 othr-data PIC X(6).
               01 table1.
                   03 table1-row PIC 9(2) OCCURS 3 TIMES.
               01 table2.
                   02 table2-row OCCURS 4 TIMES.
                       04 row2-data-opt PIC A(1) VALUE 'e'.
                       04 table2-col PIC 9(2) OCCURS 5 TIMES.
       PROCEDURE DIVISION.
           DISPLAY '| ' some-data ' | ' othr-data ' |'.
           MOVE  12  TO some-data.
           MOVE 'A1' TO othr-data.
           DISPLAY '| ' some-data ' | ' othr-data ' |'.
           DISPLAY some-data(5:2).
           DISPLAY othr-data(1:2).
           MOVE 'B2' TO othr-data(4:1).
           DISPLAY '| ' some-data ' | ' othr-data ' |'.
       
           DISPLAY table1.
           DISPLAY table2.
           DISPLAY table1-row(3).
           DISPLAY table2-row(3).
           DISPLAY table2-col(3,4).
       
           MOVE 11 TO table1-row(3).
           MOVE 19 TO table2-col(3,4).
       
           DISPLAY table1.
           DISPLAY table2.
           DISPLAY table1-row(3).
           DISPLAY table2-row(3).
           DISPLAY table2-col(3,4).
       
           STOP RUN.
