       IDENTIFICATION DIVISION.
       PROGRAM-ID. VARIABLES.
       DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 VAR-STR PIC X(8) VALUE '1234abcd'.
           01 VAR-YEET.
               02 YEET-A PIC 9(2).
               02 YEET-X.
                   05 YEET-X-A PIC A(8) VALUE 'sub'.
                   05 YEET-X-B PIC A(8) VALUE 'part'.
               02 YEET-B PIC X(4) VALUE '$2ab'.
       PROCEDURE DIVISION.
           DISPLAY VAR-STR
           DISPLAY VAR-YEET
           DISPLAY YEET-A YEET-X YEET-B
           DISPLAY YEET-X-A ' - ' YEET-X-B
           STOP RUN.
