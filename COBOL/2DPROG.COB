      ******************************************************************
      * Author: CHRISTIAN ALTICHE
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 2DPROG.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RECORD-OUT ASSIGN TO "2DPROG.DAT".
       DATA DIVISION.
       FILE SECTION.
       FD  RECORD-OUT
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS PRINT-REC.

       01  PRINT-REC PIC X(80).

       WORKING-STORAGE SECTION.
       01  I PIC 9 VALUE ZERO.
       01  J PIC 9 VALUE ZERO.
       01  L PIC 9 VALUE ZERO.
       01  CRSE PIC X(4) VALUE SPACES.
       01  CCIS.
           02 YEAR OCCURS 4 TIMES.
             03 NO-STUD OCCURS 2 TIMES PIC 99.
       01  HDG1.
           02 FILLER PIC X(19) VALUE SPACES.
           02 FILLER PIC X(19) VALUE "COLLEGE OF COMPUTER".
           02 FILLER PIC X VALUE SPACES.
           02 FILLER PIC X(23) VALUE "AND INFORMATION SCIENCE".
       01  HDG2.
           02 FILLER PIC X(31) VALUE SPACES.
           02 FILLER PIC X(18) VALUE "STUDENT POPULATION".
       01  HDG3.
           02 FILLER PIC X(20) VALUE SPACES.
           02 FILLER PIC X(4) VALUE "YEAR".
           02 FILLER PIC X(6) VALUE SPACES.
           02 FILLER PIC X(4) VALUE "BSIT".
           02 FILLER PIC X(6) VALUE SPACES.
           02 FILLER PIC X(4) VALUE "BSCS".
           02 FILLER PIC X(6) VALUE SPACES.
           02 FILLER PIC X(12) VALUE "TOTAL NUMBER".
       01  HDG4.
           02 FILLER PIC X(51) VALUE SPACES.
           02 FILLER PIC X(11) VALUE "OF STUDENTS".
       01  HDG5.
           02 FILLER PIC X(19) VALUE SPACES.
           02 CRSE-OUT PIC X(9) VALUE SPACES.
           02 ITCS-OUT OCCURS 2 TIMES.
              03 FILLER PIC X(3) VALUE SPACES.
              03 ITCSOUT PIC 99.
              03 FILLER PIC X(5) VALUE SPACES.
           02 FILLER PIC X(7) VALUE SPACES.
           02 TOT-LVL PIC 999 VALUE ZERO.
       01  HDG6.
           02 FILLER PIC X(19) VALUE SPACES.
           02 FILLER PIC X(6) VALUE "TOTALS".
           02 FILLER PIC X(5) VALUE SPACES.
           02 TOT-BSIT PIC 999 VALUE ZERO.
           02 FILLER PIC X(7) VALUE SPACES.
           02 TOT-BSCS PIC 999 VALUE ZERO.

       01  TEMPLVL PIC 999.
       01  TEMPBIT PIC 999.
       01  TEMPBCS PIC 999.
       01  BSITIN PIC 99.
       01  BSCSIN PIC 99.

       SCREEN SECTION.
       01  SCRN.
           02 BLANK SCREEN.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN OUTPUT RECORD-OUT
           PERFORM HEAD-RTN.
           PERFORM PROCESS-RTN.
           PERFORM CLOSE-RTN.

       HEAD-RTN.
           WRITE PRINT-REC FROM HDG1 BEFORE 1 LINE.
           WRITE PRINT-REC FROM HDG2 AFTER 1 LINE.
           WRITE PRINT-REC FROM HDG3 AFTER 1 LINE.
           WRITE PRINT-REC FROM HDG4 AFTER 1 LINE.
           MOVE SPACES TO PRINT-REC.
           WRITE PRINT-REC AFTER 1 LINE.

       PROCESS-RTN.
           DISPLAY SCRN.
           DISPLAY "STUDENTS FOR BSIT AND BSCS "
           LINE 5 COLUMN 5.
           MOVE 6 TO L.
           PERFORM IN-RTN VARYING I FROM 1 BY 1 UNTIL I > 4
             AFTER J FROM 1 BY 1 UNTIL J > 2.

       IN-RTN.
           DISPLAY "ENTER NUMBER OF STUDENTS FOR " LINE L COLUMN 5.
           IF J = 1
             MOVE "BSIT" TO CRSE.
           IF J = 2
             MOVE "BSCS" TO CRSE.
           DISPLAY CRSE LINE L COLUMN 34.
           IF I = 1
           DISPLAY "FRESHMAN" LINE L COLUMN 39.
           IF I = 2
           DISPLAY "SOPHOMORE" LINE L COLUMN 39.
           IF I = 3
           DISPLAY "JUNIOR   " LINE L COLUMN 39.
           IF I = 4
           DISPLAY "SENIOR   " LINE L COLUMN 39.
           DISPLAY " : " LINE L COLUMN 48.
           ACCEPT NO-STUD (I, J) LINE L COLUMN 51.

           MOVE NO-STUD (I, J) TO ITCSOUT(J).
           MOVE ITCSOUT(J) TO TEMPLVL.
           MOVE ITCSOUT(J) TO TEMPBCS.
           IF J = 1
             MOVE ITCSOUT(J) TO TEMPBIT.
           IF J = 2
             PERFORM OUT-RTN.
           ADD 1 TO L.

       OUT-RTN.
           IF I = 1
               MOVE "FRESHMAN " TO CRSE-OUT.
           IF I = 2
               MOVE "SOPHOMORE" TO CRSE-OUT.
           IF I = 3
               MOVE "JUNIOR   " TO CRSE-OUT.
           IF I = 4
               MOVE "SENIOR   " TO CRSE-OUT.
           ADD TEMPBIT TO TEMPBCS GIVING TOT-LVL.
           ADD TEMPBCS TO TOT-BSCS.
           ADD TEMPBIT TO TOT-BSIT.
           WRITE PRINT-REC FROM HDG5.
           MOVE 5 TO L.
           MOVE 0 TO NO-STUD(I, J).

       CLOSE-RTN.
           WRITE PRINT-REC FROM HDG6 AFTER 2 LINES.
           CLOSE RECORD-OUT.
           STOP RUN.
