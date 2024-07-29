      ******************************************************************
      * Author: CHRISTIAN ALTICHE
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EX001.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RECORD-OUT ASSIGN TO "EXER1.DAT".
       DATA DIVISION.
       FILE SECTION.
       FD  RECORD-OUT
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS PRINT-REC.

       01  PRINT-REC PIC X(80).

       WORKING-STORAGE SECTION.
       01  HDG-1.
           02 FILLER PIC X(20) VALUE SPACES.
           02 FILLER PIC X(22) VALUE "POLYTECHNIC UNIVERSITY".
           02 FILLER PIC X(19) VALUE " OF THE PHILIPPINES".
       01  HDG-2.
           02 FILLER PIC X(31) VALUE SPACES.
           02 FILLER PIC X(18) VALUE "QUEZON CITY BRANCH".
   .   01  HDG-3.
           02 FILLER PIC X(36) VALUE SPACES.
           02 FILLER PIC X(8) VALUE "BSIT 2-1".
       01  HDG-4.
           02 FILLER PIC X(34) VALUE SPACES.
           02 FILLER PIC X(12) VALUE "QUIZ SUMMARY".
       01  HDG-5.
           02 FILLER PIC X(1) VALUE SPACES.
           02 FILLER PIC X(7) VALUE "STUDENT".
           02 FILLER PIC X(4) VALUE SPACES.
           02 FILLER PIC X(7) VALUE "STUDENT".
           02 FILLER PIC X(19) VALUE SPACES.
           02 FILLER PIC X(6) VALUE "QUIZ#1".
           02 FILLER PIC X(6) VALUE SPACES.
           02 FILLER PIC X(6) VALUE "QUIZ#2".
           02 FILLER PIC X(6) VALUE SPACES.
           02 FILLER PIC X(6) VALUE "QUIZ#3".
           02 FILLER PIC X(6) VALUE SPACES.
           02 FILLER PIC X(7) VALUE "AVERAGE".
       01  HDG-6.
           02 FILLER PIC X(1) VALUE SPACES.
           02 FILLER PIC X(6) VALUE "NUMBER".
           02 FILLER PIC X(5) VALUE SPACES.
           02 FILLER PIC X(4) VALUE "NAME".

       01  QZ1 PIC 9V99.
       01  QZ2 PIC 9V99.
       01  QZ3 PIC 9V99.
       01  AVE PIC 9V99.
       01  TEQZ1 PIC 9V99.
       01  TEQZ2 PIC 9V99.
       01  TEQZ3 PIC 9V99.
       01  TEAVE PIC 9V99.
       01  ANS PIC X VALUE 'Y'.
       01  CNT PIC 9.
       01  L PIC 9.
       01  E-O-F PIC XXX VALUE "YES".
       01  HQ1 PIC 9V99 VALUE 0.
       01  HQ2 PIC 9V99 VALUE 0.
       01  HQ3 PIC 9V99 VALUE 0.
       01  HAVE PIC 9V99 VALUE 0.


       01  HDG-7.
           02 FILLER PIC X(12) VALUE "QUIZ AVERAGE".
           02 FILLER PIC X(25) VALUE SPACES.
           02 AQZ1 PIC 9.99.
           02 FILLER PIC X(8) VALUE SPACES.
           02 AQZ2 PIC 9.99.
           02 FILLER PIC X(8) VALUE SPACES.
           02 AQZ3 PIC 9.99.
           02 FILLER PIC X(8) VALUE SPACES.
           02 TAVE PIC 9.99.

       01  PRINT-LINE.
           02 STD-NO PIC X(10).
           02 FILLER PIC X(1) VALUE SPACES.
           02 STD-NM PIC X(25).
           02 FILLER PIC X(1) VALUE SPACES.
           02 DQZ1 PIC 9.99.
           02 FILLER PIC X(8) VALUE SPACES.
           02 DQZ2 PIC 9.99.
           02 FILLER PIC X(8) VALUE SPACES.
           02 DQZ3 PIC 9.99.
           02 FILLER PIC X(8) VALUE SPACES.
           02 DAVE PIC 9.99.

       SCREEN SECTION.
       01  BSCRN.
           02  BLANK SCREEN.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN OUTPUT RECORD-OUT.
           PERFORM HEAD-RTN.
           PERFORM INPUT-RTN UNTIL ANS = 'N' OR ANS = 'n'.
           PERFORM CLOSE-RTN.

       HEAD-RTN.
           WRITE PRINT-REC FROM HDG-1 BEFORE 1 LINE.
           WRITE PRINT-REC FROM HDG-2 AFTER 1 LINE.
           WRITE PRINT-REC FROM HDG-3 AFTER 1 LINE.
           WRITE PRINT-REC FROM HDG-4 AFTER 2 LINES.
           WRITE PRINT-REC FROM HDG-5 AFTER 1 LINE.
           WRITE PRINT-REC FROM HDG-6 AFTER 1 LINE.
           MOVE SPACES TO PRINT-REC.
           WRITE PRINT-REC AFTER 1 LINE.

       INPUT-RTN.
           DISPLAY BSCRN.
           MOVE 2 TO L.
           DISPLAY "ENTER STUDENT NUMBER: " LINE L COLUMN 5.
           ACCEPT STD-NO LINE L COLUMN 35.

           ADD 1 TO L.
           DISPLAY "ENTER STUDENT NAME: " LINE L COLUMN 5.
           ACCEPT STD-NM LINE L COLUMN 35.

           ADD 1 TO L.
           DISPLAY "QUIZ #1 SCORE" LINE L COLUMN 5.
           ACCEPT QZ1 LINE L COLUMN 35.

           ADD 1 TO L.
           DISPLAY "QUIZ #2 SCORE" LINE L COLUMN 5.
           ACCEPT QZ2 LINE L COLUMN 35.

           ADD 1 TO L.
           DISPLAY "QUIZ #3 SCORE" LINE L COLUMN 5.
           ACCEPT QZ3 LINE L COLUMN 35.


           COMPUTE AVE = (QZ1 + QZ2 + QZ3).
           DIVIDE 3 INTO AVE.

           MOVE QZ1 TO DQZ1.
           MOVE QZ2 TO DQZ2.
           MOVE QZ3 TO DQZ3.
           MOVE AVE TO DAVE.
           ADD 1 TO CNT.

           ADD QZ1 TO HQ1.
           ADD QZ2 TO HQ2.
           ADD QZ3 TO HQ3.
           ADD AVE TO HAVE.

           COMPUTE TEQZ1 = HQ1 / CNT.
           COMPUTE TEQZ2 = HQ2 / CNT.
           COMPUTE TEQZ3 = HQ3 / CNT.
           COMPUTE TEAVE = HAVE / CNT.

           MOVE TEQZ1 TO AQZ1.
           MOVE TEQZ2 TO AQZ2.
           MOVE TEQZ3 TO AQZ3.
           MOVE TEAVE TO TAVE.

           ADD 2 TO L.
           WRITE PRINT-REC FROM PRINT-LINE AFTER 1 LINE.
           DISPLAY "ENTER ANOTHER RECORD?(Y/N)" LINE L COLUMN 5.
           ACCEPT ANS LINE L COLUMN 35.

       CLOSE-RTN.
           WRITE PRINT-REC FROM HDG-7 AFTER 1 LINE.
           CLOSE RECORD-OUT.
           STOP RUN.
