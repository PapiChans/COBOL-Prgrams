      ******************************************************************
      * Author: CHRISTIAN ALTICHE
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SORTP.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BSITIN ASSIGN TO "BSIT.DAT".
           SELECT BSCSIN ASSIGN TO "BSCS.DAT".
           SELECT SORTF ASSIGN TO "SORTF.DAT".
           SELECT SORTD ASSIGN TO "SORTD.DAT".
           SELECT PRNT ASSIGN TO "PRNT.DAT".
       DATA DIVISION.
       FILE SECTION.
       FD  BSITIN
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS IT-REC.
       01  IT-REC.
           02 IT-NME PIC X(15).
           02 IT-GDE PIC 9(3).
       FD  BSCSIN
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS CS-REC.
       01  CS-REC.
           02 CS-NME PIC X(15).
           02 CS-GDE PIC 9(3).
       SD  SORTF
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS SORTF-REC.
       01  SORTF-REC.
           02 STD-NME PIC X(15).
           02 STD-GDE PIC 9(3).
       FD  SORTD
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS SORTD-REC.
       01  SORTD-REC.
           02 STD-NME-S PIC X(15).
           02 STD-GDE-S PIC 9(3).
       FD  PRNT
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS PRNT-REC.
       01  PRNT-REC PIC X(80).

       WORKING-STORAGE SECTION.
       01  HDG1.
           02 FILLER PIC X(24) VALUE SPACES.
           02 FILLER PIC X(12) VALUE "STUDENT NAME".
           02 FILLER PIC X(15) VALUE SPACES.
           02 FILLER PIC X(6) VALUE "GRADES".
       01  PRNT-LNE.
           02 FILLER PIC X(21) VALUE SPACES.
           02 STD-NME-O PIC X(15).
           02 FILLER PIC X(16) VALUE SPACES.
           02 STD-GDE-O PIC 9(3).
       01  HDG2.
           02 FILLER PIC X(23) VALUE SPACES.
           02 FILLER PIC X(13) VALUE "AVERAGE GRADE".
           02 FILLER PIC X(16) VALUE SPACES.
           02 AVE PIC 9(3) VALUE 000.
       01  EOF PIC XXX VALUE "NO".
       01  CNT PIC 9.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           MERGE SORTF
           ON ASCENDING KEY STD-NME
           ON DESCENDING KEY STD-GDE
           USING BSITIN, BSCSIN
           GIVING SORTD.

           OPEN INPUT SORTD OUTPUT PRNT.
           WRITE PRNT-REC FROM HDG1.
           MOVE SPACES TO PRNT-REC.
           WRITE PRNT-REC AFTER 1 LINE.
           READ SORTD AT END MOVE "YES" TO EOF.

           PERFORM READ-RTN UNTIL EOF = "YES".
           DIVIDE CNT INTO AVE.
           IF EOF = "YES"
           WRITE PRNT-REC FROM HDG2 AFTER 2 LINES.
           CLOSE SORTD, PRNT
           DISPLAY "SORT SUCCESSFUL".
           STOP RUN.

       READ-RTN.
           ADD STD-GDE-S TO AVE.
           MOVE STD-NME-S TO STD-NME-O.
           MOVE STD-GDE-S TO STD-GDE-O.
           WRITE PRNT-REC FROM PRNT-LNE AFTER 1 LINE.
           ADD 1 TO CNT.
           READ SORTD AT END MOVE "YES" TO EOF.
