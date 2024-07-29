       IDENTIFICATION DIVISION.
       PROGRAM-ID. BMI01.

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 L PIC 9.
       01 HGT-1 PIC 999V9.
       01 HGT-2 PIC 999V9.
       01 HGT PIC 999V9.
       01 WGT PIC 999V9.
       01 TBMI PIC 999V9.
       01 HDG.
           02 FILLER PIC X(14) VALUE "BMI CALCULATOR".
       01 DSP.
           02 FILLER PIC X(10) VALUE "YOUR BMI: ".
           02 BMI PIC 999.9.
       SCREEN SECTION.
       01  BRCRN.
           02 BLANK SCREEN.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM HDG-RTN.
           PERFORM INPUT-RTN.

       HDG-RTN.
           DISPLAY BRCRN.
           MOVE 2 TO L.
           ADD 1 TO L.
           DISPLAY HDG.

       INPUT-RTN.
           ADD 1 TO L.
           DISPLAY "ENTER YOUR HEIGHT IN CENTIMETERS (cm): " LINE L.
           ACCEPT HGT-1 LINE L COLUMN 40.
           ADD 1 TO L.
           DISPLAY "ENTER YOUR WEIGHT IN KILOGRAMS (kg): " LINE L.
           ACCEPT WGT LINE L COLUMN 40.
           COMPUTE HGT-2 = HGT-1 / 100.
           COMPUTE HGT = HGT-2 * HGT-2.
           DIVIDE WGT BY HGT GIVING TBMI.
           MOVE TBMI TO BMI.
           ADD 1 TO L.
           DISPLAY DSP LINE L.
           STOP RUN.
