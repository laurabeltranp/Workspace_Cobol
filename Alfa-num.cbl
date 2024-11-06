      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Alfa-Num.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 num1 pic x(9) value "ABCD ".
       01 num2 pic 9(9) value 123456789.

       PROCEDURE DIVISION.
       if num1 is ALPHABETIC THEN
           display "Num1 is alpahabetic: " num1.

           if num1 IS NOT NUMERIC THEN
               display "NUm1 Is not Numeric: " num2.

           If Num2 is NUMERIC THEN
               DISPLAY "Num2 is numeric" num2.

           IF Num2 is not ALPHABETIC then
               display " Num2 is not alpahbetic: " num2.

            STOP RUN.
       END PROGRAM Alfa-Num.
