      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-DNI-num pic 9(8).
       01 WS-resto pic 99.
       01 WS-letra pic X(1).
       01 WS-letras pic X(23) value "TRWAGMYFPDXBNJZSQVHLCKE".
       01 WS-opcion pic 9(1).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Introduce el numero del dni sin letra"
            ACCEPT WS-DNI-num

            DIVIDE WS-DNI-num by 23 GIVING WS-resto REMAINDER WS-resto
            MOVE WS-resto to WS-resto.
            MOVE WS-letras(WS-resto + 1:1) TO WS-letra.
            DISPLAY "La letra es: " WS-letra.
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
