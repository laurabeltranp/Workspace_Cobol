
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 N PIC 9(3).
       01 SUMA PIC 9(5).
       01 LIMITE PIC 9(3).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

            DISPLAY "INGRESE UN NUMERO: "
            ACCEPT LIMITE.

            PERFORM VARYING N FROM 1 BY 1 UNTIL N > LIMITE
                DISPLAY N
                ADD N TO SUMA
           END-PERFORM.
           DISPLAY "LA SUMA DE LOS NUMEROS ES: " SUMA.
            STOP RUN.
      ** add other procedures here
       END PROGRAM YOUR-PROGRAM-NAME.
