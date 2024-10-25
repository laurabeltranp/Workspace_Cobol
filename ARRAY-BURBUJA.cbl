
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 TABLA.
               02 VALOR PIC 9 OCCURS 100 TIMES.
       01 I PIC 9 VALUE 0.
       01 N PIC 9 VALUE 0.
       01 J PIC 9 VALUE 0.
       01 AUX PIC 9 VALUE 0.
       01 TAM PIC 9 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

            DISPLAY "INGRESE UN TAMAÑO PARA EL ARRAY"
            ACCEPT TAM.
       PERFORM VARYING I FROM 1 BY 1 UNTIL I > TAM
            DISPLAY "INGRESE NUMEROS PARA CADA CELDA" I ":"
            ACCEPT N
            MOVE N TO VALOR(I)
       END-PERFORM.
       PERFORM VARYING I FROM 1 BY 1 UNTIL I > TAM - 1
        PERFORM VARYING J FROM I + 1 BY 1 UNTIL J > TAM
            IF VALOR(I) > VALOR(J)
                MOVE VALOR(I) TO AUX
                MOVE VALOR(J) TO VALOR(I)
                MOVE AUX TO VALOR(J)
            END-IF
       END-PERFORM
       END-PERFORM.

   .
            STOP RUN.
      ** add other procedures here
       END PROGRAM YOUR-PROGRAM-NAME.
