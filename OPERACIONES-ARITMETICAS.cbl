      ******************************************************************
      * Author: LAURA BELTRAN PULIDO
      * Date:25/10/2024
      * Purpose:EXPLICACIÓN OPERACIONES ARTIMETICAS BASICAS EN COBOL
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OPERACIONES-BASICAS.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
         01 NUM1 PIC 99 VALUE 10.
         01 NUM2 PIC 9 VALUE 5.
         01 RESULTADO PIC 99.
       PROCEDURE DIVISION.
       INICIO.
      *ADD PARA SUMAR
            ADD NUM1 TO NUM2 GIVING RESULTADO.
            DISPLAY RESULTADO.
      *SUBTRACT PARA RESTAR
            SUBTRACT NUM1 FROM NUM2 GIVING RESULTADO.
            DISPLAY RESULTADO.
      *MULTIPLY PARA MULTIPLICAR
             MULTIPLY NUM1 BY NUM2 GIVING RESULTADO.
             DISPLAY RESULTADO.
      *DIVIDE PARA DIVIDIR
             DIVIDE NUM1 BY NUM2 GIVING RESULTADO.
             DISPLAY RESULTADO.


            STOP RUN.
      ** add other procedures here
       END PROGRAM OPERACIONES-BASICAS.
