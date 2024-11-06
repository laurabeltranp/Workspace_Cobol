      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
              IDENTIFICATION DIVISION.
       PROGRAM-ID. dif-fechas.
       ENVIRONMENT DIVISIOn.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 Tabla-mes pic X(24) value "312831303130313130313031".
       01 MESES Redefines TABLA-MES.
       02 DIA-MES pic 99 OCCURS 12 TIMES.

       01 F1 pic 9(8).
       01 Fecha1 redefines F1.
        02 AA1 pic 9999.
        02 MM1 pic 99.
        02 DD1 pic 99.

       01 F2 pic 9(8).
       01 Fecha2 redefines F2.
       02 AA2 pic 9999.
       02 MM2 pic 99.
       02 DD2 pic 99.

        77 RESTO PIC 99 VALUE ZEROES.
        77 AUX-MES PIC 99 VALUE ZEROES.
        77 AUX PIC 9(8).
        77 CANT-DIAS PIC s9(8) VALUE ZEROES.


       PROCEDURE DIVISION.

            DISPLAY "FECHA1"
            ACCEPT F1.
            DISPLAY "FECHA2"
            ACCEPT f2.

            IF F1> F2
                MOVE F1 TO AUX-MES
                MOVE F2 TO F1
                MOVE AUX TO F2
               END-IF
      *PARA GUARDAR ALGO EN UN CAMPO TENGO QUE GUARDARLO EN UN AUXILIAR

           PERFORM CONTROL-BISIESTO.
           IF FECHA1 NOT = 0 AND FECHA2 NOT = 0
               PERFORM R-PERFORM UNTIL FECHA1 = FECHA2
              END-IF

              GO TO FIN.
           R-PERFORM.
               ADD 1 TO DD1
               ADD 1 TO cant-dias
               IF DD1> DIA-MES (MM1)
               MOVE 1 TO DD1
               ADD 1 TO MM1
               IF MM1> 12 MOVE 1 TO MM1
                   ADD 1 TO AA1
                   PERFORM CONTROL-BISIESTO
                   END-IF
                END-IF.
           CONTROL-BISIESTO.
                   DIVIDE AA1 BY 4 GIVING AUX-MES REMAINDER RESTO
                   IF RESTO NOT = 0 MOVE 28 TO DIA-MES (2)
                   ELSE MOVE 29 TO DIA-MES (2)
                   END-IF.
                   FIN.
                   DISPLAY "CANT-DIAS" CANT-DIAS.
               EXIT PROGRAM.
