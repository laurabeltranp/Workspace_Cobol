       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALTA-PRODUCTO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL PRODUCTO
               ASSIGN TO "PRODUCTO-SECUENCIAL.dat"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS File-Status.

       DATA DIVISION.
       FILE SECTION.
       FD PRODUCTO.
       01 PRODUCTO-REGISTRO.
               05 PRODUCTO-ID PIC 9(6).
               05 PRODUCTO-CONTROL PIC 9.
               05 PRODUCTO-DESCRIP PIC X(30).
               05 PRODUCTO-PRECIO PIC 9(5)V99.
               05 PRODUCTO-STOCK PIC 9(6).
               05 PRODUCTO-CADUCIDAD PIC 9(8).
               05 PRODUCTO-CRITICO PIC 9(6).

       WORKING-STORAGE SECTION.
       01 DIGITO PIC 9 VALUE 0.
       01 File-Status PIC XX VALUE SPACES.
       01 DIGITO-OK PIC XX VALUE SPACES.
       01 SI-NO PIC X VALUE "S".

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
       PROGRAM-BEGIN.
           OPEN EXTEND PRODUCTO.
           IF File-Status NOT = "00"
           DISPLAY "ERROR AL ABRIR EL ARCHIVO FILE-STATUS: " File-Status
              CLOSE PRODUCTO STOP RUN.

               PERFORM OBTENER-CAMPOS UNTIL SI-NO = "N"
               CLOSE PRODUCTO.
               DISPLAY "FIN DEL PROGRAMA".
               STOP RUN.

           OBTENER-CAMPOS.
               MOVE SPACE TO PRODUCTO-REGISTRO.

               DISPLAY "INTRODUCE UN ID PRODUCTO: "
               ACCEPT PRODUCTO-ID.


               PERFORM CALCULO-DIGITO UNTIL DIGITO-OK = "OK".
               MOVE SPACES TO DIGITO-OK.

               DISPLAY "INGRESE DESCRIPCION DEL PRODUCTO: ".
               ACCEPT PRODUCTO-DESCRIP.

               DISPLAY "INGRESE EL PRECIO DEL PRODUCTO: ".
               ACCEPT PRODUCTO-PRECIO.

               DISPLAY "INGRESE EL STOCK DEL PRODUCTO: ".
               ACCEPT PRODUCTO-STOCK.
               DISPLAY "INGRESE EL CADUCIDAD DEL PRODUCTO: ".
               ACCEPT PRODUCTO-CADUCIDAD.
               DISPLAY "INGRESE EL CRITICO DEL PRODUCTO: ".
               ACCEPT PRODUCTO-CRITICO.
               WRITE PRODUCTO-REGISTRO.

               DISPLAY "DESEA ALMACENAR OTRO REGISTRO?".
               ACCEPT SI-NO.
               IF SI-NO = "S" OR "s"
                   MOVE "S" TO SI-NO
                   ELSE
                       IF SI-NO = "N" OR "n"
                           MOVE "N" TO SI-NO.

           CALCULO-DIGITO.
               CALL "DigitoC" USING PRODUCTO-ID, DIGITO END-CALL.
               IF DIGITO NOT EQUAL PRODUCTO-CONTROL
                   MOVE ZEROES TO PRODUCTO-ID DIGITO
                   PERFORM OBTENER-CAMPOS
                   ELSE
                       MOVE "OK" TO DIGITO-OK.
                       MOVE 0 TO DIGITO.
           FIN-DIGITO.


       END PROGRAM ALTA-PRODUCTO.
