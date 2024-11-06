       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALTA-VENTAS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT VENTAS
               ASSIGN TO "VENTAS-INDEXADO.dat"
               ORGANIZATION IS INDEXED
               RECORD KEY IS VENTAS-ID
               ALTERNATE RECORD KEY IS PRODUCTO-ID WITH DUPLICATES
               FILE STATUS IS WS-VENTAS-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD VENTAS.
       01 VENTAS-REGISTRO.
           03 VENTAS-ID PIC 9(6).        *> Nuevo campo de clave única
           03 PRODUCTO-ID PIC 9(6).
           03 UNIDADES PIC 9(3).

       WORKING-STORAGE SECTION.
       01 WS-VENTAS-STATUS PIC XX.
       01 SINO PIC X VALUE 'S'.
       01 VENTAS-ID-CONTADOR PIC 9(6) VALUE 1.  *> Contador para generar VENTAS-ID único

       PROCEDURE DIVISION.
           OPEN I-O VENTAS.
           IF WS-VENTAS-STATUS = "35"
               DISPLAY "Archivo de ventas no encontrado, se crea."
               OPEN OUTPUT VENTAS
               CLOSE VENTAS
               OPEN I-O VENTAS
           END-IF

           PERFORM UNTIL SINO NOT = 'S'
               MOVE VENTAS-ID-CONTADOR TO VENTAS-ID

               DISPLAY "Ingrese el ID del producto: "
               ACCEPT PRODUCTO-ID

               DISPLAY "Ingrese las unidades vendidas: "
               ACCEPT UNIDADES

               WRITE VENTAS-REGISTRO INVALID KEY
                   DISPLAY "Error al registrar la venta. Estado: "
                   WS-VENTAS-STATUS
               END-WRITE

               IF WS-VENTAS-STATUS = "00"
                   DISPLAY "Venta registrada correctamente."
                   ADD 1 TO VENTAS-ID-CONTADOR   *> Incrementar contador para la siguiente venta
               END-IF

               DISPLAY "¿Desea registrar otra venta? (S/N): "
               ACCEPT SINO
           END-PERFORM.

           CLOSE VENTAS.
           STOP RUN.
