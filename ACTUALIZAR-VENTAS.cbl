       IDENTIFICATION DIVISION.
       PROGRAM-ID. GESTION-STOCK.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT VENTAS-DIARIAS
               ASSIGN TO "ventas-diarias.dat"
               ORGANIZATION IS SEQUENTIAL.

           SELECT PRODUCTO
               ASSIGN TO "producto.dat"
               ORGANIZATION IS INDEXED
               RECORD KEY IS PRODUCTO-ID
               ACCESS MODE IS DYNAMIC.

       DATA DIVISION.
       FILE SECTION.
       FD VENTAS-DIARIAS.
       01 VENTAS-REGISTRO.
           05 V-VENTA-PRODUCTO-ID     PIC 9(6).
           05 V-UNIDADES-VENDIDAS     PIC 9(3).

       FD PRODUCTO.
       01 PRODUCTO-REGISTRO.
           05 PRODUCTO-ID            PIC 9(6).
           05 PRODUCTO-STOCK         PIC 9(6).
           05 PRODUCTO-DESCRIPCION    PIC X(35).
           05 PRODUCTO-PRECIO         PIC 9(4)V99.

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS             PIC XX.
       01 WS-EOF                     PIC X VALUE "N".

       PROCEDURE DIVISION.
       MAIN-PARA.
           OPEN INPUT VENTAS-DIARIAS
           OPEN I-O PRODUCTO
           IF WS-FILE-STATUS NOT = "00"
               DISPLAY "Error al abrir archivos. Código de estado: "
               WS-FILE-STATUS
               STOP RUN
           END-IF

           PERFORM PROCESAR-VENTAS
           CLOSE VENTAS-DIARIAS
           CLOSE PRODUCTO
           DISPLAY "Proceso de actualización completado."
           STOP RUN.

       PROCESAR-VENTAS.
           PERFORM UNTIL WS-EOF = "S"
               READ VENTAS-DIARIAS
                   AT END
                       MOVE "S" TO WS-EOF
                   NOT AT END
                       PERFORM ACTUALIZAR-STOCK
               END-READ
           END-PERFORM.

       ACTUALIZAR-STOCK.
           MOVE V-VENTA-PRODUCTO-ID TO PRODUCTO-ID
           READ PRODUCTO
               INVALID KEY
                   DISPLAY "Producto no encontrado: "
                   V-VENTA-PRODUCTO-ID
               NOT INVALID KEY
                   SUBTRACT V-UNIDADES-VENDIDAS FROM PRODUCTO-STOCK
                   IF PRODUCTO-STOCK < 0
                       MOVE 0 TO PRODUCTO-STOCK
                   END-IF
                   REWRITE PRODUCTO-REGISTRO
           END-READ.
