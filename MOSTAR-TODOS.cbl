       IDENTIFICATION DIVISION.
       PROGRAM-ID. MOSTRAR-TODOS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRODUCTO ASSIGN TO "PRODUCTO-INDEXADO.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PRODUCTO-ID
               FILE STATUS IS WS-FILE-STATUS.

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
           05 PRODUCTO-ALTA PIC 9(8).

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS PIC XX.
       01 WS-EOF PIC A VALUE "N".

       01 ENCABEZADO.
           05 E-PRODUCTO-ID PIC X(10) VALUE "ID".
           05 E-PRODUCTO-CONTROL PIC X(10) VALUE "CONTROL".
           05 E-PRODUCTO-DESCRIP PIC X(30) VALUE "DESCRIPCION".
           05 E-PRODUCTO-PRECIO PIC X(10) VALUE "PRECIO".
           05 E-PRODUCTO-STOCK PIC X(10) VALUE "STOCK".
           05 E-PRODUCTO-CADUCIDAD PIC X(15) VALUE "CADUCIDAD".
           05 E-PRODUCTO-CRITICO PIC X(10) VALUE "CRITICO".
           05 E-PRODUCTO-ALTA PIC X(10) VALUE "ALTA".

       01 IMPRIMIR.
           05 R-PRODUCTO-ID PIC X(10).
           05 R-PRODUCTO-CONTROL PIC X(10).
           05 R-PRODUCTO-DESCRIP PIC X(30).
           05 R-PRODUCTO-PRECIO PIC Z(5),99.
           05 R-PRODUCTO-STOCK PIC X(10).
           05 R-PRODUCTO-CADUCIDAD PIC X(15).
           05 R-PRODUCTO-CRITICO PIC X(10).
           05 R-PRODUCTO-ALTA PIC X(10).

       PROCEDURE DIVISION.

      * Abrir archivo e inicializar
           OPEN INPUT PRODUCTO.
           IF WS-FILE-STATUS NOT = "00"
               DISPLAY "Error al abrir el archivo. Código de estado: "
               WS-FILE-STATUS
               CLOSE PRODUCTO
               STOP RUN
           END-IF

      * Mostrar encabezado
           DISPLAY E-PRODUCTO-ID " " E-PRODUCTO-CONTROL " "
           E-PRODUCTO-DESCRIP " "
                   E-PRODUCTO-PRECIO " " E-PRODUCTO-STOCK " "
                   E-PRODUCTO-CADUCIDAD " "
                   E-PRODUCTO-CRITICO " " E-PRODUCTO-ALTA.

      * Leer y mostrar cada registro
           PERFORM UNTIL WS-EOF = "S"
               READ PRODUCTO
                   AT END
                       MOVE "S" TO WS-EOF
                   NOT AT END
      * Mover los datos del registro a las variables de impresión
                       MOVE PRODUCTO-ID TO R-PRODUCTO-ID
                       MOVE PRODUCTO-CONTROL TO R-PRODUCTO-CONTROL
                       MOVE PRODUCTO-DESCRIP TO R-PRODUCTO-DESCRIP
                       MOVE PRODUCTO-PRECIO TO R-PRODUCTO-PRECIO
                       MOVE PRODUCTO-STOCK TO R-PRODUCTO-STOCK

      * Formatear la fecha de caducidad
                       STRING PRODUCTO-CADUCIDAD(1:4) "/"
                       PRODUCTO-CADUCIDAD(5:2) "/"
                       PRODUCTO-CADUCIDAD(7:2)
                           DELIMITED BY SIZE INTO R-PRODUCTO-CADUCIDAD

      * Formatear la fecha de alta
                       STRING PRODUCTO-ALTA(1:4) "/"
                       PRODUCTO-ALTA(5:2) "/" PRODUCTO-ALTA(7:2)
                           DELIMITED BY SIZE INTO R-PRODUCTO-ALTA

                       MOVE PRODUCTO-CRITICO TO R-PRODUCTO-CRITICO

      * Mostrar el registro formateado con coma decimal en el precio
                       DISPLAY R-PRODUCTO-ID " " R-PRODUCTO-CONTROL " "
                       R-PRODUCTO-DESCRIP " "
                               R-PRODUCTO-PRECIO " "
                               R-PRODUCTO-STOCK " "
                               R-PRODUCTO-CADUCIDAD " "
                               R-PRODUCTO-CRITICO " " R-PRODUCTO-ALTA
               END-READ
           END-PERFORM.

           CLOSE PRODUCTO
           STOP RUN.
       END PROGRAM MOSTRAR-TODOS.
