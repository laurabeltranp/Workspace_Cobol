       IDENTIFICATION DIVISION.
       PROGRAM-ID. LISTAR-PRODUCTOS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRODUCTO
               ASSIGN TO "PRODUCTO-INDEXADO.dat"
               ORGANIZATION IS INDEXED
               RECORD KEY PRODUCTO-ID
               ACCESS MODE IS DYNAMIC
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
       01 WS-DIGITO-CONTROL-TEMP PIC X(20).
       01 WS-DIGITO-CONTROL PIC 9 VALUE 0.
       01 File-Status PIC XX VALUE SPACES.
       01 ENCABEZADO.
           05 E-PRODUCTO-ID PIC X(20) VALUE "ID".
           05 FILLER PIC X(6) VALUE SPACES.
           05 E-PRODUCTO-CONTROL PIC A(20) VALUE "CONTROL".
           05 FILLER PIC X(6) VALUE SPACES.
           05 E-PRODUCTO-DESCRIP PIC X(20) VALUE "DESCRIPCION".
           05 FILLER PIC X(6) VALUE SPACES.
           05 E-PRODUCTO-PRECIO PIC X(20) VALUE "PRECIO".
           05 FILLER PIC X(6) VALUE SPACES.
           05 E-PRODUCTO-STOCK PIC X(20) VALUE "STOCK".
           05 FILLER PIC X(6) VALUE SPACES.
           05 E-PRODUCTO-CADUCIDAD PIC X(20) VALUE "CADUCIDAD".
           05 FILLER PIC X(6) VALUE SPACES.
           05 E-PRODUCTO-CRITICO PIC X(20) VALUE "CRITICO".
           05 FILLER PIC X(6) VALUE SPACES.

       01 IMPRIMIR.
           05 R-PRODUCTO-ID PIC X(20).
           05 FILLER PIC X(6) VALUE SPACES.
           05 R-PRODUCTO-CONTROL PIC A(20).
           05 FILLER PIC A(6) VALUE SPACES.
           05 R-PRODUCTO-DESCRIP PIC X(20).
           05 FILLER PIC X(6) VALUE SPACES.
           05 R-PRODUCTO-PRECIO PIC 9(5)V99.
           05 FILLER PIC X(6) VALUE SPACES.
           05 R-PRODUCTO-STOCK PIC X(20).
           05 FILLER PIC X(6) VALUE SPACES.
           05 R-PRODUCTO-CADUCIDAD PIC X(20).
           05 FILLER PIC X(6) VALUE SPACES.
           05 R-PRODUCTO-CRITICO PIC X(20).
           05 FILLER PIC X(6) VALUE SPACES.
       01 WS-EOF PIC A(1).


       PROCEDURE DIVISION.
       OPEN I-O PRODUCTO.
       IF FILE-STATUS = "35"
        DISPLAY "El archivo no existe. Se procede a crearlo."
        OPEN OUTPUT PRODUCTO
        CLOSE PRODUCTO
      * Reintentar abrir en modo I-O
        OPEN I-O PRODUCTO
       END-IF

      * Verificar si el archivo se abrió correctamente
       IF FILE-STATUS NOT = "00"
        DISPLAY "Error al abrir el archivo. Codigo de estado: "
        FILE-STATUS
        CLOSE PRODUCTO
        STOP RUN
       END-IF
            DISPLAY ENCABEZADO.
           OPEN INPUT PRODUCTO.
           PERFORM UNTIL WS-EOF = "S"
               READ PRODUCTO
               AT END
                   MOVE "S" TO WS-EOF
               NOT AT END
                   MOVE PRODUCTO-ID TO R-PRODUCTO-ID

                   MOVE PRODUCTO-DESCRIP TO R-PRODUCTO-DESCRIP
                   MOVE PRODUCTO-PRECIO TO R-PRODUCTO-PRECIO
                   MOVE PRODUCTO-STOCK TO R-PRODUCTO-STOCK
                   MOVE PRODUCTO-CADUCIDAD TO R-PRODUCTO-CADUCIDAD
                   MOVE PRODUCTO-CRITICO TO R-PRODUCTO-CRITICO

      * Llamada al programa DigitoC para calcular el dígito de control
           CALL 'DigitoC' USING PRODUCTO-ID, WS-DIGITO-CONTROL

      * Mover WS-DIGITO-CONTROL a un campo de caracteres para mostrarlo
           MOVE WS-DIGITO-CONTROL TO WS-DIGITO-CONTROL-TEMP
           MOVE WS-DIGITO-CONTROL-TEMP TO R-PRODUCTO-CONTROL
                   DISPLAY IMPRIMIR
           END-READ
           END-PERFORM.

           CLOSE PRODUCTO
           STOP RUN.
       END PROGRAM LISTAR-PRODUCTOS.
