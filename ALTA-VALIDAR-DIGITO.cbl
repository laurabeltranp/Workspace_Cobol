       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALTA-PRODUCTO.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRODUCTO
               ASSIGN TO "PRODUCTO-INDEXADO.dat"
               ORGANIZATION IS INDEXED
               RECORD KEY IS PRODUCTO-ID
               ACCESS MODE IS DYNAMIC
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
       01 WS-ERROR-FLAG PIC A VALUE 'N'.
       01 SINO PIC A VALUE 'S'.
       01 DIGITO PIC 9 VALUE 0.

      * Campos temporales para nueva entrada
       01 NUEVO-PRODUCTO-ID PIC 9(6).
       01 NUEVO-PRODUCTO-DESCRIP PIC X(30).
       01 NUEVO-PRODUCTO-PRECIO PIC 9(5)V99.
       01 NUEVO-PRODUCTO-STOCK PIC 9(6).
       01 NUEVO-PRODUCTO-CAD-AAAA PIC 9(4).
       01 NUEVO-PRODUCTO-CAD-MM PIC 9(2).
       01 NUEVO-PRODUCTO-CAD-DD PIC 9(2).
       01 NUEVO-PRODUCTO-CRITICO PIC 9(6).
       01 NUEVO-PRODUCTO-ALTA-AAAA PIC 9(4).
       01 NUEVO-PRODUCTO-ALTA-MM PIC 9(2).
       01 NUEVO-PRODUCTO-ALTA-DD PIC 9(2).

       PROCEDURE DIVISION.
           OPEN I-O PRODUCTO.
           IF WS-FILE-STATUS = "35"
               DISPLAY "El archivo no existe. Se crea uno nuevo."
               OPEN OUTPUT PRODUCTO
               CLOSE PRODUCTO
               OPEN I-O PRODUCTO
           END-IF

           IF WS-FILE-STATUS NOT = "00"
               DISPLAY "Error al abrir el archivo. Código de estado: "
               DISPLAY WS-FILE-STATUS
               CLOSE PRODUCTO
               STOP RUN
           END-IF

           PERFORM UNTIL SINO NOT = "S"
               MOVE 'N' TO WS-ERROR-FLAG

               DISPLAY "Ingrese el ID del producto: "
               ACCEPT NUEVO-PRODUCTO-ID

               IF NUEVO-PRODUCTO-ID NOT NUMERIC
                   DISPLAY "Error: ID debe ser numérico."
                   MOVE 'S' TO WS-ERROR-FLAG
               END-IF

               MOVE NUEVO-PRODUCTO-ID TO PRODUCTO-ID
               READ PRODUCTO
                   INVALID KEY
                       DISPLAY "Ingrese descripción del producto: "
                       ACCEPT NUEVO-PRODUCTO-DESCRIP
                       IF NUEVO-PRODUCTO-DESCRIP NOT ALPHABETIC
                           DISPLAY "Error: Descripción solo  letras."
                           MOVE 'S' TO WS-ERROR-FLAG
                       END-IF

                       DISPLAY "Ingrese el precio del producto: "
                       ACCEPT NUEVO-PRODUCTO-PRECIO
                       IF NUEVO-PRODUCTO-PRECIO NOT NUMERIC
                           DISPLAY "Error: Precio numérico y decimal."
                           MOVE 'S' TO WS-ERROR-FLAG
                       END-IF

                       DISPLAY "Ingrese el STOCK del producto: "
                       ACCEPT NUEVO-PRODUCTO-STOCK
                       IF NUEVO-PRODUCTO-STOCK NOT NUMERIC
                           DISPLAY "Error: Stock debe ser numérico."
                           MOVE 'S' TO WS-ERROR-FLAG
                       END-IF

                       DISPLAY "Ingrese el año de caducidad producto:"
                       ACCEPT NUEVO-PRODUCTO-CAD-AAAA
                       IF NUEVO-PRODUCTO-CAD-AAAA NOT NUMERIC OR
                          NUEVO-PRODUCTO-CAD-AAAA < 2022 OR
                          NUEVO-PRODUCTO-CAD-AAAA > 2100
                           DISPLAY "Error: Año entre 2022 y 2100."
                           MOVE 'S' TO WS-ERROR-FLAG
                       END-IF

                       DISPLAY "Ingrese el mes  caducidad del producto:"
                       ACCEPT NUEVO-PRODUCTO-CAD-MM
                       IF NUEVO-PRODUCTO-CAD-MM NOT NUMERIC OR
                          NUEVO-PRODUCTO-CAD-MM < 1 OR
                          NUEVO-PRODUCTO-CAD-MM > 12
                           DISPLAY "Error: Mes  entre 01 y 12."
                           MOVE 'S' TO WS-ERROR-FLAG
                       END-IF

                       DISPLAY "Ingrese el día  caducidad del producto:"
                       ACCEPT NUEVO-PRODUCTO-CAD-DD
                       IF NUEVO-PRODUCTO-CAD-DD NOT NUMERIC OR
                          NUEVO-PRODUCTO-CAD-DD < 1 OR
                          NUEVO-PRODUCTO-CAD-DD > 31
                           DISPLAY "Error: Día  entre 01 y 31."
                           MOVE 'S' TO WS-ERROR-FLAG
                       END-IF

                       DISPLAY "Ingrese el CRITICO del producto: "
                       ACCEPT NUEVO-PRODUCTO-CRITICO
                       IF NUEVO-PRODUCTO-CRITICO NOT NUMERIC
                           DISPLAY "Error: Crítico debe ser numérico."
                           MOVE 'S' TO WS-ERROR-FLAG
                       END-IF

                       DISPLAY "Ingrese el año de alta del producto: "
                       ACCEPT NUEVO-PRODUCTO-ALTA-AAAA
                       IF NUEVO-PRODUCTO-ALTA-AAAA NOT NUMERIC OR
                          NUEVO-PRODUCTO-ALTA-AAAA < 2022 OR
                          NUEVO-PRODUCTO-ALTA-AAAA > 2100
                           DISPLAY "Error: Año entre 2022 y 2100."
                           MOVE 'S' TO WS-ERROR-FLAG
                       END-IF

                       DISPLAY "Ingrese el mes de alta del producto: "
                       ACCEPT NUEVO-PRODUCTO-ALTA-MM
                       IF NUEVO-PRODUCTO-ALTA-MM NOT NUMERIC OR
                          NUEVO-PRODUCTO-ALTA-MM < 1 OR
                          NUEVO-PRODUCTO-ALTA-MM > 12
                           DISPLAY "Error: Mes entre 01 y 12."
                           MOVE 'S' TO WS-ERROR-FLAG
                       END-IF

                       DISPLAY "Ingrese el día de alta del producto: "
                       ACCEPT NUEVO-PRODUCTO-ALTA-DD
                       IF NUEVO-PRODUCTO-ALTA-DD NOT NUMERIC OR
                          NUEVO-PRODUCTO-ALTA-DD < 1 OR
                          NUEVO-PRODUCTO-ALTA-DD > 31
                           DISPLAY "Error: Día entre 01 y 31."
                           MOVE 'S' TO WS-ERROR-FLAG
                       END-IF

                   IF WS-ERROR-FLAG = 'N'
                   CALL "DigitoC" USING PRODUCTO-ID, DIGITO
                   MOVE DIGITO TO PRODUCTO-CONTROL
                   MOVE NUEVO-PRODUCTO-DESCRIP TO PRODUCTO-DESCRIP
                   MOVE NUEVO-PRODUCTO-PRECIO TO PRODUCTO-PRECIO
                   MOVE NUEVO-PRODUCTO-STOCK TO PRODUCTO-STOCK
               MOVE NUEVO-PRODUCTO-CAD-AAAA TO PRODUCTO-CADUCIDAD(1:4)
                   MOVE NUEVO-PRODUCTO-CAD-MM TO PRODUCTO-CADUCIDAD(5:2)
                   MOVE NUEVO-PRODUCTO-CAD-DD TO PRODUCTO-CADUCIDAD(7:2)
                   MOVE NUEVO-PRODUCTO-CRITICO TO PRODUCTO-CRITICO
                   MOVE NUEVO-PRODUCTO-ALTA-AAAA TO PRODUCTO-ALTA(1:4)
                   MOVE NUEVO-PRODUCTO-ALTA-MM TO PRODUCTO-ALTA(5:2)
                   MOVE NUEVO-PRODUCTO-ALTA-DD TO PRODUCTO-ALTA(7:2)
                   WRITE PRODUCTO-REGISTRO
                       IF WS-FILE-STATUS = "00"
                           DISPLAY "Producto guardado correctamente."
                       ELSE
                           DISPLAY "Error SAVE  el producto. COd STATE:"
                           DISPLAY WS-FILE-STATUS
                       END-IF
                   ELSE
                       DISPLAY "Error en los datos. NO SAVE"
                   END-IF
               END-READ

               DISPLAY "¿Desea agregar otro producto? (S/N): "
               ACCEPT SINO
           END-PERFORM.

           CLOSE PRODUCTO
           STOP RUN.
       END PROGRAM ALTA-PRODUCTO.
