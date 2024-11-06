       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT PRODUCTO ASSIGN TO 'PRODUCTO-INDEXADO.dat'
        ORGANIZATION IS INDEXED
        ACCESS MODE IS DYNAMIC
        RECORD KEY IS PRODUCTO-ID
        FILE STATUS IS FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  PRODUCTO.
       01 PRODUCTO-REGISTRO.
               03 PRODUCTO-ID PIC 9(6).
               03 PRODUCTO-CONTROL PIC 9.
               03 PRODUCTO-DESCRIP PIC X(35).
               03 PRODUCTO-PRECIO PIC 9(4)V99.
               03 PRODUCTO-STOCK PIC 9(6).
               03 PRODUCTO-CADUCIDAD.
                   05 PRODUCTO-CAD-AAAA PIC 9(4).
                   05 PRODUCTO-CAD-MM PIC 99.
                   05 PRODUCTO-CAD-DD PIC 99.
               03 PRODUCTO-CRITICO PIC 9(6).
               03 PRODUCTO-ALTA-FECH.
                   05 PRODUCTO-ALTA-AAAA PIC 9(4).
                   05 PRODUCTO-ALTA-MM PIC 99.
                   05 PRODUCTO-ALTA-DD PIC 99.

       WORKING-STORAGE SECTION.

       01 WS-ERROR-FLAG PIC A VALUE 'N'.
       01 FILE-STATUS            PIC XX VALUE SPACES.
       01 OPCION                 PIC X VALUE 'S'.
       01 NUEVO-REGISTRO-PRODUCTO.
           03 NUEVO-PRODUCTO-ID PIC 9(6).
           03 NUEVO-PRODUCTO-CONTROL PIC 9.
           03 NUEVO-PRODUCTO-DESCRIP PIC X(35).
           03 NUEVO-PRODUCTO-PRECIO PIC 9(4)V99.
           03 NUEVO-PRODUCTO-STOCK PIC 9(6).
           03 NUEVO-PRODUCTO-CADUCIDAD.
               05 NUEVO-PRODUCTO-CAD-AAAA PIC 9(4).
               05 NUEVO-PRODUCTO-CAD-MM PIC 99.
               05 NUEVO-PRODUCTO-CAD-DD PIC 99.
           03 NUEVO-PRODUCTO-CRITICO PIC 9(6).
           03 NUEVO-PRODUCTO-ALTA-FECH.
               05 NUEVO-PRODUCTO-ALTA-AAAA PIC 9(4).
               05 NUEVO-PRODUCTO-ALTA-MM PIC 99.
               05 NUEVO-PRODUCTO-ALTA-DD PIC 99.
       01 SINO PIC X VALUE "S".
       PROCEDURE DIVISION.
       INICIO.
      * Intentar abrir el archivo en modo I-O
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
        STOP RUN
       END-IF.

       PERFORM UNTIL OPCION NOT = "S"
        DISPLAY "Ingrese el ID del producto: "
        ACCEPT NUEVO-PRODUCTO-ID

      * Intentar leer el registro para verificar duplicado
        MOVE NUEVO-PRODUCTO-ID TO PRODUCTO-ID
               READ PRODUCTO
                   INVALID KEY
                       DISPLAY "Ingrese descripción del producto: "

                       ACCEPT NUEVO-PRODUCTO-DESCRIP
                       IF NUEVO-PRODUCTO-DESCRIP NOT ALPHABETIC
                           DISPLAY "Error:  solo debe contener letras."
                           MOVE 'S' TO WS-ERROR-FLAG
                       END-IF

                       DISPLAY "Ingrese el precio del producto: "

                       ACCEPT NUEVO-PRODUCTO-PRECIO
                       IF NUEVO-PRODUCTO-PRECIO NOT NUMERIC
                           DISPLAY "Error:  debe ser numérico."
                           MOVE 'S' TO WS-ERROR-FLAG
                       END-IF

                       DISPLAY "Ingrese el STOCK del producto: "

                       ACCEPT NUEVO-PRODUCTO-STOCK
                       IF NUEVO-PRODUCTO-STOCK NOT NUMERIC
                           DISPLAY "Error: Stock debe ser numérico."
                           MOVE 'S' TO WS-ERROR-FLAG
                       END-IF

                       DISPLAY "Ingrese AÑO DE CADUCIDAD del producto: "

                       ACCEPT NUEVO-PRODUCTO-CAD-AAAA
                       IF NUEVO-PRODUCTO-CAD-AAAA
                           NOT NUMERIC OR NUEVO-PRODUCTO-CAD-AAAA < 2022
                           OR NUEVO-PRODUCTO-CAD-AAAA > 2100
                   DISPLAY "Error: Año de caducidad debe ser numérico"
                           MOVE 'S' TO WS-ERROR-FLAG
                       END-IF

                       DISPLAY "Ingrese MES CADUCIDAD del producto: "

                       ACCEPT NUEVO-PRODUCTO-CAD-MM
                       IF NUEVO-PRODUCTO-CAD-MM
                           NOT NUMERIC OR NUEVO-PRODUCTO-CAD-MM < 1
                           OR NUEVO-PRODUCTO-CAD-MM > 12
               DISPLAY "Error: Mes de caducidad debe ser entre 01 y 12."
                           MOVE 'S' TO WS-ERROR-FLAG
                       END-IF

                       DISPLAY "Ingrese DIA CADUCIDAD del producto: "

                       ACCEPT NUEVO-PRODUCTO-CAD-DD
                       IF NUEVO-PRODUCTO-CAD-DD
                           NOT NUMERIC OR NUEVO-PRODUCTO-CAD-DD < 1
                           OR NUEVO-PRODUCTO-CAD-DD > 31
               DISPLAY "Error: Día de caducidad debe ser entre 01 y 31."
                           MOVE 'S' TO WS-ERROR-FLAG
                       END-IF

                       DISPLAY "Ingrese el CRITICO del producto: "

                       ACCEPT NUEVO-PRODUCTO-CRITICO
                       IF NUEVO-PRODUCTO-CRITICO NOT NUMERIC
                           DISPLAY "Error: Crítico debe ser numérico."
                           MOVE 'S' TO WS-ERROR-FLAG
                       END-IF

                       DISPLAY "Ingrese AÑO ALTA del producto: "

                       ACCEPT NUEVO-PRODUCTO-ALTA-AAAA
                       IF NUEVO-PRODUCTO-ALTA-AAAA
                       NOT NUMERIC OR NUEVO-PRODUCTO-ALTA-AAAA < 2022
                           OR NUEVO-PRODUCTO-ALTA-AAAA > 2100
                   DISPLAY "Error: Año de alta debe ser numérico."
                           MOVE 'S' TO WS-ERROR-FLAG
                       END-IF

                       DISPLAY "Ingrese MES ALTA del producto: "

                       ACCEPT NUEVO-PRODUCTO-ALTA-MM
                       IF NUEVO-PRODUCTO-ALTA-MM
                           NOT NUMERIC OR NUEVO-PRODUCTO-ALTA-MM < 1
                           OR NUEVO-PRODUCTO-ALTA-MM > 12
                   DISPLAY "Error: Mes de alta debe ser entre 01 y 12."
                           MOVE 'S' TO WS-ERROR-FLAG
                       END-IF

                       DISPLAY "Ingrese DIA ALTA del producto: "

                       ACCEPT NUEVO-PRODUCTO-ALTA-DD
                       IF NUEVO-PRODUCTO-ALTA-DD
                           NOT NUMERIC OR NUEVO-PRODUCTO-ALTA-DD < 1
                           OR NUEVO-PRODUCTO-ALTA-DD > 31
                   DISPLAY "Error: Día de alta debe ser entre 01 y 31."
                           MOVE 'S' TO WS-ERROR-FLAG
                       END-IF

           IF WS-ERROR-FLAG = 'N'
               MOVE NUEVO-PRODUCTO-ID TO PRODUCTO-ID
               MOVE NUEVO-PRODUCTO-DESCRIP TO PRODUCTO-DESCRIP
               MOVE NUEVO-PRODUCTO-PRECIO TO PRODUCTO-PRECIO
               MOVE NUEVO-PRODUCTO-STOCK TO PRODUCTO-STOCK
               MOVE NUEVO-PRODUCTO-CAD-AAAA TO PRODUCTO-CADUCIDAD(1:4)
               MOVE NUEVO-PRODUCTO-CAD-MM TO PRODUCTO-CADUCIDAD(5:2)
               MOVE NUEVO-PRODUCTO-CAD-DD TO PRODUCTO-CADUCIDAD(7:2)
               MOVE NUEVO-PRODUCTO-CRITICO TO PRODUCTO-CRITICO
               MOVE NUEVO-PRODUCTO-ALTA-AAAA TO PRODUCTO-ALTA-FECH(1:4)
               MOVE NUEVO-PRODUCTO-ALTA-MM TO PRODUCTO-ALTA-FECH(5:2)
               MOVE NUEVO-PRODUCTO-ALTA-DD TO PRODUCTO-ALTA-FECH(7:2)

               WRITE PRODUCTO-REGISTRO
               IF FILE-STATUS = "00"
                   DISPLAY "Producto guardado correctamente."
               ELSE
               DISPLAY "Producto no guardado. Código de estado: "
               FILE-STATUS
               STOP RUN
               END-IF
               ELSE
           DISPLAY "Producto no registrado."
               END-IF
               NOT INVALID KEY
                   DISPLAY "Error: El ID del producto ya existe."
           END-READ

               DISPLAY "¿Desea agregar otro producto? (S/N): "

               ACCEPT SINO
               STOP RUN
       END-PERFORM.


       STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
