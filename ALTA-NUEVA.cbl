       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
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
               03 PRODUCTO-ID PIC 9(6).
               03 PRODUCTO-CONTROL PIC 9.
               03 PRODUCTO-DESCRIP PIC X(35).
               03 PRODUCTO-PRECIO PIC 9(5)V99.
               03 PRODUCTO-STOCK PIC 9(6).
               03 PRODUCTO-CADUCIDAD.
                   05 PRODUCTO-CAD-AAAA PIC 9(4).
                   05 PRODUCTO-CAD-MM PIC 99.
                   05 PRODUCTO-CAD-DD PIC 99.
               03 PRODUCTO-CRITICO PIC 9(6).
               03 PRODUCTO-ALTA.
                   05 PRODUCTO-ALTA-AAAA PIC 9(4).
                   05 PRODUCTO-ALTA-MM PIC 99.
                   05 PRODUCTO-ALTA-DD PIC 99.
       WORKING-STORAGE SECTION.
       01 DIGITO PIC 9 VALUE 0.
       01 File-Status PIC XX VALUE SPACES.
       01 DIGITO-OK PIC XX VALUE SPACES.
       01 SI-NO PIC X VALUE "S".
       01 EXISTE PIC X VALUE "N".
       01 OPCION PIC 9.
       01 SINO PIC X VALUE "S".
       01 NUEVO-REGISTRO-PRODUCTO.
           03 NUEVO-PRODUCTO-ID PIC 9(6).
           03 NUEVO-PRODUCTO-CONTROL PIC 9.
           03 NUEVO-PRODUCTO-DESCRIP PIC X(35).
           03 NUEVO-PRODUCTO-PRECIO PIC 9(5)V99.
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
           05 R-PRODUCTO-PRECIO PIC ZZZ9.99.
           05 R-PRODUCTO-STOCK PIC X(10).
           05 R-PRODUCTO-CADUCIDAD PIC X(15).
           05 R-PRODUCTO-CRITICO PIC X(10).
           05 R-PRODUCTO-ALTA PIC X(10).
       01 WS-EOF PIC A(1).
       01 CONTINUE-PROG PIC X VALUE "S".
       01 OPCION-MODIFICAR PIC 9.
       01 WS-FILE-STATUS PIC XX.
       01 WS-ERROR-FLAG PIC A VALUE 'N'.
       PROCEDURE DIVISION.
       INICIO.
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
           PERFORM MENU.
           STOP RUN.
           MENU.
               DISPLAY "-----SISTEMA DE GESTION DE STOCK-------"
               DISPLAY "1. ALTA PRODUCTO."
               DISPLAY "2. CONASULTAR PRODUCTOS."
               DISPLAY "3. MODIFICAR PRODUCTO."
               DISPLAY "4. BUSCAR UN PRODUCTO."
               DISPLAY "5. ELIMINAR UN PRODUCTO."
               DISPLAY "6. ACTUALIZACION DESDE VENTAS."
               DISPLAY "7. LISTADO DE PRODUCTOS BAJO MINIMOS."
               DISPLAY "8. LISTADO A MENOS DE 30 DIAS DE VENCER."
               DISPLAY "0. SALIR."
               ACCEPT OPCION

               EVALUATE OPCION
                   WHEN 1
                       PERFORM ALTA-PRODUCTO
                   WHEN 2
                       PERFORM LEER-PRODUCTOS
                   WHEN 3
                       PERFORM MODIFICAR-PRODUCTO
                   WHEN 4
                       PERFORM BUSCAR-UNO
                   WHEN 5
                       PERFORM ELIMINAR-PRODUCTO
                   WHEN 6
                       PERFORM ACTUALIZACION-VENTAS
                   WHEN 7
                       PERFORM PRODUCTO-BAJO-MIN
                   WHEN 8
                       PERFORM PROD-A-VENCER
                   WHEN 0
                       PERFORM SALIR
               WHEN OTHER
                       DISPLAY "INGRESASTE UN NUMERO ERRONEO"
                       PERFORM MENU
               END-EVALUATE.
               PERFORM MENU.
      *----------------------- ALTA PRODUCTOS ----------------------
       ALTA-PRODUCTO.

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


      *----------------------- LEER PRODUCTOS ------------------------
      * Leer y mostrar cada registro
              LEER-PRODUCTOS.
           DISPLAY "MOSTRANDO TODOS LOS PRODUCTOS:"

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

                   MOVE PRODUCTO-ID TO R-PRODUCTO-ID
                   MOVE PRODUCTO-CONTROL TO R-PRODUCTO-CONTROL
                   MOVE PRODUCTO-DESCRIP TO R-PRODUCTO-DESCRIP
                   MOVE PRODUCTO-STOCK TO R-PRODUCTO-STOCK
                   MOVE PRODUCTO-CRITICO TO R-PRODUCTO-CRITICO

      * Ajustar el precio para que siempre muestre dos decimales con ceros significativos
                   MOVE PRODUCTO-PRECIO TO R-PRODUCTO-PRECIO

      * Formatear la fecha de caducidad
       STRING PRODUCTO-CAD-AAAA "/" PRODUCTO-CAD-MM "/" PRODUCTO-CAD-DD
                       DELIMITED BY SIZE INTO R-PRODUCTO-CADUCIDAD

      * Formatear la fecha de alta
       STRING PRODUCTO-ALTA-AAAA "/" PRODUCTO-ALTA-MM "/"
       PRODUCTO-ALTA-DD
                       DELIMITED BY SIZE INTO R-PRODUCTO-ALTA

      * Mostrar el registro formateado con los decimales en el precio
                   DISPLAY R-PRODUCTO-ID " " R-PRODUCTO-CONTROL " "
                           R-PRODUCTO-DESCRIP " "
                           R-PRODUCTO-PRECIO " " R-PRODUCTO-STOCK " "
                           R-PRODUCTO-CADUCIDAD " "
                           R-PRODUCTO-CRITICO " " R-PRODUCTO-ALTA
           END-READ
       END-PERFORM.
        DISPLAY " ".
      *----------------------- MODIFICAR PRODUCTOS --------------------
       MODIFICAR-PRODUCTO.
       PERFORM UNTIL CONTINUE-PROG = "N"

       DISPLAY "Ingrese el ID del producto: "
        ACCEPT PRODUCTO-ID

        READ PRODUCTO
        INVALID KEY
        DISPLAY "PRODUCTO NO ENCONTRADO. CERRANDO PROGRAMA"
        MOVE "N" TO CONTINUE-PROG
        NOT INVALID KEY
        DISPLAY "PRODUCTO ENCONTRADO: "
        DISPLAY "ID: " PRODUCTO-ID
        DISPLAY "1. DESCRIPCION: " PRODUCTO-DESCRIP
        DISPLAY "2. PRECIO: " PRODUCTO-PRECIO
        DISPLAY "3. STOCK: " PRODUCTO-STOCK
        DISPLAY "4. CADUCIDAD ANIO: " PRODUCTO-CAD-AAAA
        DISPLAY "5. CADUCIDAD MES: " PRODUCTO-CAD-MM
        DISPLAY "6. CADUCIDAD DIA: " PRODUCTO-CAD-DD
        DISPLAY "2. CRITICO: " PRODUCTO-CRITICO
        DISPLAY "4. ALTA ANIO: " PRODUCTO-ALTA-AAAA
        DISPLAY "5. ALTA MES: " PRODUCTO-ALTA-MM
        DISPLAY "6. ALTA DIA: " PRODUCTO-ALTA-DD

        DISPLAY "¿QUE DESEA MODIFICAR? SELECCIONE EL NUMERO:"
        ACCEPT OPCION-MODIFICAR

        EVALUATE OPCION-MODIFICAR
        WHEN 1
           DISPLAY "INGRESE LA NUEVA DESCRIPCION: "
           ACCEPT NUEVO-PRODUCTO-DESCRIP
           MOVE NUEVO-PRODUCTO-DESCRIP TO PRODUCTO-DESCRIP
        WHEN 2
           DISPLAY "INGRESE EL NUEVO PRECIO: "
           ACCEPT NUEVO-PRODUCTO-DESCRIP
           MOVE NUEVO-PRODUCTO-PRECIO TO PRODUCTO-PRECIO
       WHEN 3
           DISPLAY "INGRESE EL NUEVO STOCK: "
           ACCEPT NUEVO-PRODUCTO-STOCK
           MOVE NUEVO-PRODUCTO-STOCK TO PRODUCTO-STOCK
       WHEN 4
           DISPLAY "INGRESE NUEVA CADUCIDAD ANIO: "
           ACCEPT NUEVO-PRODUCTO-CAD-AAAA
           MOVE NUEVO-PRODUCTO-CAD-AAAA TO PRODUCTO-CAD-AAAA
       WHEN 5
           DISPLAY "INGRESE NUEVA CADUCIDAD MES: "
           ACCEPT NUEVO-PRODUCTO-CAD-MM
           MOVE NUEVO-PRODUCTO-CAD-MM TO PRODUCTO-CAD-MM
       WHEN 6
           DISPLAY "INGRESE NUEVA CADUCIDAD DIA: "
           ACCEPT NUEVO-PRODUCTO-CAD-DD
           MOVE NUEVO-PRODUCTO-CAD-DD TO PRODUCTO-CAD-DD

       WHEN 7
           DISPLAY "INGRESE NUEVO CRITICO PRODUCTO: "
           ACCEPT NUEVO-PRODUCTO-CRITICO
           MOVE NUEVO-PRODUCTO-CRITICO TO PRODUCTO-CRITICO
       WHEN 8
       DISPLAY "INGRESE NUEVA ALTA ANIO: "
           ACCEPT NUEVO-PRODUCTO-ALTA-AAAA
           MOVE NUEVO-PRODUCTO-ALTA-AAAA TO PRODUCTO-ALTA-AAAA
       WHEN 9
           DISPLAY "INGRESE NUEVA ALTA MES: "
           ACCEPT NUEVO-PRODUCTO-ALTA-MM
           MOVE NUEVO-PRODUCTO-ALTA-MM TO PRODUCTO-ALTA-MM
       WHEN 10
           DISPLAY "INGRESE NUEVA ALTA DIA: "
           ACCEPT NUEVO-PRODUCTO-ALTA-DD
           MOVE NUEVO-PRODUCTO-ALTA-DD TO PRODUCTO-ALTA-DD

       WHEN OTHER
           DISPLAY "NUMERO INTRODUCIDO NO VALIDO : "
           MOVE "N" TO CONTINUE-PROG
       END-EVALUATE
       REWRITE PRODUCTO-REGISTRO

       DISPLAY "¿DESEA REALIZAR OTRA MODIFICACION? S/N"
       ACCEPT CONTINUE-PROG

       END-READ
       END-PERFORM.

      *----------------------- ELIMINAR PRODUCTOS ----------------------
       ELIMINAR-PRODUCTO.
       PERFORM UNTIL SINO NOT = "S"
       DISPLAY "INTRODUZCA EL ID DEL PRODUCTO DESEADO A ELIMINAR:"
       ACCEPT PRODUCTO-ID

       READ PRODUCTO
       INVALID KEY
            DISPLAY "Producto no existe."
       NOT INVALID KEY
            DELETE PRODUCTO
      * Registro duplicado
                DISPLAY "borrando producto."
       end-read
       DISPLAY "Desea agregar otro producto? (S/N): "
        ACCEPT SINO
       stop run
       END-PERFORM.

      *----------------------- FIND ONE PRODUCTO ----------------------
       BUSCAR-UNO.
       PERFORM UNTIL CONTINUE-PROG = "N"

       DISPLAY "Ingrese el ID del producto: "
        ACCEPT PRODUCTO-ID

        READ PRODUCTO
        INVALID KEY
        DISPLAY "PRODUCTO NO ENCONTRADO. CERRANDO PROGRAMA"
        MOVE "N" TO CONTINUE-PROG
        NOT INVALID KEY
        DISPLAY "PRODUCTO ENCONTRADO: "
        DISPLAY "ID: " PRODUCTO-ID
        DISPLAY "1. DESCRIPCION: " PRODUCTO-DESCRIP
        DISPLAY "2. PRECIO: " PRODUCTO-PRECIO
        DISPLAY "3. STOCK: " PRODUCTO-STOCK
        DISPLAY "4. CADUCIDAD ANIO: " PRODUCTO-CAD-AAAA
        DISPLAY "5. CADUCIDAD MES: " PRODUCTO-CAD-MM
        DISPLAY "6. CADUCIDAD DIA: " PRODUCTO-CAD-DD
        DISPLAY "2. CRITICO: " PRODUCTO-CRITICO
        DISPLAY "4. ALTA ANIO: " PRODUCTO-ALTA-AAAA
        DISPLAY "5. ALTA MES: " PRODUCTO-ALTA-MM
        DISPLAY "6. ALTA DIA: " PRODUCTO-ALTA-DD

       DISPLAY "¿DESEA REALIZAR OTRA MODIFICACION? S/N"
       ACCEPT CONTINUE-PROG
       END-READ
       END-PERFORM.


       ACTUALIZACION-VENTAS.



       PRODUCTO-BAJO-MIN.


       PROD-A-VENCER.

       SALIR.
           DISPLAY "CERRANDO PROGRAMA"
           CLOSE PRODUCTO.
           STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
