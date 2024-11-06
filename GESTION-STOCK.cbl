       IDENTIFICATION DIVISION.
       PROGRAM-ID. GESTION-STOCK.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRODUCTO
               ASSIGN TO "producto.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PRODUCTO-ID
               FILE STATUS IS FILE-STATUS.

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
       01 OPCION PIC 9 VALUE 0.
       01 FILE-STATUS PIC XX.
       01 FIN PIC X VALUE "N".

        01 ENCABEZADO.
           05 E-PRODUCTO-ID PIC X(20) VALUE "ID".
           05 FILLER PIC X(6) VALUE SPACES.
           05 E-PRODUCTO-CONTROL PIC X(20) VALUE "CONTROL".
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
           05 R-PRODUCTO-CONTROL PIC X(20).
           05 FILLER PIC X(6) VALUE SPACES.
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
       MAIN-PROGRAM.
           PERFORM UNTIL FIN = "S"
           DISPLAY "-------------------------------"
           DISPLAY "BIENVENIDO AL SISTEMA DE GESTION DE STOCK"
           DISPLAY "1. CONSULTA PRODUCTO."
           DISPLAY "2. ALTA DE  PRODUCTO."
           DISPLAY "3. MODIFICAR PRODUCTO."
           DISPLAY "4. BAJA PRODUCTO."
           DISPLAY "5. IMPRIMIR ST-PRODUCTO."
           DISPLAY "6. IMPRIMIR ARCHIVO VT-VENTAS-DIARIAS."
           DISPLAY "7. PRODUCTOS BAJO MINIMOS - STOCK CRITICOS."
           DISPLAY "8. PRODUCTOS A 30 DIAS DE VENCIMIENTO."
           DISPLAY "9. ALTAS PRODCTOS BATCH DESDE VT-VENTAS-DIARIAS."
           DISPLAY "0. SALIR."

               DISPLAY "INGRESE SU OPCION: "
               ACCEPT OPCION.

            EVALUATE OPCION
               WHEN 1
                   CALL "BUSCAR-UN-PRODUCTO"
                   ON EXCEPTION
                       DISPLAY "ERROR AL ALLAMAR A LA CONSULTA"
                       END-CALL
               WHEN 2
                   CALL "ALTA-PRODUCTO"
               WHEN 3
                   DISPLAY "MODIFICAR-PRODUCTO"
               WHEN 4
                   CALL "BAJA-PRODUCTO"
               WHEN 5
                   DISPLAY "MOSTRANDO TODOS LOS PRODUCTOS"
                   DISPLAY ENCABEZADO.
                   OPEN  INPUT PRODUCTO.
                   PERFORM UNTIL WS-EOF = "S"

                   READ PRODUCTO
                   AT END
                   MOVE "S" TO WS-EOF
                   NOT AT END

                   MOVE PRODUCTO-ID TO R-PRODUCTO-ID
                   MOVE PRODUCTO-CONTROL TO R-PRODUCTO-CONTROL
                   MOVE PRODUCTO-DESCRIP TO R-PRODUCTO-DESCRIP
                   MOVE PRODUCTO-PRECIO TO R-PRODUCTO-PRECIO
                   MOVE PRODUCTO-STOCK TO R-PRODUCTO-STOCK
                   MOVE PRODUCTO-CADUCIDAD TO R-PRODUCTO-CADUCIDAD
                   MOVE PRODUCTO-CRITICO TO R-PRODUCTO-CRITICO
                   DISPLAY IMPRIMIR
                   END-READ
                   END-PERFORM

                   CLOSE PRODUCTO

      *         WHEN 6
      *             CALL "IMPRIMIR-ARCHIVO-VI-VENTAS"
      *         WHEN 7
      *             CALL "PRODUCTOS-STOCK-CRITICO"
      *         WHEN 8
      *             CALL "PRODUCTOS-VENCIMIENTO"
      *         WHEN 9
      *             CALL "ALTA-PRODUCTOS-BATCH"
               WHEN 0
                   MOVE "S" TO FIN
               WHEN OTHER
                   DISPLAY "OPCION NO VALIDA, INTENTELO DE NUEVO"
               END-EVALUATE
           END-PERFORM.

            STOP RUN.
       END PROGRAM GESTION-STOCK.
