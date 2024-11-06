       IDENTIFICATION DIVISION.
       PROGRAM-ID. BAJA-PRODUCTO.
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
       01 File-Status PIC XX VALUE SPACES.
       01 SINO PIC X VALUE "S".
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
       END PROGRAM BAJA-PRODUCTO.
