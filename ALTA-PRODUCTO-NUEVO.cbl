       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT PRODUCTO ASSIGN TO 'PRODUCTO-ALTA-NUEVA.dat'
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
      * Registro no encontrado, es un nuevo producto
                DISPLAY "Ingrese descripcion del producto: "
                ACCEPT NUEVO-PRODUCTO-DESCRIP
                DISPLAY "Ingrese el precio del producto: "
                ACCEPT NUEVO-PRODUCTO-PRECIO
                DISPLAY "Ingrese el STOCK del producto: "
                ACCEPT NUEVO-PRODUCTO-STOCK
                DISPLAY "Ingrese AÑO DE CADUCIDAD del producto: "
                ACCEPT NUEVO-PRODUCTO-CAD-AAAA
                DISPLAY "Ingrese MES CADUCIDAD del producto: "
                ACCEPT NUEVO-PRODUCTO-CAD-MM
                DISPLAY "Ingrese DIA CADUCIDAD del producto: "
                ACCEPT NUEVO-PRODUCTO-CAD-DD
                DISPLAY "Ingrese el CRITICO del producto: "
                ACCEPT NUEVO-PRODUCTO-CRITICO
                DISPLAY "Ingrese el AÑO ALTA del producto: "
                ACCEPT NUEVO-PRODUCTO-ALTA-AAAA
                DISPLAY "Ingrese el MES ALTA del producto: "
                ACCEPT NUEVO-PRODUCTO-ALTA-MM
                DISPLAY "Ingrese el DIA ALTA del producto: "
                ACCEPT NUEVO-PRODUCTO-ALTA-DD

      * Guardar el nuevo registro en el archivo
                MOVE NUEVO-PRODUCTO-ID TO PRODUCTO-ID
                MOVE NUEVO-PRODUCTO-DESCRIP TO PRODUCTO-DESCRIP
                MOVE NUEVO-PRODUCTO-PRECIO TO PRODUCTO-PRECIO
                MOVE NUEVO-PRODUCTO-STOCK TO PRODUCTO-STOCK
                MOVE NUEVO-PRODUCTO-CAD-AAAA TO PRODUCTO-CAD-AAAA
                MOVE NUEVO-PRODUCTO-CAD-MM TO PRODUCTO-CAD-MM
                MOVE NUEVO-PRODUCTO-CAD-DD TO PRODUCTO-CAD-DD
                MOVE NUEVO-PRODUCTO-CRITICO TO PRODUCTO-CRITICO
                MOVE NUEVO-PRODUCTO-ALTA-AAAA TO PRODUCTO-ALTA-AAAA
                MOVE NUEVO-PRODUCTO-ALTA-MM TO PRODUCTO-ALTA-MM
                MOVE NUEVO-PRODUCTO-ALTA-DD TO PRODUCTO-ALTA-DD
                WRITE PRODUCTO-REGISTRO
                IF FILE-STATUS = "00"
                    DISPLAY "Producto guardado correctamente."
                ELSE
                    DISPLAY "Producto no guardado. Codigo de estado: "
                    FILE-STATUS
                END-IF
            NOT INVALID KEY
      * Registro duplicado
                DISPLAY "Error: El ID del producto ya existe."

      * Preguntar si desea ingresar otro registro
        DISPLAY "Desea agregar otro producto? (S/N): "
        ACCEPT OPCION
       END-PERFORM.

      * Cerrar el archivo después de finalizar
       CLOSE PRODUCTO.
       DISPLAY "Archivo cerrado. Fin del programa."
       STOP RUN.
