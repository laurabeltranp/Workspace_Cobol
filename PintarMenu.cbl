       IDENTIFICATION DIVISION.
       PROGRAM-ID. Menu.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 OPCION PIC 9.
       PROCEDURE DIVISION.
       INICIO.
           PERFORM MENU.
           STOP RUN.
           MENU.
               DISPLAY "-----SISTEMA DE GTESTION DE STOCK-------"
               DISPLAY "1. CONSULTA PRODUCTO."
               DISPLAY "2. ALTA PRODUCTO."
               DISPLAY "3. MODIFICA PRODUCTO."
               DISPLAY "4. BAJA PRODUCTO."
               DISPLAY "5. IMPRIMIR PRODUCTO."
               DISPLAY "6. ---->IMPIRMIR ARCHIVO VT-VENTAS-DIARIAS <--."
               DISPLAY "7. PRODUCTOS BAJO MINIMOS STOCK CRITICO."
               DISPLAY "8. ALTAS PRODUCTO A 30 DIAS DE VENCER."
               DISPLAY "9. ALTAS PRODUCTOS BATCH DESDE VT-VENTAS-DIAR ."
               DISPLAY "0. SALIR."
               DISPLAY "INGRESE SU OPCION: "
               ACCEPT OPCION.

               EVALUATE OPCION
                   WHEN 1
                       PERFORM OP-1
                   WHEN 2
                       PERFORM OP-2
                   WHEN 3
                       PERFORM OP-3
                   WHEN 4
                       PERFORM OP-4
                   WHEN 5
                       PERFORM OP-5
                   WHEN 6
                       PERFORM OP-6
                   WHEN 7
                       PERFORM OP-7
                   WHEN 8
                       PERFORM OP-8
                   WHEN 9
                       PERFORM OP-9
                   WHEN 0
                       PERFORM OP-0
                       STOP RUN
                   WHEN OTHER
                       DISPLAY "INGRESASTE UN NUMERO ERRONEO"
                       PERFORM MENU
               END-EVALUATE.
               PERFORM MENU.

           OP-1.
               DISPLAY "ELEGISTE OPCION 1".
           OP-2.
               DISPLAY "ELEGISTE OPCION2".
           OP-3.
               DISPLAY "ELEGISTE OPCION3".
           OP-4.
               DISPLAY "ELEGISTE OPCION4".
           OP-5.
               DISPLAY "ELEGISTE OPCION5".
           OP-6.
               DISPLAY "ELEGISTE OPCION6".
           OP-7.
               DISPLAY "ELEGISTE OPCION7".
           OP-8.
               DISPLAY "ELEGISTE OPCION8".
           OP-9.
               DISPLAY "ELEGISTE OPCION9".
           OP-0.
               DISPLAY "ELEGISTE OPCION0".
                DISPLAY "SALIENDOD DEL PROGRAMA, ADIOS".
       END PROGRAM Menu.
