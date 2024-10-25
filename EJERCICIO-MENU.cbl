       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJERCICIO-MENU.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 NUMERO PIC 9(2).
       01 OPCION PIC 9.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
       PERFORM MENU.

       MENU.
           DISPLAY "==========MENU======="
           DISPLAY "1. INGRESE UN NUMERO"
           DISPLAY "2. MOSTRAR SALUD"
           DISPLAY "3. SALIR"
           DISPLAY "INGRESE OPCION"
           ACCEPT OPCION.

           EVALUATE OPCION
               WHEN 1
               PERFORM INGRESA-NUMERO

               WHEN 2

                PERFORM MOSTRAR-SALUDO

               WHEN 3
                  STOP RUN

               WHEN OTHER
               DISPLAY "error al seleccionar"
           END-EVALUATE.
       PERFORM MENU.

       INGRESA-NUMERO.
         DISPLAY "INGRESE UN NUMERO"
         ACCEPT NUMERO
         DISPLAY "EL NUMERO INGRESADO ES: "NUMERO
         PERFORM MENU.

       MOSTRAR-SALUDO.
           DISPLAY "HOLA BIENVENIDO AL MENU"
           PERFORM MENU

       STOP RUN.

       END PROGRAM EJERCICIO-MENU.
