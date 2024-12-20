       IDENTIFICATION DIVISION.
       PROGRAM-ID. MATRICES.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  N PIC 9 VALUE 0.
       77 I PIC 9 VALUE 0.
       77 J PIC 9 VALUE 0.

       01 MATRIZ.
           03 MAT OCCURS 3 TIMES.
               05 MAT-IND OCCURS 3 TIMES PIC 9(2).

       PROCEDURE DIVISION.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 3
           DISPLAY "INGRESA EL DATO DE LA PISICION: "
           ACCEPT N MOVE N TO MAT-IND(I,J)
           END-PERFORM
           END-PERFORM

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 3
           DISPLAY I "," J "|" MAT-IND(I,J) "|"
           END-PERFORM
           END-PERFORM
           STOP RUN.
       END PROGRAM MATRICES.
