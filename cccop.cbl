  
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. cccop.
      

      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-INPUT ASSIGN 
           TO "file.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-INPUT.

           SELECT F-OUTPUT ASSIGN TO "CCC.cpy"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-OUTPUT.

      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD  F-INPUT
           RECORD CONTAINS 230 CHARACTERS
           RECORDING MODE IS F.
       01  R-INPUT PIC X(230).

       FD  F-OUTPUT
           RECORD CONTAINS 80 CHARACTERS
           RECORDING MODE IS F.
       01  R-OUTPUT PIC X(80).

       WORKING-STORAGE SECTION.
       01  FS-INPUT PIC X(02).
           88 FS-INPUT-OK VALUE "00".
           88 FS-INPUT-EOF VALUE "10".

       01  FS-OUTPUT PIC X(02).
           88 FS-OUTPUT-OK VALUE "00".

       01  GROUPE.
           03 CNT PIC 9(04) VALUE 0.
           03 ARRAY OCCURS 1 TO 1000 TIMES
                     DEPENDING ON CNT
                     INDEXED BY IDX.
               05 VALEUR PIC X(20).

       01  VAR PIC 9(02) VALUE 1.
       01  WS-IDX PIC 9(2).
       01  WS-IDX2 PIC 9(2).
       01  LONGUEUR pic 99.
       01  CNT-SPACE pic 99.

      ****************************************************************** 
       PROCEDURE DIVISION.
       START-MAIN.
           PERFORM START-R-IP THRU END-R-IP.
       END-MAIN.
           STOP RUN.

      ******************************************************************
       START-R-IP.
           OPEN INPUT F-INPUT
                OUTPUT F-OUTPUT.
           IF FS-INPUT EQUAL "00"
              SET FS-INPUT-OK TO TRUE
               
              PERFORM UNTIL FS-INPUT-EOF
                 READ F-INPUT 
                 AT END 
                    SET FS-INPUT-EOF TO TRUE
                 NOT AT END 
                 MOVE "       01 GROUPE." TO R-OUTPUT
                 WRITE R-OUTPUT
                 ADD 1 TO WS-IDX
                 IF WS-IDX < 21 THEN
                  UNSTRING R-INPUT DELIMITED BY SPACE INTO R-OUTPUT
                  END-UNSTRING
                 
                 COMPUTE VAR = WS-IDX - 1
                 STRING "           03  FILLER PIC X(", 
                    VAR, ") VALUE ", "'", ARRAY(WS-IDX), "'" "."
                    DELIMITED BY SPACE
                    INTO R-OUTPUT
                  END-STRING
                    WRITE R-OUTPUT
                    
                  END-READ
              END-PERFORM
           ELSE
              DISPLAY "ERREUR :" SPACE FS-INPUT
           END-IF.
           CLOSE F-INPUT.
       END-R-IP.
           EXIT.

      ******************************************************************
       START-W-OP.

           INSPECT R-INPUT TALLYING CNT-SPACE FOR ALL " "
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 
           LENGTH OF ARRAY(WS-IDX)
           ADD 1 TO CNT
           END-PERFORM
          
           DISPLAY CNT
           WRITE R-OUTPUT.
       END-W-OP.
           EXIT.
          