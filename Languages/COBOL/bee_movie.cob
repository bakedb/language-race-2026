IDENTIFICATION DIVISION.
       PROGRAM-ID. BEE-MOVIE.
       AUTHOR. LANGUAGE RACE.
       DATE-WRITTEN. 2025-12-31.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "../beemoviescript.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-LINE.
          05 LINE-PIC PIC X(256).
       
       WORKING-STORAGE SECTION.
       01 WS-VARIABLES.
          05 LETTER-COUNTS.
             10 COUNT-A OCCURS 26 TIMES.
                15 COUNT PIC 9(8).
          05 TOTAL-LETTERS PIC 9(8).
          05 CHAR-INDEX PIC 9(3).
          05 LINE-LENGTH PIC 9(3).
          05 SORTED-COUNTS.
             10 SORT-ENTRY OCCURS 26 TIMES.
                15 SORT-LETTER PIC X(1).
                15 SORT-COUNT PIC 9(8).
          05 I PIC 9(3).
          05 J PIC 9(3).
          05 TEMP-LETTER PIC X(1).
          05 TEMP-COUNT PIC 9(8).
       
       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM HELLO-WORLD.
           PERFORM INITIALIZE-COUNTS.
           PERFORM PROCESS-FILE.
           PERFORM DISPLAY-RESULTS.
           STOP RUN.
       
       HELLO-WORLD.
           DISPLAY "Hello, World!".
       
       INITIALIZE-COUNTS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 26
               MOVE 0 TO COUNT(I)
           END-PERFORM.
           MOVE 0 TO TOTAL-LETTERS.
       
       PROCESS-FILE.
           DISPLAY "Bee Movie Script:".
           DISPLAY "--------------------------------------------------".
           
           OPEN INPUT INPUT-FILE.
           PERFORM UNTIL EXIT
               READ INPUT-FILE AT END
                   EXIT PERFORM
               NOT AT END
                   DISPLAY LINE-PIC
                   PERFORM COUNT-LETTERS-IN-LINE
           END-PERFORM.
           CLOSE INPUT-FILE.
       
       COUNT-LETTERS-IN-LINE.
           MOVE 0 TO CHAR-INDEX.
           INSPECT LINE-PIC TALLYING LINE-LENGTH FOR CHARACTERS.
           PERFORM VARYING CHAR-INDEX FROM 1 BY 1 
               UNTIL CHAR-INDEX > LINE-LENGTH
               IF LINE-PIC(CHAR-INDEX:1) IS ALPHABETIC
                   MOVE FUNCTION LOWER-CASE(LINE-PIC(CHAR-INDEX:1)) 
                     TO TEMP-LETTER
                   COMPUTE I = ORD(TEMP-LETTER) - ORD('a') + 1
                   IF I > 0 AND I <= 26
                       ADD 1 TO COUNT(I)
                       ADD 1 TO TOTAL-LETTERS
                   END-IF
               END-IF
           END-PERFORM.
       
       DISPLAY-RESULTS.
           DISPLAY "--------------------------------------------------".
           DISPLAY "Analysis complete.".
           
           IF TOTAL-LETTERS > 0
               PERFORM PREPARE-SORTED-COUNTS
               PERFORM SORT-COUNTS
               PERFORM DISPLAY-TOP-3
           ELSE
               DISPLAY "No letters found in the script."
           END-IF.
       
       PREPARE-SORTED-COUNTS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 26
               MOVE FUNCTION CHAR(I + ORD('a') - 1) TO SORT-LETTER(I)
               MOVE COUNT(I) TO SORT-COUNT(I)
           END-PERFORM.
       
       SORT-COUNTS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 25
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 26 - I
                   IF SORT-COUNT(J) < SORT-COUNT(J + 1)
                       MOVE SORT-LETTER(J) TO TEMP-LETTER
                       MOVE SORT-COUNT(J) TO TEMP-COUNT
                       MOVE SORT-LETTER(J + 1) TO SORT-LETTER(J)
                       MOVE SORT-COUNT(J + 1) TO SORT-COUNT(J)
                       MOVE TEMP-LETTER TO SORT-LETTER(J + 1)
                       MOVE TEMP-COUNT TO SORT-COUNT(J + 1)
                   END-IF
               END-PERFORM
           END-PERFORM.
       
       DISPLAY-TOP-3.
           DISPLAY " ".
           DISPLAY "Top 3 most commonly used letters:".
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
               IF SORT-COUNT(I) > 0
                   DISPLAY I ": '" SORT-LETTER(I) "': " SORT-COUNT(I) " times"
               END-IF
           END-PERFORM.
