IDENTIFICATION DIVISION.
       PROGRAM-ID. RAND-AVG.
       AUTHOR. LANGUAGE RACE.
       DATE-WRITTEN. 2025-12-31.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTPUT-FILE ASSIGN TO "../rand_avg output/random_numbers.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD OUTPUT-FILE.
       01 OUTPUT-LINE.
          05 NUM-PIC PIC 9(3).
       
       WORKING-STORAGE SECTION.
       01 WS-VARIABLES.
          05 COUNT PIC 9(4) VALUE 1000.
          05 RANDOM-NUMBER PIC 9(3).
          05 SUM PIC 9(8)V99.
          05 MEAN PIC 9(8)V99.
          05 I PIC 9(4).
          05 SEED PIC 9(8).
          05 OUTPUT-DIR PIC X(20) VALUE "../rand_avg output".
       
       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM HELLO-WORLD.
           PERFORM CREATE-OUTPUT-DIR.
           PERFORM GENERATE-RANDOM-NUMBERS.
           PERFORM CALCULATE-MEAN.
           PERFORM DISPLAY-RESULTS.
           STOP RUN.
       
       HELLO-WORLD.
           DISPLAY "Hello, World!".
       
       CREATE-OUTPUT-DIR.
           CALL "SYSTEM" USING "mkdir -p '../rand_avg output'".
       
       GENERATE-RANDOM-NUMBERS.
           ACCEPT SEED FROM TIME.
           MOVE SEED TO RANDOM-NUMBER.
           OPEN OUTPUT OUTPUT-FILE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > COUNT
               COMPUTE RANDOM-NUMBER = FUNCTION RANDOM(FUNCTION MOD(SEED, 1000))
               ADD RANDOM-NUMBER TO SUM
               WRITE OUTPUT-LINE
           END-PERFORM.
           CLOSE OUTPUT-FILE.
       
       CALCULATE-MEAN.
           COMPUTE MEAN = SUM / COUNT.
       
       DISPLAY-RESULTS.
           DISPLAY "Generated 1000 random numbers".
           DISPLAY "Mean: " MEAN.
           DISPLAY "Saved to: ../rand_avg output/random_numbers.txt".
