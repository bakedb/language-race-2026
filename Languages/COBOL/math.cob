IDENTIFICATION DIVISION.
       PROGRAM-ID. MATH.
       AUTHOR. LANGUAGE RACE.
       DATE-WRITTEN. 2025-12-31.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "../test_data/math_equations.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT INPUT-FILE-MD ASSIGN TO "../test_data/math_equations.md"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT INPUT-FILE-RAW ASSIGN TO "../test_data/math_equations"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-LINE.
          05 LINE-PIC PIC X(256).
       
       FD INPUT-FILE-MD.
       01 INPUT-LINE-MD.
          05 LINE-PIC-MD PIC X(256).
          
       FD INPUT-FILE-RAW.
       01 INPUT-LINE-RAW.
          05 LINE-PIC-RAW PIC X(256).
       
       WORKING-STORAGE SECTION.
       01 WS-VARIABLES.
          05 EQUATION-STR PIC X(100).
          05 OPERAND-A PIC 9(8)V99.
          05 OPERAND-B PIC 9(8)V99.
          05 OPERATOR PIC X(1).
          05 RESULT PIC 9(8)V99.
          05 EQUATE-POS PIC 9(3).
          05 LINE-LENGTH PIC 9(3).
          05 IS-EQUATION PIC 9(1).
       
       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM HELLO-WORLD.
           PERFORM PROCESS-ALL-FILES.
           STOP RUN.
       
       HELLO-WORLD.
           DISPLAY "Hello, World!".
           DISPLAY " ".
           DISPLAY "Processing math equations...".
       
       PROCESS-ALL-FILES.
           PERFORM PROCESS-TXT-FILE.
           PERFORM PROCESS-MD-FILE.
           PERFORM PROCESS-RAW-FILE.
       
       PROCESS-TXT-FILE.
           OPEN INPUT INPUT-FILE.
           PERFORM UNTIL EXIT
               READ INPUT-FILE AT END
                   EXIT PERFORM
               NOT AT END
                   PERFORM PROCESS-LINE
           END-PERFORM.
           CLOSE INPUT-FILE.
       
       PROCESS-MD-FILE.
           OPEN INPUT INPUT-FILE-MD.
           PERFORM UNTIL EXIT
               READ INPUT-FILE-MD AT END
                   EXIT PERFORM
               NOT AT END
                   PERFORM PROCESS-MD-LINE
           END-PERFORM.
           CLOSE INPUT-FILE-MD.
       
       PROCESS-RAW-FILE.
           OPEN INPUT INPUT-FILE-RAW.
           PERFORM UNTIL EXIT
               READ INPUT-FILE-RAW AT END
                   EXIT PERFORM
               NOT AT END
                   PERFORM PROCESS-LINE
           END-PERFORM.
           CLOSE INPUT-FILE-RAW.
       
       PROCESS-LINE.
           MOVE LINE-PIC TO EQUATION-STR.
           PERFORM CHECK-IF-EQUATION.
           IF IS-EQUATION = 1
               PERFORM SOLVE-EQUATION
           END-IF.
       
       PROCESS-MD-LINE.
           MOVE LINE-PIC-MD TO EQUATION-STR.
           IF EQUATION-STR(1:2) = "- "
               MOVE EQUATION-STR(3:) TO EQUATION-STR
           END-IF.
           PERFORM CHECK-IF-EQUATION.
           IF IS-EQUATION = 1
               PERFORM SOLVE-EQUATION
           END-IF.
       
       CHECK-IF-EQUATION.
           MOVE 0 TO IS-EQUATION.
           INSPECT EQUATION-STR TALLYING LINE-LENGTH FOR CHARACTERS BEFORE SPACE.
           IF LINE-LENGTH > 0 AND EQUATION-STR(1:1) NOT = "#"
               MOVE 1 TO IS-EQUATION
           END-IF.
       
       SOLVE-EQUATION.
           PERFORM FIND-EQUALS-POSITION.
           IF EQUATE-POS > 0
               MOVE EQUATION-STR(1:EQUATE-POS - 1) TO EQUATION-STR
               PERFORM PARSE-AND-CALCULATE
           END-IF.
       
       FIND-EQUALS-POSITION.
           MOVE 1 TO EQUATE-POS.
           PERFORM VARYING EQUATE-POS FROM 1 BY 1
               UNTIL EQUATE-POS > 100 OR EQUATION-STR(EQUATE-POS:1) = "="
                   CONTINUE
           END-PERFORM.
           IF EQUATE-POS > 100
               MOVE 0 TO EQUATE-POS
           END-IF.
       
       PARSE-AND-CALCULATE.
           INITIALIZE OPERAND-A OPERAND-B OPERATOR.
           UNSTRING EQUATION-STR DELIMITED BY SPACE
               INTO OPERAND-A OPERATOR OPERAND-B
           END-UNSTRING.
           
           EVALUATE OPERATOR
               WHEN "+"
                   ADD OPERAND-A TO OPERAND-B GIVING RESULT
               WHEN "-"
                   SUBTRACT OPERAND-B FROM OPERAND-A GIVING RESULT
               WHEN "*"
                   MULTIPLY OPERAND-A BY OPERAND-B GIVING RESULT
               WHEN "/"
                   DIVIDE OPERAND-B INTO OPERAND-A GIVING RESULT
               WHEN OTHER
                   MOVE 0 TO RESULT
           END-EVALUATE.
           
           DISPLAY EQUATION-STR " = " RESULT.
