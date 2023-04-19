=
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TI.
       AUTHOR. Marco De Melo.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT F01-RECIPES-FILE ASSIGN TO "customers.dat"
              ORGANIZATION IS LINE SEQUENTIAL.
       SELECT  F01-VALID-OUTPUT-FILE ASSIGN TO "RecipesGoodRecords.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       SELECT F01-ERROR-OUTPUT-FILE ASSIGN TO "RecipesErrorFile.dat"
              ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD F01-RECIPES-FILE
       RECORD CONTAINS 133 CHARACTERS
       DATA RECORD IS F01-INPUT-RECORD.
       01 F01-INPUT-RECORD.
         05 NAME PIC X(25).
         05 COURSE-TYPE PIC X(10).
         05 PREP-TIME PIC 9(3)V9.
         05 BAKE-TIME PIC 9(3)V9.
         05 INGREDIENTS OCCURS 3 TIMES.
           10 INGREDIENTS-NAME PIC X(15).
           10 AMOUNT PIC 9(3)V99.
           10 AMOUNT-TYPE PIC X(5).

       FD F01-VALID-OUTPUT-FILE
              RECORD CONTAINS 133 CHARACTERS
              DATA RECORD IS F01-VALID-OUTPUT-RECORD.
       01 F01-VALID-OUTPUT-RECORD PIC X(133).
       FD F01-ERROR-OUTPUT-FILE
              RECORD CONTAINS 55 CHARACTERS
                    DATA RECORD IS F01-ERROR-OUTPUT-RECORD.
       01 F01-ERROR-OUTPUT-RECORD PIC X(75).
       WORKING-STORAGE SECTION.
       01 I PIC 9(3) VALUE 0.
       01 W01-SWITCHES.
         05 W01-DATA-REMAINS-SWITCH PIC X(3) VALUE 'YES'.
         05 W01-VALID-RECORD-SWITCH PIC X(3) VALUE SPACES.

       01 W02-VALID-HEADING-LINE.
         05 FILLER PIC X(33) VALUE "VALID RECIPES MARCO DE MELO PAGE ".
         05 PAGE-NUMBER pic 99.
         05 FILLER PIC X(2) VALUE SPACES.
         05 FILLER PIC X(5) VALUE "DATE".
         05 W02-VALID-DATE PIC X(10).
       01 W02-ERROR-HEADING-LINE.
          05 FILLER PIC X(36) VALUE "INVALID RECORDS MARCO DE MELO PAGE ".
          05 PAGE-NUMBER pic 99.
          05 FILLER PIC X(2) VALUE SPACES.
         05 FILLER PIC X(5) VALUE "DATE".
          05 W02-ERROR-DATE PIC X(10).


       01 W02-VALID-REPORT.
         05 NAME-OUT PIC X(25).
         05 COURSE-TYPE-OUT PIC X(10).
         05 PREP-TIME-OUT PIC 9(4)V9.
         05 BAKE-TIME-OUT PIC 9(4)V9.
         05 INGREDIENTS-OUT OCCURS 3 TIMES.
           10 INGREDIENT-NAME-OUT PIC X(15). 
           10 AMOUNT-OUT PIC 9(3)V99.
           10 AMOUNT-TYPE-OUT PIC X(5).

       01 W02-INVALID-REPORT.
         05 NAME-ERR PIC X(25).
         05 ERROR-MSG-ERR PIC X(50).

       01 W02-F00TER-ERRORS.
       
         05 FILLER PIC X(19) VALUE "NUMBER OF ERRORS =".
         05 W02-NUMBER-ERR PIC X(2).
       01 W03-CONSTANTS.
         05 W03-MAX-AMOUNT PIC 9(3) VALUE 500.
         05 W03-COURSETYPE-ERROR PIC X(47) VALUE 'Course type must be Appetizer, Dessert or Main.'.
         05 W03-PREPTIME-NOT-NUMERIC-ERROR PIC X(26) VALUE 'Prep time must be numeric.'.
         05 W03-AMOUNT-NOT-INRANGE-ERROR PIC X(36) VALUE 'Amount must be between 0 and 500.'.
         05 W03-PREPTIME-NOT-INRANGE-ERROR PIC X(36) VALUE 'Prep time must be between 0 and 500.'.

       01 W04-ERROR-FILE-DETAIL-LINE.
         05 WO4-ERROR-MSG PIC X(30).
       01 W05-PAGING-VARIABLES.
         05 W05-LINE-MAX PIC 99 VALUE 3.
         05 W05-LINE-COUNT PIC 99.
       01 W07-NUM-ERRS PIC 9(2) VALUE 0.

       PROCEDURE DIVISION.
           PERFORM 100-START-NEW-PAGE
           PERFORM UNTIL W01-DATA-REMAINS-SWITCH = 'NO'
               PERFORM 200-PROCESS-RECORDS
           END-PERFORM
           PERFORM 300-WRITE-FOOTER.
           STOP RUN.
       200-PROCESS-RECORDS.
           READ F01-RECIPES-FILE
               AT END
                   MOVE 'NO' TO W01-DATA-REMAINS-SWITCH
               NOT AT END
                   PERFORM 210-VALIDATE-FIELDS
                   IF W01-VALID-RECORD-SWITCH = 'YES'
                       MOVE NAME TO NAME-OUT
                       MOVE COURSE-TYPE TO COURSE-TYPE-OUT
                       MOVE PREP-TIME TO PREP-TIME-OUT
                       MOVE BAKE-TIME TO BAKE-TIME-OUT
                       PERFORM VARYING I FROM 1 BY 1
                         UNTIL I > 3
                           MOVE INGREDIENTS-NAME(I) TO INGREDIENT-NAME-OUT(I)
                           MOVE AMOUNT(I) TO AMOUNT-OUT(I)
                           MOVE AMOUNT-TYPE(I) TO AMOUNT-TYPE-OUT(I)
                       END-PERFORM
                       WRITE F01-VALID-OUTPUT-RECORD FROM W02-VALID-REPORT
                   ELSE
                       MOVE NAME TO NAME-ERR
                       MOVE WO4-ERROR-MSG TO ERROR-MSG-ERR
                       ADD 1 TO W07-NUM-ERRS
                       WRITE F01-ERROR-OUTPUT-RECORD FROM W02-INVALID-REPORT
           END-READ.
          
           
                   
       210-VALIDATE-FIELDS.
           MOVE SPACES TO W01-VALID-RECORD-SWITCH
           INSPECT PREP-TIME REPLACING LEADING SPACES BY ZEROS
           IF COURSE-TYPE NOT EQUAL TO "Appetizer" AND
             COURSE-TYPE NOT EQUAL TO "Dessert" AND
             COURSE-TYPE NOT EQUAL TO "Main"
               MOVE W03-COURSETYPE-ERROR TO WO4-ERROR-MSG
               MOVE "NO" TO W01-VALID-RECORD-SWITCH
           ELSE
               IF PREP-TIME NOT NUMERIC
                   MOVE W03-PREPTIME-NOT-NUMERIC-ERROR TO WO4-ERROR-MSG
                   MOVE "NO" TO W01-VALID-RECORD-SWITCH

               ELSE
                   IF PREP-TIME LESS THAN 0 OR GREATER THAN W03-MAX-AMOUNT
                       MOVE W03-PREPTIME-NOT-INRANGE-ERROR TO WO4-ERROR-MSG
                       MOVE "NO" TO W01-VALID-RECORD-SWITCH
                   END-IF
                   PERFORM VARYING I FROM 1 BY 1
                     UNTIL I > 3
                   IF AMOUNT(I) LESS THAN 0 OR AMOUNT(I) GREATER THAN W03-MAX-AMOUNT
                       MOVE W03-AMOUNT-NOT-INRANGE-ERROR TO WO4-ERROR-MSG
                       MOVE "NO" TO W01-VALID-RECORD-SWITCH
                   END-IF
                   end-perform
               END-IF
           END-IF
           IF W01-VALID-RECORD-SWITCH NOT EQUAL TO "NO"
               MOVE "YES" TO W01-VALID-RECORD-SWITCH
           END-IF.
       
       100-START-NEW-PAGE.
           OPEN INPUT F01-RECIPES-FILE.
           OPEN OUTPUT F01-VALID-OUTPUT-FILE.
           OPEN OUTPUT F01-ERROR-OUTPUT-FILE.
           DISPLAY "FILES OPENED".
           MOVE FUNCTION CURRENT-DATE (1:8) TO W02-VALID-DATE.
           MOVE FUNCTION CURRENT-DATE (1:8) TO W02-ERROR-DATE.
           MOVE 1 TO PAGE-NUMBER of W02-VALID-HEADING-LINE
           MOVE 1 TO PAGE-NUMBER of W02-ERROR-HEADING-LINE
           WRITE F01-VALID-OUTPUT-RECORD FROM W02-VALID-HEADING-LINE
           WRITE F01-ERROR-OUTPUT-RECORD FROM W02-ERROR-HEADING-LINE.
          

           

       300-WRITE-FOOTER.
           MOVE W07-NUM-ERRS TO W02-NUMBER-ERR
           WRITE F01-ERROR-OUTPUT-RECORD FROM SPACES
           WRITE F01-ERROR-OUTPUT-RECORD FROM W02-F00TER-ERRORS.
           CLOSE F01-ERROR-OUTPUT-FILE.
           CLOSE F01-RECIPES-FILE.
           CLOSE F01-VALID-OUTPUT-FILE.
           DISPLAY "FILES CLOSED".
      *I spent a considerable amount of time studying how to create different types of reports, including invalid, valid, or a combination of both.
      *To improve my preparation, I could have dedicated more time to learning how to use INSPECT effectively.
      *I also should have put in more effort to acquire a better understanding of tables, which would have been beneficial in this assignment.