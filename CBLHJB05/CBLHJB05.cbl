       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBLHJB05.
	   AUTHOR. HARRISON BIRKNER.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
	   FILE SECTION.
	 
       WORKING-STORAGE SECTION.
	   
       PROCEDURE DIVISION.
       L1-MAIN.
       PERFORM L2-INIT.
       PERFORM L2-MAINLINE
         UNTIL MORE-RECS = 'N'.
       PERFORM L2-CLOSING.
       STOP RUN.
 
       L2-INIT.
       OPEN INPUT POP-INPUT.
       OPEN OUTPUT PRTOUT.
	      OPEN OUTPUT ERROUT.
       MOVE FUNCTION CURRENT-DATE        TO CURRENT-DATE-AND-TIME.
       MOVE CURRENT-MONTH                TO TITLE-MONTH.
       MOVE CURRENT-DAY                  TO TITLE-DAY.
        MOVE CURRENT-YEAR                 TO TITLE-YEAR.
     
       PERFORM L3-INIT-HEADING.
       PERFORM L9-READ-INPUT.

       L2-MAINLINE.
	      PERFORM L3-VALIDATION
		THRU L3-VALIDATION-EXIT.
	      IF ERROR-SW = 'N'
		PERFORM L3-CALC
		PERFORM L3-MOVE-PRINT
	      ELSE
		PERFORM L3-ERROR-PRINT.
	      PERFORM L3-READ.
 
       L2-CLOSING.
       PERFORM L3-TOTALS.
	      PERFORM L3-ERR-TOTALS.
       CLOSE POP-INPUT.
       CLOSE PRTOUT.
	      CLOSE ERROUT.

       L3-INIT-HEADING.
      *THIS PARAGRAPH FUNCTIONS SIMILARLY TO L4-HEADING, EXCEPT IT IS
      *ONLY RUN ONCE AT THE BEGINNING OF THE PROGRAM, DOES NOT PRINT
      *AN EXTRA 60 BLANK LINES, AND PRINTS INIT HEADINGS FOR ERROR REPORT.
       ADD 1 TO PAGE-CTR.
       MOVE PAGE-CTR TO TITLE-PAGE.
       WRITE PRTLINE FROM TITLE-LINE1.
       WRITE PRTLINE FROM TITLE-LINE2
       AFTER ADVANCING 1 LINE.
        WRITE PRTLINE FROM COL-HEADING1
         AFTER ADVANCING 2 LINES.
       WRITE PRTLINE FROM COL-HEADING2
         AFTER ADVANCING 1 LINE.

      *ERROR REPORT HEADINGS		 
	      ADD 1 TO ERR-PAGE-CTR.
       MOVE ERR-PAGE-CTR TO ERR-TITLE-PAGE.
       WRITE ERRLINE FROM ERR-TITLE-LINE.
       WRITE ERRLINE FROM TITLE-LINE2
         AFTER ADVANCING 1 LINE.
	      WRITE ERRLINE FROM ERR-TITLE-LINE2
		 AFTER ADVANCING 1 LINE.
	
       L3-VALIDATION.
	      MOVE 'N' TO ERROR-SW.
	      IF L-NAME EQUAL SPACES
		MOVE 'LAST NAME REQUIRED' TO ERROR-MSG
		MOVE 'Y' TO ERROR-SW
		GO TO L3-VALIDATION-EXIT.
	      IF NOT VAL-STATES
		MOVE 'STATES MUST BE IA, IL, MI, MO, NE, OR WI' TO ERROR-MSG
		MOVE 'Y' TO ERROR-SW
		GO TO L3-VALIDATION-EXIT.
	      IF I-ZIP NOT NUMERIC
		MOVE 'ZIP CODE MUST BE NUMERIC' TO ERROR-MSG
		MOVE 'Y' TO ERROR-SW
		GO TO L3-VALIDATION-EXIT.
	      IF I-POP-TYPE NUMERIC
		IF NOT VAL-POP-TYPES
			NEXT SENTENCE
		ELSE
			MOVE 'POP TYPE MUST BE 1-6' TO ERROR-MSG
			MOVE 'Y' TO ERROR-SW
			GO TO L3-VALIDATION-EXIT.
		END-IF.
	      ELSE
		MOVE 'POP TYPE MUST BE NUMERIC' TO ERROR-MSG
			MOVE 'Y' TO ERROR-SW
			GO TO L3-VALIDATION-EXIT.
			
       L3-VALIDATION-EXIT.
	      EXIT.
	
       L3-ERROR-PRINT.
	      COMPUTE C-GT-ERR-CTR = C-GT-ERR-CTR + 1.
	      MOVE I-REC TO O-ERR-REC
	      WRITE ERRLINE FROM O-ERROR-LINE
		AFTER ADVANCING 2 LINES
			AT EOP
				PERFORM L4-ERROR-HEADING
       L3-CALCS.
	      EVALUATE I-STATE
		WHEN 'IA'
			MOVE 0.05 TO C-DEP
		WHEN 'IL'
			MOVE 0 TO C-DEP
		WHEN 'MI'
			MOVE 0.10 TO C-DEP
		WHEN 'MO'
			MOVE 0 TO C-DEP
		WHEN 'NE'
			MOVE 0.05 TO C-DEP
		WHEN 'WI'
			MOVE 0.05 TO C-DEP
	      END-EVALUATE.
	
	      COMPUTE C-TOT-DEP = C-DEP * (24 * I-CASES).
	      COMPUTE C-TOT-AMNT = (18.71 * I-CASES) + C-TOT-DEP.
	
	      EVALUATE I-POP-TYPE
		WHEN 1
			COMPUTE C-GT-COKE-CASES = GT-COKE-CASES + I-CASES
		WHEN 2
			COMPUTE C-GT-DC-CASES = GT-COKE-CASES + I-CASES
		WHEN 3
			COMPUTE C-GT-MY-CASES = GT-COKE-CASES + I-CASES
		WHEN 4
			COMPUTE C-GT-CC-CASES = GT-COKE-CASES + I-CASES
		WHEN 5
			COMPUTE C-GT-DCC-CASES = GT-COKE-CASES + I-CASES
		WHEN 6
			COMPUTE C-GT-SPRITE-CASES = GT-COKE-CASES + I-CASES
	      END-EVALUATE.
	
	      EVALUATE I-TEAM
		WHEN 'A'
			COMPUTE C-GT-A-TEAM = C-GT-A-TEAM + C-TOT-AMNT
		WHEN 'B'
			COMPUTE C-GT-B-TEAM = C-GT-B-TEAM + C-TOT-AMNT
		WHEN 'C'
			COMPUTE C-GT-C-TEAM = C-GT-C-TEAM + C-TOT-AMNT
		WHEN 'D'
			COMPUTE C-GT-D-TEAM = C-GT-D-TEAM + C-TOT-AMNT
		WHEN 'E'
			COMPUTE C-GT-E-TEAM = C-GT-E-TEAM + C-TOT-AMNT
		
	
       L3-MOVE-PRINT.
      *MOVE I AND C FIELDS TO D FIELDS.  

       WRITE PRTLINE FROM DETAIL-LINE
         AFTER ADVANCING 2 LINES
             AT EOP
                 PERFORM L4-HEADING.
				 
       L3-ERR-TOTALS.
	      MOVE C-GT-ERR-CTR TO GT-ERR-CTR.
	      WRITE ERRLINE FROM ERR-TOTAL-LINE
		AFTER ADVANCING 2 LINES.
      
       L3-TOTALS.
	      WRITE PRTLINE FROM TITLE-LINE1
		AFTER ADVANCING PAGE.
	      WRITE PRTLINE FROM TITLE-LINE2
        AFTER ADVANCING 1 LINE.
	      WRITE PRTLINE FROM TITLE-LINE3
		AFTER ADVANCING 1 LINE.
       WRITE PRTLINE FROM GT-POP-LINE
		AFTER ADVANCING 2 LINES.
	  *MOVE C-GT FIELDS TO GT FIELDS.
       WRITE PRTLINE FROM GT-POP-LINE2
        AFTER ADVANCING 2 LINES.
      *MOVE NEXT C-GT FIELDS.
	      WRITE PRTLINE FROM GT-POP-LINE2
        AFTER ADVANCING 2 LINES.
		
	      WRITE PRTLINE FROM GT-TEAM-LINE
		AFTER ADVANCING 3 LINES.
	  *MOVE NEXT C-GT FIELDS.
	      WRITE PRTLINE FROM GT-TEAM-LINE2
		AFTER ADVANCING 2 LINES.
	  *MOVE NEXT C-GT FIELDS.
	      WRITE PRTLINE FROM GT-TEAM-LINE2
		AFTER ADVANCING 2 LINES.
	  *MOVE NEXT C-GT FIELDS.
	      WRITE PRTLINE FROM GT-TEAM-LINE2
		AFTER ADVANCING 2 LINES.
	  *MOVE NEXT C-GT FIELDS.
	      WRITE PRTLINE FROM GT-TEAM-LINE2
		AFTER ADVANCING 2 LINES.
      *MOVE NEXT C-GT FIELDS.
	      WRITE PRTLINE FROM GT-TEAM-LINE2
		AFTER ADVANCING 2 LINES.


       L4-HEADING.
       ADD 1 TO PAGE-CTR.
       MOVE PAGE-CTR TO TITLE-PAGE.
       WRITE PRTLINE FROM TITLE-LINE1
         AFTER ADVANCING PAGE.
       WRITE PRTLINE FROM TITLE-LINE2
         AFTER ADVANCING 1 LINE.
	      WRITE PRTLINE FROM TITLE-LINE3
		 AFTER ADVANCING 1 LINE.
       WRITE PRTLINE FROM COL-HEADING1
         AFTER ADVANCING 2 LINES.
       WRITE PRTLINE FROM COL-HEADING2
         AFTER ADVANCING 1 LINE.
		 
       L4-ERROR-HEADING
	      ADD 1 TO ERR-PAGE-CTR.
       MOVE ERR-PAGE-CTR TO ERR-TITLE-PAGE.
       WRITE ERRLINE FROM ERR-TITLE-LINE
         AFTER ADVANCING PAGE.
       WRITE ERRLINE FROM TITLE-LINE2
         AFTER ADVANCING 1 LINE.
	       WRITE ERRLINE FROM ERR-TITLE-LINE2
		 AFTER ADVANCING 1 LINE.
 
       L9-READ-INPUT.
       READ POP-INPUT
         AT END
             MOVE 'N' TO MORE-RECS.

       END PROGRAM CBLHJB05.