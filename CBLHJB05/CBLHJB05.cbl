       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBLHJB05.
	   AUTHOR. HARRISON BIRKNER.
      *THIS PROGRAM CALCULATES THE AMOUNT OF POP SALES SOLD FOR A
      *LOCAL FUNDRAISER. ALL RECORDS RECORDS ARE VALIDATED PRIOR TO 
      *PROCESSING. ERRONEOUS RECORDS ARE DUMPED TO AN ERROR REPORT
      *AND PROCESSING IS BYPASSED. VALID RECORDS ARE PROCESSED AND
      *WRITTEN TO THE SALES REPORT.                                       

       ENVIRONMENT DIVISION.
		   SELECT POP-INPUT
			   ASSIGN TO 'C:\COBOLWI19\CBLPOPSL.DAT'
				   ORGANIZATION IS LINE SEQUENTIAL.

		   SELECT PRTOUT
			   ASSIGN TO 'C:\COBOLWI19\CBLPOPSL.PRT'
				   ORGANIZATION IS RECORD SEQUENTIAL.
           
           SELECT ERROUT
			   ASSIGN TO 'C:\COBOLWI19\CBLPOPER.PRT'
				   ORGANIZATION IS RECORD SEQUENTIAL.

       DATA DIVISION.
	   FILE SECTION.
	   FD POP-INPUT
	   LABEL RECORD IS STANDARD
	   DATA RECORD IS I-REC
	   RECORD CONTAINS 71 CHARACTERS.

	   01 I-REC.
           05 I-LNAME                 PIC X(15).
		   05 I-FNAME                 PIC X(15).
		   05 I-ADDRESS               PIC X(15).
		   05 I-CITY                  PIC X(10).
		   05 I-STATE                 PIC XX.
			   88 VAL-STATES      VALUE 'IA','IL','MI','MO','NE','WI'.
		   05 I-ZIP.
               10 I-ZIP1              PIC 9(5).
			   10 I-ZIP2              PIC 9(4).
		   05 I-POP-TYPE              PIC 99.
			   88 VAL-POP-TYPES   VALUE 01,02,03,04,05,06.
		   05 I-CASES                 PIC 99.
		   05 I-TEAM                  PIC X.
			   88 VAL-TEAMS       VALUE 'A','B','C','D','E'.

	   FD PRTOUT
	   LABEL RECORD IS OMITTED
	   DATA RECORD IS PRTLINE
	   RECORD CONTAINS 132 CHARACTERS
	   LINAGE IS 60 WITH FOOTING AT 56.

	   01 PRTLINE                     PIC X(132).

	   FD ERROUT
	   LABEL RECORD IS OMITTED
	   DATA RECORD IS ERRLINE
	   RECORD CONTAINS 132 CHARACTERS
	   LINAGE IS 60 WITH FOOTING AT 56.

	   01 ERRLINE                     PIC X(132).
	 
       WORKING-STORAGE SECTION.
	   01 MISC.
	       05 MORE-RECS               PIC X       VALUE 'Y'.
		   05 ERR-SW                  PIC X       VALUE 'N'.
		   05 PAGE-CTR                PIC 99      VALUE 0.
		   05 ERR-PAGE-CTR            PIC 99      VALUE 0.
		   05 CURRENT-DATE-AND-TIME.			  
		       10 CURRENT-YEAR        PIC X(4).	  
			   10 CURRENT-MONTH       PIC XX.	  
			   10 CURRENT-DAY         PIC XX.	  
			   10 CURRENT-TIME        PIC X(11).
		   05 CALCS.
			   10 C-DEP               PIC 9V99.
			   10 C-TOT-DEP           PIC 9(5)V99.
			   10 C-TOT-SALES         PIC 9(6)V99.
		   05 GRAND-TOTALS.
			   10 C-GT-ERR-CTR        PIC 9(6)       VALUE 0.
			   10 POP-ACCUMS.
				   15 C-GT-COKE-CASES    PIC 9(8)    VALUE 0.
				   15 C-GT-DC-CASES      PIC 9(8)    VALUE 0.
				   15 C-GT-MY-CASES      PIC 9(8)    VALUE 0.
				   15 C-GT-CC-CASES      PIC 9(8)    VALUE 0.
				   15 C-GT-DCC-CASES     PIC 9(8)    VALUE 0.
				   15 C-GT-SPRITE-CASES  PIC 9(8)    VALUE 0.
			   10 TEAM-ACCUMS.
				   15 C-GT-A-TEAM        PIC 9(11)V99  VALUE 0.
				   15 C-GT-B-TEAM        PIC 9(11)V99  VALUE 0.
				   15 C-GT-C-TEAM        PIC 9(11)V99  VALUE 0.
				   15 C-GT-D-TEAM        PIC 9(11)V99  VALUE 0.
				   15 C-GT-E-TEAM        PIC 9(11)V99  VALUE 0.
												  
	   01 TITLE-LINE1.							  
           05 FILLER                  PIC X(6)    VALUE 'DATE: '.
		   05 TITLE-DATE.						  
		       10 TITLE-MONTH         PIC XX.	  
			   10 FILLER              PIC X       VALUE '/'.
			   10 TITLE-DAY           PIC XX.	  
		       10 FILLER              PIC X       VALUE '/'.
			   10 TITLE-YEAR          PIC X(4).	  
		   05 FILLER                  PIC X(36)   VALUE SPACES.
		   05 FILLER                  PIC X(28)	  
              VALUE 'ALBIA SOCCER CLUB FUNDRAISER'. 
		   05 FILLER                  PIC X(44)   VALUE SPACES.
		   05 FILLER                  PIC X(6)    VALUE 'PAGE: '.
		   05 TITLE-PAGE              PIC Z9.	  
												  
	   01 TITLE-LINE2.							  
		   05 FILLER                  PIC X(8)    VALUE 'COBHJB05'.
		   05 FILLER                  PIC X(49)   VALUE SPACES.
		   05 FILLER                  PIC X(17)	  
              VALUE 'HARRISON DIVISION'.		  
												  
	   01 TITLE-LINE3.							  
		   05 FILLER                  PIC X(60)   VALUE SPACES.
		   05 FILLER                  PIC X(12)   VALUE 'SALES REPORT'.

	   01 ERR-TITLE-LINE.
		   05 FILLER                  PIC X(6)    VALUE 'DATE: '.
		   05 ERR-TITLE-DATE.					   
		       10 ERR-TITLE-MONTH     PIC XX.	  
			   10 FILLER              PIC X       VALUE '/'.
			   10 ERR-TITLE-DAY       PIC XX.	  
		       10 FILLER              PIC X       VALUE '/'.
			   10 ERR-TITLE-YEAR      PIC X(4).	  
		   05 FILLER                  PIC X(36)   VALUE SPACES.
		   05 FILLER                  PIC X(28)	  
              VALUE 'ALBIA SOCCER CLUB FUNDRAISER'. 
		   05 FILLER                  PIC X(44)   VALUE SPACES.
		   05 FILLER                  PIC X(6)    VALUE 'PAGE: '.
		   05 ERR-TITLE-PAGE          PIC Z9.

	   01 ERR-TITLE-LINE2.
		   05 FILLER                  PIC X(60)   VALUE SPACES.
		   05 FILLER                  PIC X(12)   VALUE 'ERROR REPORT'.

	   01 COL-HEADING.
		   05 FILLER                  PIC XXX     VALUE SPACES.
		   05 FILLER                  PIC X(9)    VALUE 'LAST NAME'.
		   05 FILLER                  PIC X(8)    VALUE SPACES.
		   05 FILLER                  PIC X(10)   VALUE 'FIRST NAME'.
		   05 FILLER                  PIC X(7)    VALUE SPACES.
		   05 FILLER                  PIC X(4)    VALUE 'CITY'.
		   05 FILLER                  PIC X(8)    VALUE SPACES.
		   05 FILLER                  PIC X(5)    VALUE 'STATE'.
		   05 FILLER                  PIC X       VALUE SPACES.
		   05 FILLER                  PIC X(8)    VALUE 'ZIP CODE'.
		   05 FILLER                  PIC X(4)    VALUE SPACES.
		   05 FILLER                  PIC X(8)    VALUE 'POP TYPE'.
		   05 FILLER                  PIC X(13)   VALUE SPACES.
		   05 FILLER                  PIC X(8)    VALUE 'QUANTITY'.
		   05 FILLER                  PIC X(6)    VALUE SPACES.
		   05 FILLER                  PIC X(11)   VALUE 'DEPOSIT AMT'.
		   05 FILLER                  PIC X(6)    VALUE SPACES.
		   05 FILLER                  PIC X(11)   VALUE 'TOTAL SALES'.

	   01 ERR-COL-HEADING.
		   05 FILLER                  PIC X(12)   VALUE 'ERROR RECORD'.
		   05 FILLER                  PIC X(60)   VALUE SPACES.
		   05 FILLER                  PIC X(17)
              VALUE 'ERROR DESCRIPTION'.

	   01 DETAIL-LINE.
		   05 FILLER                  PIC XXX     VALUE SPACES.
		   05 D-LNAME                 PIC X(15).
		   05 FILLER                  PIC XX      VALUE SPACES.
		   05 D-FNAME                 PIC X(15).
		   05 FILLER                  PIC XX      VALUE SPACES.
		   05 D-CITY                  PIC X(10).
		   05 FILLER                  PIC XXX     VALUE SPACES.
		   05 D-STATE                 PIC XX.
		   05 FILLER                  PIC XXX     VALUE SPACES.
		   05 D-ZIP1                  PIC X(5).
		   05 FILLER                  PIC X       VALUE '-'.
		   05 D-ZIP2                  PIC X(4).
		   05 FILLER                  PIC XX.
		   05 D-POP-TYPE              PIC X(16).
		   05 FILLER                  PIC X(8)    VALUE SPACES.
		   05 D-QTY                   PIC Z9.
		   05 FILLER                  PIC X(11)   VALUE SPACES.
		   05 D-DEPOSIT-AMT           PIC $$$$.99.
		   05 FILLER                  PIC X(9)    VALUE SPACES.
		   05 D-TOT-SALES             PIC $$,$$$.99.

	   01 ERROR-LINE.
		   05 ERR-REC                 PIC X(71).
		   05 FILLER                  PIC X       VALUE SPACES.
		   05 ERR-MSG                 PIC X(60).

       01 GT-POP-LINE.
		   05 FILLER                  PIC X(13)   VALUE 'GRAND TOTALS:'.

	   01 GT-POP-LINE2.
		   05 FILLER                  PIC XXX.
		   05 GT-POP-NAME1            PIC X(16).
		   05 FILLER                  PIC X       VALUE SPACES.
		   05 GT-POP-TOTAL1           PIC ZZZ,ZZ9.
		   05 FILLER                  PIC X(6)    VALUE SPACES.
		   05 GT-POP-NAME2            PIC X(16).
		   05 FILLER                  PIC X       VALUE SPACES.
		   05 GT-POP-TOTAL2           PIC ZZZ,ZZ9.
		   05 FILLER                  PIC X(6)    VALUE SPACES.
		   05 GT-POP-NAME3            PIC X(16).
		   05 FILLER                  PIC X       VALUE SPACES.
		   05 GT-POP-TOTAL3           PIC ZZZ,ZZ9.

	   01 GT-TEAM-LINE.
		   05 FILLER                  PIC X(12)   VALUE 'TEAM TOTALS:'.

       01 GT-TEAM-LINE2.
		   05 FILLER                  PIC XXX     VALUE SPACES.
		   05 GT-TEAM-NAME            PIC X.
		   05 GT-TEAM-TOTAL           PIC $$$$,$$$,$$$.99.

	   01 ERR-TOTAL-LINE.
		   05 FILLLER                 PIC X(13)   VALUE 'TOTAL ERRORS '.
		   05 GT-ERR-CTR              PIC Z,ZZ9.
	   
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
	       MOVE CURRENT-MONTH                TO ERR-TITLE-MONTH.
           MOVE CURRENT-DAY                  TO ERR-TITLE-DAY.
           MOVE CURRENT-YEAR                 TO ERR-TITLE-YEAR.
     
           PERFORM L3-INIT-HEADING.
           PERFORM L9-READ-INPUT.

       L2-MAINLINE.
	       PERFORM L3-VALIDATION
		       THRU L3-VALIDATION-EXIT.
	       IF ERR-SW = 'N'
		       PERFORM L3-CALCS
		       PERFORM L3-MOVE-PRINT
	       ELSE
		       PERFORM L3-ERROR-PRINT
		   END-IF.
		   PERFORM L9-READ-INPUT.
 
       L2-CLOSING.
           PERFORM L3-TOTALS.
	       PERFORM L3-ERR-TOTALS.
           CLOSE POP-INPUT.
           CLOSE PRTOUT.
	       CLOSE ERROUT.

       L3-INIT-HEADING.
      *THIS PARAGRAPH FUNCTIONS SIMILARLY TO L4-HEADING, EXCEPT IT IS
      *ONLY RUN ONCE AT THE BEGINNING OF THE PROGRAM, DOES NOT PRINT
      *AN EXTRA 60 BLANK LINES, AND PRINTS INIT HEADINGS FOR ERROR
      *REPORT.
           ADD 1 TO PAGE-CTR.
           MOVE PAGE-CTR TO TITLE-PAGE.
           WRITE PRTLINE FROM TITLE-LINE1.
           WRITE PRTLINE FROM TITLE-LINE2
               AFTER ADVANCING 1 LINE.
           WRITE PRTLINE FROM COL-HEADING
               AFTER ADVANCING 2 LINES.

      *ERROR REPORT HEADINGS		 
	       ADD 1 TO ERR-PAGE-CTR.
           MOVE ERR-PAGE-CTR TO ERR-TITLE-PAGE.
           WRITE ERRLINE FROM ERR-TITLE-LINE.
           WRITE ERRLINE FROM TITLE-LINE2
               AFTER ADVANCING 1 LINE.
	       WRITE ERRLINE FROM ERR-TITLE-LINE2
		       AFTER ADVANCING 1 LINE.
	       WRITE ERRLINE FROM ERR-COL-HEADING
		       AFTER ADVANCING 2 LINES.
	
       L3-VALIDATION.
      *VALIDATION PARAGRAPH. EACH FIELD IN A RECORD IS CHECKED AGAINST 
      *REQUIREMENTS. IF INVALID AN APPROPRIATE ERROR MESSAGE IS MOVED TO
      *THE ERROR OUTPUT.
	       MOVE 'N' TO ERR-SW.
	           IF I-LNAME EQUAL SPACES
		           MOVE 'LAST NAME REQUIRED' TO ERR-MSG
		           MOVE 'Y' TO ERR-SW
		           GO TO L3-VALIDATION-EXIT.

			   IF I-FNAME EQUAL SPACES
		           MOVE 'FIRST NAME REQUIRED' TO ERR-MSG
		           MOVE 'Y' TO ERR-SW
		           GO TO L3-VALIDATION-EXIT.

			   IF I-ADDRESS EQUAL SPACES
		           MOVE 'ADDRESS REQUIRED' TO ERR-MSG
		           MOVE 'Y' TO ERR-SW
		           GO TO L3-VALIDATION-EXIT.

			   IF I-CITY EQUAL SPACES
		           MOVE 'CITY REQUIRED' TO ERR-MSG
		           MOVE 'Y' TO ERR-SW
		           GO TO L3-VALIDATION-EXIT.

	           IF NOT VAL-STATES
		           MOVE 'STATES MUST BE IA, IL, MI, MO, NE, OR WI'
                   TO ERR-MSG
		           MOVE 'Y' TO ERR-SW
		           GO TO L3-VALIDATION-EXIT.

	           IF I-ZIP NOT NUMERIC
		           MOVE 'ZIP CODE MUST BE NUMERIC' TO ERR-MSG
		           MOVE 'Y' TO ERR-SW
	               GO TO L3-VALIDATION-EXIT.

	           IF I-POP-TYPE NUMERIC
		           IF VAL-POP-TYPES
			           NEXT SENTENCE
		           ELSE
			           MOVE 'POP TYPE MUST BE 1-6' TO ERR-MSG
			           MOVE 'Y' TO ERR-SW
			           GO TO L3-VALIDATION-EXIT
		           END-IF
	           ELSE
		           MOVE 'POP TYPE MUST BE NUMERIC' TO ERR-MSG
			       MOVE 'Y' TO ERR-SW
			       GO TO L3-VALIDATION-EXIT
			   END-IF.

               IF I-CASES NUMERIC
		           IF I-CASES >= 1
			           NEXT SENTENCE
		           ELSE
			           MOVE 'MINIMUM OF 1 CASE' TO ERR-MSG
			           MOVE 'Y' TO ERR-SW
			           GO TO L3-VALIDATION-EXIT
		           END-IF
	           ELSE
		           MOVE 'NUMBER OF CASES MUST BE NUMERIC' TO ERR-MSG
			       MOVE 'Y' TO ERR-SW
			       GO TO L3-VALIDATION-EXIT
		       END-IF.

			   IF NOT VAL-TEAMS
				   MOVE 'TEAM MUST BE A, B, C, D, OR E' TO ERR-MSG
				   MOVE 'Y' TO ERR-SW
				   GO TO L3-VALIDATION-EXIT
               END-IF.
			
       L3-VALIDATION-EXIT.
	       EXIT.
	
       L3-ERROR-PRINT.
	       COMPUTE C-GT-ERR-CTR = C-GT-ERR-CTR + 1.
	       MOVE I-REC TO ERR-REC
	       WRITE ERRLINE FROM ERROR-LINE
		       AFTER ADVANCING 2 LINES
			       AT EOP
				       PERFORM L4-ERROR-HEADING.
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
	       COMPUTE C-TOT-SALES = (18.71 * I-CASES) + C-TOT-DEP.
	
	       EVALUATE I-POP-TYPE
		       WHEN 1
			       COMPUTE C-GT-COKE-CASES = C-GT-COKE-CASES + I-CASES
			       MOVE 'COKE' TO D-POP-TYPE
		       WHEN 2
			       COMPUTE C-GT-DC-CASES = C-GT-DC-CASES + I-CASES
			       MOVE  'DIET COKE' TO D-POP-TYPE
		       WHEN 3
			       COMPUTE C-GT-MY-CASES = C-GT-MY-CASES + I-CASES
			       MOVE 'MELLO YELLO' TO D-POP-TYPE
		       WHEN 4
			       COMPUTE C-GT-CC-CASES = C-GT-CC-CASES + I-CASES
			       MOVE 'CHERRY COKE' TO D-POP-TYPE
		       WHEN 5
			       COMPUTE C-GT-DCC-CASES = C-GT-DCC-CASES + I-CASES
			       MOVE 'DIET CHERRY COKE' TO D-POP-TYPE
		       WHEN 6
			       COMPUTE C-GT-SPRITE-CASES = C-GT-SPRITE-CASES
                                               + I-CASES
			       MOVE 'SPRITE' TO D-POP-TYPE
	       END-EVALUATE.
	
	       EVALUATE I-TEAM
		       WHEN 'A'
			       COMPUTE C-GT-A-TEAM = C-GT-A-TEAM + C-TOT-SALES
		       WHEN 'B'
			       COMPUTE C-GT-B-TEAM = C-GT-B-TEAM + C-TOT-SALES
		       WHEN 'C'
			       COMPUTE C-GT-C-TEAM = C-GT-C-TEAM + C-TOT-SALES
		       WHEN 'D'
			       COMPUTE C-GT-D-TEAM = C-GT-D-TEAM + C-TOT-SALES
		       WHEN 'E'
			       COMPUTE C-GT-E-TEAM = C-GT-E-TEAM + C-TOT-SALES
	       END-EVALUATE.
		
	
       L3-MOVE-PRINT.
           MOVE I-LNAME        TO D-LNAME.
		   MOVE I-FNAME        TO D-FNAME.
		   MOVE I-CITY         TO D-CITY.
		   MOVE I-STATE        TO D-STATE.
		   MOVE I-ZIP1         TO D-ZIP1.
		   MOVE I-ZIP2         TO D-ZIP2.
		   MOVE I-CASES        TO D-QTY.
		   MOVE C-TOT-DEP      TO D-DEPOSIT-AMT.
		   MOVE C-TOT-SALES    TO D-TOT-SALES.

           WRITE PRTLINE FROM DETAIL-LINE
               AFTER ADVANCING 2 LINES
                   AT EOP
                       PERFORM L4-HEADING.
				 
       L3-ERR-TOTALS.
	       MOVE C-GT-ERR-CTR TO GT-ERR-CTR.
	       WRITE ERRLINE FROM ERR-TOTAL-LINE
		       AFTER ADVANCING 3 LINES.
      
       L3-TOTALS.
		   ADD 1 TO PAGE-CTR.
		   MOVE PAGE-CTR TO TITLE-PAGE.
	       WRITE PRTLINE FROM TITLE-LINE1
		       AFTER ADVANCING PAGE.
	       WRITE PRTLINE FROM TITLE-LINE2
               AFTER ADVANCING 1 LINE.
	       WRITE PRTLINE FROM TITLE-LINE3
		       AFTER ADVANCING 1 LINE.
           WRITE PRTLINE FROM GT-POP-LINE
		       AFTER ADVANCING 2 LINES.
		   MOVE 'COKE' TO GT-POP-NAME1.
           MOVE C-GT-COKE-CASES TO GT-POP-TOTAL1.
		   MOVE 'DIET COKE' TO GT-POP-NAME2.
           MOVE C-GT-DC-CASES TO GT-POP-TOTAL2.
		   MOVE 'MELLO YELLO' TO GT-POP-NAME3
           MOVE C-GT-MY-CASES TO GT-POP-TOTAL3.
           WRITE PRTLINE FROM GT-POP-LINE2
               AFTER ADVANCING 2 LINES.
           MOVE 'CHERRY COKE' TO GT-POP-NAME1.
           MOVE C-GT-CC-CASES TO GT-POP-TOTAL1.
		   MOVE 'DIET CHERRY COKE' TO GT-POP-NAME2.
           MOVE C-GT-DCC-CASES TO GT-POP-TOTAL2.
		   MOVE 'SPRITE' TO GT-POP-NAME3
           MOVE C-GT-SPRITE-CASES TO GT-POP-TOTAL3.
	       WRITE PRTLINE FROM GT-POP-LINE2
               AFTER ADVANCING 2 LINES.
		
	       WRITE PRTLINE FROM GT-TEAM-LINE
		       AFTER ADVANCING 3 LINES.
		   MOVE 'A' TO GT-TEAM-NAME.
		   MOVE C-GT-A-TEAM TO GT-TEAM-TOTAL.
	       WRITE PRTLINE FROM GT-TEAM-LINE2
		       AFTER ADVANCING 2 LINES.
		   MOVE 'B' TO GT-TEAM-NAME.
		   MOVE C-GT-B-TEAM TO GT-TEAM-TOTAL.
	       WRITE PRTLINE FROM GT-TEAM-LINE2
		       AFTER ADVANCING 2 LINES.
		   MOVE 'C' TO GT-TEAM-NAME.
		   MOVE C-GT-C-TEAM TO GT-TEAM-TOTAL.
	       WRITE PRTLINE FROM GT-TEAM-LINE2
		       AFTER ADVANCING 2 LINES.
		   MOVE 'D' TO GT-TEAM-NAME.
		   MOVE C-GT-D-TEAM TO GT-TEAM-TOTAL.
	       WRITE PRTLINE FROM GT-TEAM-LINE2
		       AFTER ADVANCING 2 LINES.
		   MOVE 'E' TO GT-TEAM-NAME.
		   MOVE C-GT-E-TEAM TO GT-TEAM-TOTAL.
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
           WRITE PRTLINE FROM COL-HEADING
               AFTER ADVANCING 2 LINES.
		 
       L4-ERROR-HEADING.
	       ADD 1 TO ERR-PAGE-CTR.
           MOVE ERR-PAGE-CTR TO ERR-TITLE-PAGE.
           WRITE ERRLINE FROM ERR-TITLE-LINE
               AFTER ADVANCING PAGE.
           WRITE ERRLINE FROM TITLE-LINE2
               AFTER ADVANCING 1 LINE.
	       WRITE ERRLINE FROM ERR-TITLE-LINE2
		       AFTER ADVANCING 1 LINE.
	       WRITE ERRLINE FROM ERR-COL-HEADING
		       AFTER ADVANCING 2 LINES.
 
       L9-READ-INPUT.
           READ POP-INPUT
               AT END
                   MOVE 'N' TO MORE-RECS.

       END PROGRAM CBLHJB05.