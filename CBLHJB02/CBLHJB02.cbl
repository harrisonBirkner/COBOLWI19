       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBLHJB02.
	   AUTHOR. HARRISON BIRKNER.

       ENVIRONMENT DIVISION.
		   SELECT BOAT-INPUT
			   ASSIGN TO 'C:\COBOLWI19\CBLBOAT1.DAT'
				   ORGANIZATION IS LINE SEQUENTIAL.

		   SELECT PRTOUT
		       ASSIGN TO 'C:\COBOLWI19\BOATRPT1.PRT'
				   ORGANIZATION IS RECORD SEQUENTIAL.
       DATA DIVISION.
	   FILE SECTION.
	   FD BOAT-INPUT
	   LABEL RECORD IS STANDARD
	   DATA RECORD IS I-REC
	   RECORD CONTAINS 42 CHARACTERS.

	   01 I-REC.
           05 I-LAST-NAME                 PIC X(15).
		   05 I-STATE                     PIC XX.
		   05 I-BOAT-COST                 PIC 9(6)V99.
		   05 I-PURCHASE-DATE.
               10 I-PURCHASE-YY           PIC 9(4).
			   10 I-PURCHASE-MM           PIC 99.
			   10 I-PURCHASE-DD           PIC 99.
		   05 I-BOAT-TYPE                 PIC X.
		   05 I-ACC-PACK         PIC 9.
		   05 I-PREP-COST                 PIC 9(5)V99.

	   FD PRTOUT
	   LABEL RECORD IS OMITTED
	   DATA RECORD IS PRTLINE
	   RECORD CONTAINS 132 CHARACTERS
	   LINAGE IS 60 WITH FOOTING AT 56.

	   01 PRTLINE                         PIC X(132).

       WORKING-STORAGE SECTION.
	   01 MISC.
		   05 H-BOAT-TYPE                 PIC X.
		   05 MORE-RECS                   PIC X(3)    VALUE 'YES'.
		   05 PAGE-CTR                    PIC 99      VALUE 0.
		   05 CURRENT-DATE-AND-TIME.
			   10 CURRENT-YEAR            PIC X(4).
			   10 CURRENT-MONTH           PIC XX.
			   10 CURRENT-DAY             PIC XX.
			   10 CURRENT-TIME            PIC X(11).
		   05 CALCS.
			   10 C-TOTAL-COST            PIC 9(10)V99.

		   05 MJ-SUBTOTALS.
			   10 C-MJ-NUM-SALES          PIC 9(6)      VALUE 0.
			   10 C-MJ-TOTAL-SALES        PIC 9(11)V99  VALUE 0.
		   05 GRAND-TOTALS.
			   10 C-GT-NUM-SALES          PIC 9(7)      VALUE 0.
			   10 C-GT-TOTAL-SALES        PIC 9(13)V99  VALUE 0.

	   01 TITLE-LINE.
	       05 FILLER                      PIC X(6)      VALUE 'DATE: '.
		   05 TITLE-DATE.
		       10 TITLE-MONTH             PIC XX.
			   10 FILLER                  PIC X         VALUE '/'.
			   10 TITLE-DAY               PIC XX.
		       10 FILLER                  PIC X         VALUE '/'.
			   10 TITLE-YEAR              PIC X(4).
		   05 FILLER                      PIC X(39)     VALUE SPACES.
		   05 FILLER                      PIC X(23)
		      VALUE 'BIRKNER S MONTHLY SALES'.
		   05 FILLER                      PIC X(46)     VALUE SPACES.
		   05 FILLER                      PIC X(6)      VALUE 'PAGE: '.
		   05 TITLE-PAGE                  PIC Z9.

	   01 COL-HEADING1.
		   05 FILLER                      PIC X(8)
		      VALUE 'CUSTOMER'.
		   05 FILLER                      PIC X(36)     VALUE SPACES.
		   05 FILLER                      PIC X(4)      VALUE 'BOAT'.
		   05 FILLER                      PIC X(9)      VALUE SPACES.
		   05 FILLER                      PIC X(8)
              VALUE 'PURCHASE'.
		   05 FILLER                      PIC X(11)     VALUE SPACES.
		   05 FILLER                      PIC X(9)
              VALUE 'ACCESSORY'.
		   05 FILLER                      PIC X(21)     VALUE SPACES.
		   05 FILLER                      PIC X(4)      VALUE 'PREP'.
		   05 FILLER                      PIC X(17)     VALUE SPACES.
		   05 FILLER                      PIC X(5)      VALUE 'TOTAL'.

	   01 COL-HEADING2.
		   05 FILLER                      PIC X(9)
              VALUE 'LAST NAME'.
		   05 FILLER                      PIC X(14)     VALUE SPACES.
		   05 FILLER                      PIC X(5)      VALUE 'STATE'.
		   05 FILLER                      PIC X(16)     VALUE SPACES.
		   05 FILLER                      PIC X(4)      VALUE 'COST'.
		   05 FILLER                      PIC X(9)      VALUE SPACES.
		   05 FILLER                      PIC X(4)      VALUE 'DATE'.
		   05 FILLER                      PIC X(15)     VALUE SPACES.
		   05 FILLER                      PIC X(7)
              VALUE 'PACKAGE'.
		   05 FILLER                      PIC X(23)     VALUE SPACES.
		   05 FILLER                      PIC X(4)      VALUE 'COST'.
		   05 FILLER                      PIC X(18)     VALUE SPACES.
		   05 FILLER                      PIC X(4)      VALUE 'COST'.

	   01 DETAIL-LINE.
		   05 D-LAST-NAME                 PIC X(15).
		   05 FILLER                      PIC X(9)      VALUE SPACES.
		   05 D-STATE                     PIC XX.
		   05 FILLER                      PIC X(12)     VALUE SPACES.
		   05 D-BOAT-COST                 PIC ZZZ,ZZZ.99.
		   05 FILLER                      PIC X(9)      VALUE SPACES.
		   05 D-PURCHASE-MM               PIC XX.
		   05 FILLER                      PIC X         VALUE '/'.
		   05 D-PURCHASE-DD               PIC XX.
		   05 FILLER                      PIC X         VALUE '/'.
		   05 D-PURCHASE-YY               PIC 99.
		   05 FILLER                      PIC X(11)     VALUE SPACES.
		   05 D-ACC-PACK                  PIC X(15).
		   05 FILLER                      PIC X(10)     VALUE SPACES.
		   05 D-PREP-COST                 PIC ZZ,ZZZ.99.
		   05 FILLER                      PIC X(10)     VALUE SPACES.
		   05 D-TOTAL-COST                PIC Z,ZZZ,ZZZ.99.

	   01 BOAT-LINE.
	       05 FILLER                      PIC X(11)
              VALUE 'BOAT TYPE: '.
	       05 COL-BOAT-TYPE               PIC X(13).

       01 MJ-SUBTOTAL-LINE.
           05 FILLER                      PIC X(23)     VALUE SPACES.
           05 FILLER                      PIC X(14)
              VALUE 'SUBTOTALS FOR '.
           05 MJ-BOAT-TYPE                PIC X(13).
           05 FILLER                      PIC X(10)     VALUE SPACES.
           05 FILLER                      PIC X(14)
              VALUE 'NUMBER SOLD:  '.
           05 MJ-NUM-SALES                PIC Z,ZZ9.
           05 FILLER                      PIC X(38)     VALUE SPACES.
           05 MJ-TOTAL-SALES              PIC $,$$$,$$$,$$$.99.

	   01 TOTAL-LINE.
		   05 FILLER                      PIC X(23)     VALUE SPACES.
		   05 FILLER                      PIC X(12)
              VALUE 'GRAND TOTALS'.
		   05 FILLER                      PIC X(25)     VALUE SPACES.
		   05 FILLER                      PIC X(13)
		      VALUE 'NUMBER SOLD: '.
		   05 GT-NUM-SALES                PIC ZZ,ZZ9.
		   05 FILLER                      PIC X(35)     VALUE SPACES.
		   05 GT-TOTAL-SALES              PIC $$$,$$$,$$$,$$$.99.

       PROCEDURE DIVISION.
	   L1-MAIN.
           PERFORM L2-INIT.
	       PERFORM L2-MAINLINE
	           UNTIL MORE-RECS = "NO".
	       PERFORM L2-CLOSING.
	       STOP RUN.
	
       L2-INIT.
           OPEN INPUT BOAT-INPUT.
           OPEN OUTPUT PRTOUT.
           MOVE FUNCTION CURRENT-DATE   TO CURRENT-DATE-AND-TIME.
           MOVE CURRENT-MONTH           TO TITLE-MONTH.
           MOVE CURRENT-DAY             TO TITLE-DAY.
           MOVE CURRENT-YEAR            TO TITLE-YEAR.
           PERFORM L3-READ-INPUT.
	       MOVE I-BOAT-TYPE TO H-BOAT-TYPE.
		   PERFORM L5-EVAL-BOAT-TYPE.
	       PERFORM L3-INIT-HEADING.

       L2-MAINLINE.
	       IF I-BOAT-TYPE NOT = H-BOAT-TYPE
		       PERFORM L3-MJ-SUBTOTALS
			   PERFORM L5-EVAL-BOAT-TYPE
		       WRITE PRTLINE FROM BOAT-LINE
			       AFTER ADVANCING 2 LINES
		       WRITE PRTLINE FROM SPACES.
           PERFORM L3-CALCS.
           PERFORM L3-MOVE-PRINT.
           PERFORM L3-READ-INPUT.

       L2-CLOSING.
	       PERFORM L3-MJ-SUBTOTALS.
           PERFORM L3-TOTALS.
           CLOSE BOAT-INPUT.
           CLOSE PRTOUT.

       L3-INIT-HEADING.
           ADD 1 TO PAGE-CTR.
           MOVE PAGE-CTR                  TO TITLE-PAGE.
           WRITE PRTLINE FROM TITLE-LINE.
           WRITE PRTLINE FROM COL-HEADING1
	           AFTER ADVANCING 2 LINES.
           WRITE PRTLINE FROM COL-HEADING2
	           AFTER ADVANCING 1 LINE.
	       WRITE PRTLINE FROM BOAT-LINE
		       AFTER ADVANCING 2 LINES.
	       WRITE PRTLINE FROM SPACES.

       L3-READ-INPUT.
           READ BOAT-INPUT
	           AT END
		           MOVE 'NO'              TO MORE-RECS.
			
       L3-CALCS.
	       COMPUTE C-TOTAL-COST = I-BOAT-COST + I-PREP-COST.
	
	       COMPUTE C-MJ-NUM-SALES = C-MJ-NUM-SALES + 1.
	       COMPUTE C-MJ-TOTAL-SALES = C-MJ-TOTAL-SALES + C-TOTAL-COST.

       L3-MOVE-PRINT.
		   EVALUATE I-ACC-PACK
	           WHEN '1'
		           MOVE 'ELECTRONICS'     TO D-ACC-PACK
		       WHEN '2'
		           MOVE 'SKI PACKAGE'     TO D-ACC-PACK
		       WHEN '3'
			       MOVE 'FISHING PACKAGE' TO D-ACC-PACK
	       END-EVALUATE.
	       MOVE I-LAST-NAME               TO D-LAST-NAME.
		   MOVE I-STATE                   TO D-STATE.
		   MOVE I-BOAT-COST               TO D-BOAT-COST.
		   MOVE I-PURCHASE-DD             TO D-PURCHASE-DD.
		   MOVE I-PURCHASE-MM             TO D-PURCHASE-MM.
		   MOVE I-PURCHASE-YY             TO D-PURCHASE-YY.
		   MOVE I-PREP-COST               TO D-PREP-COST.
		   MOVE C-TOTAL-COST              TO D-TOTAL-COST.
	       WRITE PRTLINE FROM DETAIL-LINE
	           AFTER ADVANCING 1 LINE
		           AT EOP
		               PERFORM L4-HEADING.
			
       L3-MJ-SUBTOTALS.
           MOVE C-MJ-NUM-SALES            TO MJ-NUM-SALES.
		   MOVE C-MJ-TOTAL-SALES          TO MJ-TOTAL-SALES.
	       WRITE PRTLINE FROM MJ-SUBTOTAL-LINE
		       AFTER ADVANCING 2 LINES
			       AT EOP
				       PERFORM L4-HEADING.
           ADD C-MJ-NUM-SALES             TO C-GT-NUM-SALES.
		   ADD C-MJ-TOTAL-SALES           TO C-GT-TOTAL-SALES.

           MOVE 0                         TO C-MJ-NUM-SALES.
		   MOVE 0                         TO C-MJ-TOTAL-SALES.

	       MOVE I-BOAT-TYPE               TO H-BOAT-TYPE.

       L3-TOTALS.
           MOVE C-GT-NUM-SALES            TO GT-NUM-SALES.
		   MOVE C-GT-TOTAL-SALES          TO GT-TOTAL-SALES.
	       WRITE PRTLINE FROM TOTAL-LINE
		       AFTER ADVANCING 3 LINES.

       L4-HEADING.
	       ADD 1 TO PAGE-CTR.
	           MOVE PAGE-CTR              TO TITLE-PAGE.
	       WRITE PRTLINE FROM TITLE-LINE
		       AFTER ADVANCING PAGE.
	       WRITE PRTLINE FROM COL-HEADING1
		       AFTER ADVANCING 2 LINES.
	       WRITE PRTLINE FROM COL-HEADING2
		       AFTER ADVANCING 1 LINE.
	       WRITE PRTLINE FROM BOAT-LINE
		       AFTER ADVANCING 2 LINES.
	       WRITE PRTLINE FROM SPACES.

	   L5-EVAL-BOAT-TYPE.
	       EVALUATE I-BOAT-TYPE
	           WHEN 'B'
		           MOVE 'BASS BOAT'       TO COL-BOAT-TYPE
			       MOVE 'BASS BOAT'       TO MJ-BOAT-TYPE
		       WHEN 'P'
		           MOVE 'PONTOON'         TO COL-BOAT-TYPE
			       MOVE 'PONTOON'         TO MJ-BOAT-TYPE
		       WHEN 'S'
			       MOVE 'SKI BOAT'        TO COL-BOAT-TYPE
			       MOVE 'SKI BOAT'        TO MJ-BOAT-TYPE
		       WHEN 'J'
			       MOVE 'JOHN BOAT'       TO COL-BOAT-TYPE
			       MOVE 'JOHN BOAT'       TO MJ-BOAT-TYPE
		       WHEN 'C'
			       MOVE 'CANOE'           TO COL-BOAT-TYPE
			       MOVE 'CANOE'           TO MJ-BOAT-TYPE
		       WHEN 'R'
			       MOVE 'CABIN CRUISER'   TO COL-BOAT-TYPE
			       MOVE 'CABIN CRUISER'   TO MJ-BOAT-TYPE
	       END-EVALUATE.
           
       END PROGRAM CBLHJB02.