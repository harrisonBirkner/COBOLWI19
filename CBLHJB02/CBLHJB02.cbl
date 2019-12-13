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
		   05 I-ACCESSORY-PACKAGE         PIC 9.
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
		   05 MORE-RECS                  PIC X(3)    VALUE 'YES'.
		   05 PAGE-CTR                   PIC 99      VALUE 0.
		   05 CURRENT-DATE-AND-TIME.
			   10 CURRENT-YEAR            PIC X(4).
			   10 CURRENT-MONTH           PIC XX.
			   10 CURRENT-DAY             PIC XX.
			   10 CURRENT-TIME            PIC X(11).

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

	   01 BOAT-LINE.
	       05 FILLER                      PIC X(11)
              VALUE 'BOAT TYPE: '.
	       05 COL-BOAT-TYPE                 PIC X(13).

	   01 MJ-SUBTOTAL-LINE.
		   05 FILLER                      PIC X(23)     VALUE SPACES.
		   05 FILLER                      PIC X(14)
              VALUE 'SUBTOTALS FOR '.
		   05 MJ-BOAT-TYPE                PIC X(13).

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
		       PERFORM L3-MJ-SUBTOTALS.
		       WRITE PRTLINE FROM BOAT-LINE
			       AFTER ADVANCING 2 LINES.
		       WRITE PRTLINE FROM SPACES.
		       PERFORM L5-EVAL-BOAT-TYPE.
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
           MOVE PAGE-CTR                TO TITLE-PAGE.
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
		           MOVE 'NO' TO MORE-RECS.
			
       L3-CALCS.
	       COMPUTE C-TOTAL-COST = I-BOAT-COST + I-PREP-COST.
	
	       COMPUTE C-MJ-NUM-SALES = C-MJ-NUM-SALES + 1.
	       COMPUTE C-MJ-TOTAL-SALES = C-MJ-TOTAL-SALES + C-TOTAL-COST.

       L3-MOVE-PRINT.
	       MOVE I AND C FIELDS          TO D FIELDS.
	       WRITE PRTLINE FROM DETAIL-LINE
	           AFTER ADVANCING 1 LINE
		           AT EOP
		               PERFORM L4-HEADING.
			
       L3-MJ-SUBTOTALS.
	       MOVE C-MJ FIELDS             TO MJ FIELDS.
	       WRITE PRTLINE FROM MJ-SUBTOTAL-LINE
		       AFTER ADVANCING 2 LINES
			       AT EOP
				       PERFORM L4-HEADING.
	       ADD C-MJ FIELDS TO C-GT FIELDS.
	       C-MJ FIELDS = 0.
	       MOVE I-BOAT-TYPE             TO H-BOAT-TYPE.

       L3-TOTALS.
	       MOVE C-GT FIELDS             TO GT FIELDS.
	       WRITE PRTLINE FROM TOTAL-LINE
		       AFTER ADVANCING 3 LINES.

       L4-HEADING.
	       ADD 1 TO PAGE-CTR.
	           MOVE PAGE-CTR            TO TITLE-PAGE.
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
		           MOVE 'BASS BOAT'     TO COL-BOAT-TYPE
			       MOVE 'BASS BOAT'     TO MJ-BOAT-TYPE
		       WHEN 'P'
		           MOVE 'PONTOON'       TO COL-BOAT-TYPE
			       MOVE 'PONTOON'       TO MJ-BOAT-TYPE
		       WHEN 'S'
			       MOVE 'SKI BOAT'      TO COL-BOAT-TYPE
			       MOVE 'SKI BOAT'      TO MJ-BOAT-TYPE
		       WHEN 'J'
			       MOVE 'JOHN BOAT'     TO COL-BOAT-TYPE
			       MOVE 'JOHN BOAT'     TO MJ-BOAT-TYPE
		       WHEN 'C'
			       MOVE 'CANOE'         TO COL-BOAT-TYPE
			       MOVE 'CANOE'         TO MJ-BOAT-TYPE
		       WHEN 'R'
			       MOVE 'CABIN CRUISER' TO COL-BOAT-TYPE
			       MOVE 'CABIN CRUISER' TO MJ-BOAT-TYPE
	       END-EVALUATE.
           
       END PROGRAM CBLHJB02.