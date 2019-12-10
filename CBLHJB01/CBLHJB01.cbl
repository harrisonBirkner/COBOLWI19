       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBLHJB01.
	   AUTHOR. HARRISON BIRKNER.
	   

       ENVIRONMENT DIVISION.
		   SELECT PIZZA-INPUT
			   ASSIGN TO 'C:\COBOLWI19\CBLPIZZA.DAT'
				   ORGANIZATION IS LINE SEQUENTIAL.

		   SELECT PRTOUT
		       ASSIGN TO 'C:\COBOLWI19\PIZZARPT.PRT'
				   ORGANIZATION IS RECORD SEQUENTIAL.

       DATA DIVISION.
	   FILE SECTION.
	   FD PIZZA-INPUT
		   LABEL RECORD IS STANDARD
		   DATA RECORD IS I-PIZZA-REC
		   RECORD CONTAINS 26 CHARACTERS.

	   01 I-PIZZA-REC.
		   05 I-PIZZA-ITEM-NO.
			  10 I-PIZZA-ITEM-NO1    PIC X.
			  10 I-PIZZA-ITEM-NO2    PIC X.
			  10 I-PIZZA-ITEM-NO3    PIC XX.
		   05 I-PIZZA-CUR-DATE.
			  10 I-PIZZA-CUR-YY      PIC 9(4).
			  10 I-PIZZA-CUR-MM      PIC 99.
			  10 I-PIZZA-CUR-DD      PIC 99.
		   05 I-PIZZA-PRICE          PIC 99V99.
		   05 I-PIZZA-CUR-QTY        PIC 9(5).
		   05 I-PIZZA-PREV-QTY       PIC 9(5).

	   FD PRTOUT
		   LABEL RECORD IS OMITTED
		   DATA RECORD IS PRTLINE
		   RECORD CONTAINS 132 CHARACTERS
		   LINAGE IS 60 WITH FOOTING AT 56.

	   01 PRTLINE                    PIC X(132).

       WORKING-STORAGE SECTION.
	   01 MISC.
		   05  MORE-RECS               PIC X(3)    VALUE 'YES'.
		   05  PAGE-CTR                PIC 99      VALUE 0.
		   05  CURRENT-DATE-AND-TIME.
			   10 CURRENT-YEAR         PIC X(4).
			   10 CURRENT-MONTH        PIC XX.
			   10 CURRENT-DAY          PIC XX.
			   10 CURRENT-TIME         PIC X(11).
		   05 CALCS.
			   10 C-INC-DEC-AMT        PIC 9(7).
			   10 C-INC-DEC-PCT        PIC 9(7).
			   10 C-TOTAL-SALES        PIC 9(13).
			   10 C-NUM-SALES          PIC 9(4)    VALUE 0.
		   05 GRAND-TOTALS.
			   10 C-GT-TOTAL-SALES     PIC 9(15)   VALUE 0.
			   10 C-GT-INC-DEC-AMT     PIC 9(9)    VALUE 0.
			   10 C-GT-AVG-INC-DEC-AMT PIC 9(7)    VALUE 0.
			   10 C-GT-AVG-INC-DEC-PCT PIC 9(5)    VALUE 0.
			   10 C-GT-PREV-QTY        PIC 9(7)    VALUE 0.


	   01 TITLE-LINE.
	       05 FILLER                 PIC X(6)      VALUE 'DATE: '.
		   05 TITLE-DATE.
		       10 TITLE-MONTH        PIC XX.
			   10 FILLER             PIC X         VALUE '/'.
			   10 TITLE-DAY          PIC XX.
		       10 FILLER             PIC X         VALUE '/'.
			   10 TITLE-YEAR         PIC X(4).
		   05 FILLER                 PIC X(39)     VALUE SPACES.
		   05 FILLER                 PIC X(23)
		      VALUE 'BIRKNER S MONTHLY SALES'.
		   05 FILLER                 PIC X(46)     VALUE SPACES.
		   05 FILLER                 PIC X(6)      VALUE 'PAGE: '.
		   05 TITLE-PAGE             PIC Z9.

	   01 COL-HEADING1.
		   05 FILLER                 PIC X(5)      VALUE SPACES.
		   05 FILLER                 PIC X(4)      VALUE 'ITEM'.
		   05 FILLER                 PIC X(23)     VALUE SPACES.
		   05 FILLER                 PIC X(5)      VALUE 'PRIOR'.
		   05 FILLER                 PIC X(7)      VALUE SPACES.
		   05 FILLER                 PIC X(7)      VALUE 'CURRENT'.
		   05 FILLER                 PIC X(9)      VALUE SPACES.
		   05 FILLER                 PIC X(14)
              VALUE 'SALES INCREASE'.
		   05 FILLER                 PIC X(8)      VALUE SPACES.
		   05 FILLER                 PIC X(9)      VALUE 'INCR/DECR'.
		   05 FILLER                 PIC X(10)     VALUE SPACES.
		   05 FILLER                 PIC X(4)      VALUE 'SALE'.

	   01 COL-HEADING2.
	       05 FILLER                 PIC X(4)      VALUE SPACES.
		   05 FILLER                 PIC X(6)      VALUE 'NUMBER'.
		   05 FILLER                 PIC X(4)      VALUE SPACES.
		   05 FILLER                 PIC X(10)     VALUE 'SALES DATE'.
		   05 FILLER                 PIC X(9)      VALUE SPACES.
		   05 FILLER                 PIC X(3)      VALUE 'QTY'.
		   05 FILLER                 PIC X(10)     VALUE SPACES.
		   05 FILLER                 PIC X(3)      VALUE 'QTY'.
		   05 FILLER                 PIC X(11)     VALUE SPACES.
		   05 FILLER                 PIC X(13)
              VALUE '/DECREASE AMT'.
		   05 FILLER                 PIC X(9)      VALUE SPACES.
		   05 FILLER                 PIC X(10)     VALUE 'PERCENTAGE'.
		   05 FILLER                 PIC X(8)      VALUE SPACES.
		   05 FILLER                 PIC X(5)      VALUE 'PRICE'.
		   05 FILLER                 PIC X(10)     VALUE SPACES.
		   05 FILLER                 PIC X(11)     VALUE 'TOTAL SALES'.

	   01 DETAIL-LINE.
		   05 FILLER                 PIC X(4)      VALUE SPACES.
		   05 D-PIZZA-ITEM-NO1       PIC X.
		   05 FILLER                 PIC X         VALUE '-'.
		   05 D-PIZZA-ITEM-NO2       PIC X.
		   05 FILLER                 PIC X         VALUE '-'.
		   05 D-PIZZA-ITEM-NO3       PIC XX.
		   05 FILLER                 PIC X(4)      VALUE SPACES.
		   05 D-PIZZA-CUR-MM         PIC 99.
		   05 FILLER                 PIC X         VALUE '/'.
		   05 D-PIZZA-CUR-DD         PIC 99.
		   05 FILLER                 PIC X         VALUE '/'.
		   05 D-PIZZA-CUR-YY         PIC 9(4).
		   05 FILLER                 PIC X(7)      VALUE SPACES.
		   05 D-PIZZA-PREV-QTY       PIC ZZ,ZZ9.
		   05 FILLER                 PIC X(8)      VALUE SPACES.
		   05 D-PIZZA-CUR-QTY        PIC ZZ,ZZ9.
		   05 FILLER                 PIC X(12)     VALUE SPACES.
		   05 D-INC-DEC-AMT          PIC ZZ,ZZ9B-.
		   05 FILLER                 PIC X(13)     VALUE SPACES.
		   05 D-INC-DEC-PCT          PIC ++++9.
		   05 FILLER                 PIC X         VALUE '%'.
		   05 FILLER                 PIC X(9)      VALUE SPACES.
		   05 D-SALE-PRICE           PIC $$$.99.
		   05 FILLER                 PIC X(7)      VALUE SPACES.
		   05 D-TOTAL-SALES          PIC $$$,$$$,$$$.99.

	   01 TOTAL-LINE1.
		   05 FILLER                 PIC X(45)     VALUE SPACES.
		   05 FILLER                 PIC X(15)
              VALUE 'GRAND TOTALS:'.
		   05 FILLER                 PIC XX        VALUE SPACES.
		   05 GT-INC-DEC-AMT         PIC Z,ZZZ,ZZ9B-.
		   05 FILLER                 PIC X(38)     VALUE SPACES.
		   05 GT-TOTAL-SALES         PIC $$,$$$,$$$,$$$.99.

	   01 TOTAL-LINE2.
		   05 FILLER                 PIC X(25)       VALUE SPACES.
		   05 FILLER                 PIC X(38)
		      VALUE 'AVERAGE INCREASE/DECREASE AMOUNT:'.
		   05 FILLER                 PIC X(5)        VALUE SPACES.
		   05 GT-AVG-INC-DEC-AMT     PIC ZZ,ZZ9B-.    

	   01 TOTAL-LINE3.
		   05 FILLER                 PIC X(21)       VALUE SPACES.
		   05 FILLER                 PIC X(37)
		      VALUE 'AVERAGE INCREASE/DECREASE PERCENTAGE:'.
		   05 FILLER                 PIC X(7)        VALUE SPACES.
		   05 GT-AVG-INC-DEC-PCT     PIC +++9B.
		   05 FILLER                 PIC X           VALUE '%'.

       PROCEDURE DIVISION.
	   L1-MAIN.
		   PERFORM L2-INIT.
		   PERFORM L2-MAINLINE
			   UNTIL MORE-RECS = "NO".
		   PERFORM L2-CLOSING.
           STOP RUN.

	   L2-INIT.
		   OPEN INPUT PIZZA-INPUT.
		   OPEN OUTPUT PRTOUT.
		   MOVE FUNCTION CURRENT-DATE  TO CURRENT-DATE-AND-TIME.
		   MOVE CURRENT-MONTH          TO TITLE-MONTH.
		   MOVE CURRENT-DAY            TO TITLE-DAY.
		   MOVE CURRENT-YEAR           TO TITLE-YEAR.
		   PERFORM L3-INIT-HEADING.
		   PERFORM L3-READ-INPUT.

	   L2-MAINLINE.
		   PERFORM L3-CALCS.
		   PERFORM L3-MOVE-PRINT.
		   PERFORM L3-READ-INPUT.

	   L2-CLOSING.
		   PERFORM L3-TOTALS.
		   CLOSE PIZZA-INPUT.
		   CLOSE PRTOUT.

	   L3-INIT-HEADING.
		   ADD 1 TO PAGE-CTR.
		   MOVE PAGE-CTR               TO TITLE-PAGE.
		   WRITE PRTLINE FROM TITLE-LINE.
		   WRITE PRTLINE FROM COL-HEADING1
			   AFTER ADVANCING 2 LINES.
		   WRITE PRTLINE FROM COL-HEADING2
			   AFTER ADVANCING 1 LINE.

	   L3-READ-INPUT.
		   READ PIZZA-INPUT
			   AT END
				   MOVE 'NO' TO MORE-RECS.

	   L3-CALCS.
		   COMPUTE C-INC-DEC-AMT = I-PIZZA-CUR-QTY - I-PIZZA-PREV-QTY.
		   COMPUTE C-INC-DEC-PCT =
               C-INC-DEC-AMT / I-PIZZA-PREV-QTY * 100.
		   COMPUTE C-TOTAL-SALES = I-PIZZA-PRICE * I-PIZZA-CUR-QTY.
           COMPUTE C-NUM-SALES = C-NUM-SALES + 1.

	   L3-MOVE-PRINT.
		   MOVE I-PIZZA-ITEM-NO1       TO D-PIZZA-ITEM-NO1.
		   MOVE I-PIZZA-ITEM-NO2       TO D-PIZZA-ITEM-NO2.
		   MOVE I-PIZZA-ITEM-NO3       TO D-PIZZA-ITEM-NO3.
		   MOVE I-PIZZA-CUR-MM         TO D-PIZZA-CUR-MM.
		   MOVE I-PIZZA-CUR-DD         TO D-PIZZA-CUR-DD.
		   MOVE I-PIZZA-CUR-YY         TO D-PIZZA-CUR-YY.
		   MOVE I-PIZZA-PREV-QTY       TO D-PIZZA-PREV-QTY.
		   MOVE I-PIZZA-CUR-QTY        TO D-PIZZA-CUR-QTY.
		   MOVE C-INC-DEC-AMT          TO D-INC-DEC-AMT.
		   MOVE C-INC-DEC-PCT          TO D-INC-DEC-PCT.
		   MOVE I-PIZZA-PRICE          TO D-SALE-PRICE.
		   MOVE C-TOTAL-SALES          TO D-TOTAL-SALES.
		   WRITE PRTLINE FROM DETAIL-LINE
		       AFTER ADVANCING 2 LINES
		           AT EOP
		               PERFORM L4-HEADING.

	   L3-TOTALS.
		   MOVE C-GT-TOTAL-SALES       TO GT-TOTAL-SALES.
		   MOVE C-GT-AVG-INC-DEC-AMT   TO GT-AVG-INC-DEC-AMT.
		   MOVE C-GT-AVG-INC-DEC-PCT   TO GT-AVG-INC-DEC-PCT.
		   MOVE C-GT-INC-DEC-AMT       TO GT-INC-DEC-AMT.
		   WRITE PRTLINE FROM TOTAL-LINE1
			   AFTER ADVANCING 4 LINES.
		   WRITE PRTLINE FROM TOTAL-LINE2
			   AFTER ADVANCING 2 LINES.
		   WRITE PRTLINE FROM TOTAL-LINE3
			   AFTER ADVANCING 2 LINES.

	   L4-HEADING.
		   ADD 1 TO PAGE-CTR.
		   MOVE PAGE-CTR               TO TITLE-PAGE.
		   WRITE PRTLINE FROM TITLE-LINE
			   AFTER ADVANCING PAGE.
		   WRITE PRTLINE FROM COL-HEADING1
			   AFTER ADVANCING 2 LINES.
		   WRITE PRTLINE FROM COL-HEADING2
			   AFTER ADVANCING 1 LINE.
           
       END PROGRAM CBLHJB01.