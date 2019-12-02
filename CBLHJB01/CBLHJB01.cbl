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
			  10 I-PIZZA-ITEM-NO1      PIC X.
			  10 I-PIZZA-ITEM-NO2      PIC X.
			  10 I-PIZZA-ITEM-NO3      PIC XX.
		   05 I-PIZZA-CUR-DATE.
			  10 I-PIZZA-CUR-YY        PIC 9(4).
			  10 I-PIZZA-CUR-MM        PIC 99.
			  10 I-PIZZA-CUR-DD        PIC 99.
		   05 I-PIZZA-PRICE            PIC 99V99.
		   05 I-PIZZA-CUR-QTY          PIC 9(5).
		   05 I-PIZZA-PREV-QTY         PIC 9(5).

	   FD PRTOUT
		   LABEL RECORD IS OMITTED
		   DATA RECORD IS PRTLINE
		   RECORD CONTAINS 132 CHARACTERS
		   LINAGE IS 60 WITH FOOTING AT 56.

	   01 PRTLINE                      PIC X(132).

       WORKING-STORAGE SECTION.
	   01 MISC.

	   01 TITLE-LINE.
	       05 FILLER                   PIC X(6)    VALUE 'DATE: '.
		   05 TITLE-DATE.
		       10 TITLE-MONTH          PIC XX.
			   10 FILLER               PIC X       VALUE '/'.
			   10 TITLE-DAY            PIC XX.
		       10 FILLER               PIC X       VALUE '/'.
			   10 TITLE-YEAR           PIC X(4).
		   05 FILLER                   PIC X(39)   VALUE SPACES.
		   05 FILLER                   PIC X(23)
		      VALUE 'BIRKNER S MONTHLY SALES'.
		   05 FILLER                   PIC X(46)   VALUE SPACES.
		   05 FILLER                   PIC X(6)    VALUE 'PAGE: '.
		   05 TITLE-PAGE               PIC Z9.

	   01 COL-HEADING1.
		   05 FILLER                   PIC X(5)    VALUE SPACES.
		   05 FILLER                   PIC X(4)    VALUE 'ITEM'.
		   05 FILLER                   PIC X(23)   VALUE SPACES.
		   05 FILLER                   PIC X(5)    VALUE 'PRIOR'.
		   05 FILLER                   PIC X(7)    VALUE SPACES.
		   05 FILLER                   PIC X(7)    VALUE 'CURRENT'.
		   05 FILLER                   PIC X(9)    VALUE SPACES.
		   05 FILLER                   PIC X(14)
              VALUE 'SALES INCREASE'.
		   05 FILLER                   PIC X(8)    VALUE SPACES.
		   05 FILLER                   PIC X(9)    VALUE 'INCR/DECR'.
		   05 FILLER                   PIC X(10)   VALUE SPACES.
		   05 FILLER                   PIC X(4)    VALUE 'SALE'.
		   05 FILLER                   PIC X(27)   VALUE SPACES.

	   01 COL-HEADING2.
	       05 FILLER                   PIC X(4)    VALUE SPACES.
		   05 FILLER                   PIC X(6)    VALUE 'NUMBER'.
		   05 FILLER                   PIC X(4)    VALUE SPACES.
		   05 FILLER                   PIC X(10)   VALUE 'SALES DATE'.
		   05 FILLER                   PIC X(9)    VALUE SPACES.
		   05 FILLER                   PIC X(3)    VALUE 'QTY'.
		   05 FILLER                   PIC X(11)   VALUE SPACES.
		   05 FILLER                   PIC X(13)
              VALUE '/DECREASE AMT'.
		   05 FILLER                   PIC X(9)    VALUE SPACES.
		   05 FILLER                   PIC X(10)   VALUE 'PERCENTAGE'.
		   05 FILLER                   PIC X(8)    VALUE SPACES.
		   05 FILLER                   PIC X(5)    VALUE 'PRICE'.
		   05 FILLER                   PIC X(10)   VALUE SPACES.
		   05 FILLER                   PIC X(11)   VALUE 'TOTAL SALES'.
		   05 FILLER                   PIC X(6)    VALUE SPACES.

	   01 DETAIL-LINE.

	   01 TOTAL-LINE.

       PROCEDURE DIVISION.

           STOP RUN.
           
       END PROGRAM CBLHJB01.