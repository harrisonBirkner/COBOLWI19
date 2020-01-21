       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBLHJB04.
	   AUTHOR. HARRISON BIRKNER.

       ENVIRONMENT DIVISION.
	       SELECT BILL-INPUT
			   ASSIGN TO 'C:\COBOLWI19\MONBILLS.DAT'
				   ORGANIZATION IS LINE SEQUENTIAL.

		   SELECT PRTOUT
		       ASSIGN TO 'C:\COBOLWI19\RENT.PRT'
				   ORGANIZATION IS RECORD SEQUENTIAL.

       DATA DIVISION.
	   FILE SECTION.
	   FD BILL-INPUT
	   LABEL RECORD IS STANDARD
	   DATA RECORD IS I-REC
	   RECORD CONTAINS 24 CHARACTERS.

	   01 I-REC.
	       05 I-BLD-CODE           PIC XX.
		   05 I-UNIT			   PIC 99.
               88 VAL-UNIT-PREM               VALUE 23, 25.
		   05 I-TENANTS		       PIC 9.
			   88 VAL-BASE-TENANTS            VALUE 1.
			   88 VAL-2-3-TENANTS             VALUE 2 THRU 3.
			   88 VAL-4-TENANTS               VALUE 4 THRU 9.
		   05 I-ELECTRIC	       PIC 999V99.
		   05 I-GAS			       PIC 999V99.
		   05 I-WATER		       PIC 999V99.
		   05 I-GARBAGE		       PIC 99V99.

	   FD PRTOUT
	   LABEL RECORD IS OMITTED
	   DATA RECORD IS PRTLINE
	   RECORD CONTAINS 132 CHARACTERS
	   LINAGE IS 60 WITH FOOTING AT 56.

	   01 PRTLINE                  PIC X(132).

       WORKING-STORAGE SECTION.
	   01 MISC.
	       05 MORE-RECS            PIC XXX    VALUE 'YES'.
		   05 PAGE-CTR             PIC 99     VALUE 0.
		   05 CURRENT-DATE-AND-TIME.
			   10 CURRENT-YEAR     PIC X(4).
			   10 CURRENT-MONTH    PIC XX.
			   10 CURRENT-DAY      PIC XX.
			   10 CURRENT-TIME     PIC X(11).
		   05 CALCS.
			   10 C-BASE-RENT      PIC 9(5)V99.
			   10 C-TENANT-CHARGE  PIC 9(5)V99.
			   10 C-PREM-DISC      PIC S9(6)V99.
			   10 C-TOTAL-UTIL-COST PIC 9(6)V99.
			   10 C-SUBTOTAL       PIC 9(6)V99.
			   10 C-RENT-DUE       PIC 9(6)V99.
		   05 TOTALS.
			   10 C-GT-PREM-CTR    PIC 9(5)   VALUE 0.
			   10 C-GT-DISC-CTR    PIC 9(5)   VALUE 0.

	   01 TITLE-LINE1.
		   05 FILLER               PIC X(6)   VALUE 'DATE: '.
		   05 TITLE-DATE.
		       10 TITLE-MONTH      PIC XX.
			   10 FILLER           PIC X      VALUE '/'.
			   10 TITLE-DAY        PIC XX.
		       10 FILLER           PIC X      VALUE '/'.
			   10 TITLE-YEAR       PIC X(4).
		   05 FILLER               PIC X(42)  VALUE SPACES.
		   05 FILLER               PIC X(15)
		      VALUE 'FURLY S RENTALS'.
		   05 FILLER               PIC X(51)  VALUE SPACES.
		   05 FILLER               PIC X(6)   VALUE 'PAGE: '.
		   05 TITLE-PAGE           PIC Z9.

	   01 TITLE-LINE2.
		   05 FILLER               PIC X(8)   VALUE 'COBHJB04'.
		   05 FILLER               PIC X(45)  VALUE SPACES.
		   05 FILLER               PIC X(16)
		      VALUE 'BILLABLE RENT - '.
		   05 TITLE-BILL-MONTH     PIC X(9).

	   01 COL-HEADING1.
		   05 FILLER               PIC X(23)  VALUE SPACES.
		   05 FILLER               PIC X(4)   VALUE 'BASE'.
		   05 FILLER               PIC XX     VALUE SPACES.
		   05 FILLER               PIC X(6)   VALUE 'TENANT'.
		   05 FILLER               PIC XX     VALUE SPACES.
		   05 FILLER               PIC X(6)   VALUE 'TENANT'.
		   05 FILLER               PIC X(5)   VALUE SPACES.
		   05 FILLER               PIC X(8)   VALUE 'PREMIUM/'.
		   05 FILLER               PIC X(53)  VALUE SPACES.
		   05 FILLER               PIC X(5)   VALUE 'TOTAL'.

       01 COL-HEADING2.
		   05 FILLER               PIC X(15)  VALUE 'RENTAL BUILDING'.
		   05 FILLER               PIC X      VALUE SPACES.
		   05 FILLER               PIC X(4)   VALUE 'UNIT'.
		   05 FILLER               PIC XXX    VALUE SPACES.
		   05 FILLER               PIC X(4)   VALUE 'RENT'.
		   05 FILLER               PIC XX     VALUE SPACES.
		   05 FILLER               PIC X(6)   VALUE 'NUMBER'.
		   05 FILLER               PIC X(2)   VALUE SPACES.
		   05 FILLER               PIC X(6)   VALUE 'CHARGE'.
		   05 FILLER               PIC X(5)   VALUE SPACES.
		   05 FILLER               PIC X(8)   VALUE 'DISCOUNT'.
		   05 FILLER               PIC X(5)   VALUE SPACES.
		   05 FILLER               PIC X(8)   VALUE 'SUBTOTAL'.
		   05 FILLER               PIC XX     VALUE SPACES.
		   05 FILLER               PIC X(8)   VALUE 'ELECTRIC'.
		   05 FILLER               PIC X(4)   VALUE SPACES.
		   05 FILLER               PIC XXXX   VALUE 'GAS'.
		   05 FILLER               PIC X(3)   VALUE SPACES.
		   05 FILLER               PIC X(5)   VALUE 'WATER'.
		   05 FILLER               PIC XX     VALUE SPACES.
		   05 FILLER               PIC X(7)   VALUE 'GARBAGE'.
		   05 FILLER               PIC XXX    VALUE SPACES.
		   05 FILLER               PIC X(9)   VALUE 'UTILITIES'.
		   05 FILLER               PIC X(5)   VALUE SPACES.
		   05 FILLER               PIC X(8)   VALUE 'RENT DUE'.

	   01 DETAIL-LINE.
		   05 D-BLD                PIC X(15).
		   05 FILLER               PIC XX     VALUE SPACES.
		   05 D-UNIT               PIC Z9.
		   05 FILLER               PIC XX     VALUE SPACES.
		   05 D-BASE-RENT          PIC $$$$.99.
		   05 FILLER               PIC XXX    VALUE SPACES.
		   05 D-TENANTS            PIC 9.
		   05 FILLER               PIC X(4)   VALUE SPACES.
		   05 D-TENANT-CHARGE      PIC $$$$.99.
		   05 FILLER               PIC X(4)   VALUE SPACES.
		   05 D-PREM-DISC          PIC $$,$$$.99+.
		   05 FILLER               PIC XXX    VALUE SPACES.
		   05 D-SUBTOTAL           PIC $$,$$$.99.
		   05 FILLER               PIC XX     VALUE SPACES.
		   05 D-ELECTRIC           PIC $$$$.99.
		   05 FILLER               PIC XX     VALUE SPACES.
		   05 D-GAS                PIC $$$$.99.
		   05 FILLER               PIC XX     VALUE SPACES.
		   05 D-WATER              PIC $$$$.99.
		   05 FILLER               PIC XX     VALUE SPACES.
		   05 D-GARBAGE            PIC $$$.99.
		   05 FILLER               PIC XXX    VALUE SPACES.
		   05 D-TOTAL-UTIL-COST    PIC $$,$$$.99.
		   05 FILLER               PIC X(4)   VALUE SPACES.
		   05 D-RENT-DUE           PIC $$,$$$.99.
		   05 D-RENT-LIMIT-FLAG    PIC XXX    VALUE SPACES.

	   01 TOTAL-LINE.

	   01 TOTAL-LINE2.

	   01 TOTAL-LINE3.

       PROCEDURE DIVISION.
	   L1-MAIN.
           PERFORM L2-INIT.
           PERFORM L2-MAINLINE
               UNTIL MORE-RECS = "NO".
           PERFORM L2-CLOSING.
       STOP RUN.
       
       L2-INIT.
           OPEN INPUT BILL-INPUT.
           OPEN OUTPUT PRTOUT.
           MOVE FUNCTION CURRENT-DATE     TO CURRENT-DATE-AND-TIME.
           MOVE CURRENT-MONTH             TO TITLE-MONTH.
           MOVE CURRENT-DAY               TO TITLE-DAY.
           MOVE CURRENT-YEAR              TO TITLE-YEAR.
	       EVALUATE CURRENT-MONTH
		       WHEN 01
			       MOVE 'JANUARY'         TO TITLE-BILL-MONTH
		       WHEN 02
			       MOVE 'FEBRUARY'        TO TITLE-BILL-MONTH
		       WHEN 03
			       MOVE 'MARCH'           TO TITLE-BILL-MONTH
		       WHEN 04
			       MOVE 'APRIL'           TO TITLE-BILL-MONTH
		       WHEN 05
			       MOVE 'MAY'             TO TITLE-BILL-MONTH
		       WHEN 06
			       MOVE 'JUNE'            TO TITLE-BILL-MONTH
		       WHEN 07
			       MOVE 'JULY'            TO TITLE-BILL-MONTH
		       WHEN 08
			       MOVE 'AUGUST'          TO TITLE-BILL-MONTH
		       WHEN 09
			       MOVE 'SEPTEMBER'       TO TITLE-BILL-MONTH
		       WHEN 10
			       MOVE 'OCTOBER'         TO TITLE-BILL-MONTH
		       WHEN 11
			       MOVE 'NOVEMBER'        TO TITLE-BILL-MONTH
		       WHEN 12
			       MOVE 'DECEMBER'        TO TITLE-BILL-MONTH
		   END-EVALUATE.
	       PERFORM L3-INIT-HEADING.
           PERFORM L9-READ-INPUT.
      
       L2-MAINLINE.
           PERFORM L3-CALCS.
           PERFORM L3-MOVE-PRINT.
           PERFORM L9-READ-INPUT.
       
       L2-CLOSING.
           PERFORM L3-TOTALS.
           CLOSE BILL-INPUT.
           CLOSE PRTOUT.

       L3-INIT-HEADING.
           ADD 1 TO PAGE-CTR.
           MOVE PAGE-CTR                  TO TITLE-PAGE.
           WRITE PRTLINE FROM TITLE-LINE1.
	       WRITE PRTLINE FROM TITLE-LINE2
		       AFTER ADVANCING 1 LINE.
           WRITE PRTLINE FROM COL-HEADING1
	           AFTER ADVANCING 2 LINES.
           WRITE PRTLINE FROM COL-HEADING2
	           AFTER ADVANCING 1 LINE.
			
       L3-CALCS.
           EVALUATE I-UNIT
               WHEN 1 THRU 8
                   COMPUTE C-BASE-RENT ROUNDED = 650
                   IF VAL-2-3-TENANTS
                       COMPUTE C-TENANT-CHARGE ROUNDED = 25 * I-TENANTS
                   ELSE
                       IF VAL-4-TENANTS
                           MOVE 83.45        TO C-TENANT-CHARGE
					   ELSE
						   MOVE 0            TO C-TENANT-CHARGE
                       END-IF
                   END-IF
               WHEN 9 THRU 16
                   COMPUTE C-BASE-RENT ROUNDED = 700
                   IF VAL-2-3-TENANTS
                       COMPUTE C-TENANT-CHARGE ROUNDED =
                           35.55 * I-TENANTS
                   ELSE
                       IF VAL-4-TENANTS
                           MOVE 135          TO C-TENANT-CHARGE
					   ELSE
						   MOVE 0            TO C-TENANT-CHARGE
                       END-IF
                   END-IF
               WHEN 17 THRU 25
                   COMPUTE C-BASE-RENT ROUNDED = 825
                   IF VAL-2-3-TENANTS
                       COMPUTE C-TENANT-CHARGE ROUNDED = 50 * I-TENANTS
                   ELSE
                       IF VAL-4-TENANTS
                          MOVE 185.60        TO C-TENANT-CHARGE
					   ELSE
						   MOVE 0            TO C-TENANT-CHARGE
                       END-IF
                   END-IF
           END-EVALUATE.
       
           EVALUATE I-BLD-CODE
               WHEN 'AA'
                   MOVE 'PALACE PLACE' TO D-BLD
               WHEN 'GG'
                   MOVE 'GEORIGA' TO D-BLD
               WHEN 'PP'
                   MOVE 'PARK PLACE' TO D-BLD
                   IF VAL-UNIT-PREM
                       COMPUTE C-PREM-DISC ROUNDED = C-BASE-RENT * 0.12
                       ADD 1 TO C-GT-PREM-CTR
               WHEN 'IA'
                   MOVE 'IOWA CONDO' TO D-BLD
                   IF CURRENT-MONTH = 7 OR CURRENT-MONTH = 12
                       COMPUTE C-PREM-DISC ROUNDED = C-BASE-RENT * -0.5
                       ADD 1 TO C-GT-DISC-CTR
                   END-IF
               WHEN 'MS'
                   MOVE 'MARKET STREET' TO D-BLD
               WHEN 'HH'
                   MOVE 'HIGH TOWER' TO D-BLD
               WHEN 'R7'
                   MOVE 'UPTOWN CONDOS' TO D-BLD
                   IF VAL-UNIT-PREM
                       COMPUTE C-PREM-DISC ROUNDED = C-BASE-RENT * 0.12
                       ADD 1 TO C-GT-PREM-CTR
               WHEN 'GM'
                   MOVE 'GANDER MOUNTAIN' TO D-BLD
               WHEN 'BP'
                   MOVE 'BENTON PLACE' TO D-BLD
                   COMPUTE C-PREM-DISC ROUNDED = C-BASE-RENT * -0.33
                   ADD 1 TO C-GT-DISC-CTR
               WHEN 'GA'
                   MOVE 'GRAND AVENUE' TO D-BLD
               WHEN 'JK'
                   MOVE 'JACKS PLACE' TO D-BLD
                   IF CURRENT-MONTH = 7 OR CURRENT-MONTH = 12
                       COMPUTE C-PREM-DISC ROUNDED = C-BASE-RENT * -0.5
                       ADD 1 TO C-GT-DISC-CTR
                   END-IF
               WHEN 'UN'
                   MOVE 'UNDERGROUND SAM' TO D-BLD
               WHEN 'YD'
                   MOVE 'YANKEE DOODLE' TO D-BLD
               WHEN 'YT'
                   MOVE 'YAHTZEE AVE' TO D-BLD
                   IF VAL-UNIT-PREM
                       COMPUTE C-PREM-DISC ROUNDED = C-BASE-RENT * 0.12
                       ADD 1 TO C-GT-PREM-CTR
                WHEN 'CP'
                   MOVE 'COURT PLACE' TO D-BLD
               WHEN 'NZ'
                   MOVE 'NEW ZOO' TO D-BLD
               WHEN 'VV'
                   MOVE 'VERMONT' TO D-BLD
               WHEN 'CT'
                   MOVE 'CHINA TOWN' TO D-BLD
                   COMPUTE C-PREM-DISC ROUNDED = C-BASE-RENT * -0.33
                   ADD 1 TO C-GT-DISC-CTR
               WHEN 'YS'
                   MOVE 'YORKSHIRE' TO D-BLD
               WHEN 'ME'
                   MOVE 'MAINE APT' TO D-BLD
           END-EVALUATE.
       
           COMPUTE C-SUBTOTAL ROUNDED =
               C-TENANT-CHARGE + C-BASE-RENT + C-PREM-DISC.             
           COMPUTE C-TOTAL-UTIL-COST ROUNDED =
               I-ELECTRIC + I-GAS + I-WATER + I-GARBAGE.
           COMPUTE C-RENT-DUE ROUNDED = C-TOTAL-UTIL-COST + C-SUBTOTAL.

		   IF C-RENT-DUE > 1000
			   MOVE '***'                    TO D-RENT-LIMIT-FLAG
		   ELSE
			   MOVE SPACES                   TO D-RENT-LIMIT-FLAG
           END-IF.
       
       L3-MOVE-PRINT.
		   MOVE I-UNIT                       TO D-UNIT.
		   MOVE C-BASE-RENT                  TO D-BASE-RENT.
		   MOVE I-TENANTS                    TO D-TENANTS.
		   MOVE C-TENANT-CHARGE              TO D-TENANT-CHARGE.
		   MOVE C-PREM-DISC                  TO D-PREM-DISC.
		   MOVE C-SUBTOTAL                   TO D-SUBTOTAL.
		   MOVE I-ELECTRIC                   TO D-ELECTRIC.
		   MOVE I-GAS                        TO D-GAS.
		   MOVE I-WATER                      TO D-WATER.
		   MOVE I-GARBAGE                    TO D-GARBAGE.
		   MOVE C-TOTAL-UTIL-COST            TO D-TOTAL-UTIL-COST.
		   MOVE C-RENT-DUE                   TO D-RENT-DUE.
           WRITE PRTLINE FROM DETAIL-LINE
               AFTER ADVANCING 2 LINES
                   AT EOP
                       PERFORM L4-HEADING.
            
       L3-TOTALS.
		   MOVE C-GT-DISC-CTR                TO GT-DISC-CTR.
		   MOVE C-GT-PREM-CTR                TO GT-PREM-CTR.
               WRITE PRTLINE FROM TOTAL-LINE
                   AFTER ADVANCING 3 LINES.
           WRITE PRTLINE FROM TOTAL-LINE2
                   AFTER ADVANCING 2 LINES.
           WRITE PRTLINE FROM TOTAL-LINE3
                   AFTER ADVANCING 1 LINE.

       L4-HEADING.
           ADD 1 TO PAGE-CTR.
           MOVE PAGE-CTR                     TO TITLE-PAGE.
           WRITE PRTLINE FROM TITLE-LINE1
               AFTER ADVANCING PAGE.
           WRITE PRTLINE FROM TITLE-LINE2
               AFTER ADVANCING 1 LINE.
           WRITE PRTLINE FROM COL-HEADING1
               AFTER ADVANCING 2 LINES.
           WRITE PRTLINE FROM COL-HEADING2
               AFTER ADVANCING 1 LINE.
       
       L9-READ-INPUT.
           READ BILL-INPUT
               AT END
                   MOVE 'NO' TO MORE-RECS.

       END PROGRAM CBLHJB04.