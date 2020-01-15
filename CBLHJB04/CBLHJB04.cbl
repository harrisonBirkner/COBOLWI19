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
		   05 I-TENANTS		       PIC 9.
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
	       05 MORE-RECS            PIC X(3)   VALUE 'YES'.
		   05 PAGE-CTR             PIC 99     VALUE 0.
		   05 CURRENT-DATE-AND-TIME.
			   10 CURRENT-YEAR     PIC X(4).
			   10 CURRENT-MONTH    PIC XX.
			   10 CURRENT-DAY      PIC XX.
			   10 CURRENT-TIME     PIC X(11).

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
		   05 FILLER               PIC X(5)   VALUE SPACES.
		   05 FILLER               PIC X(8)   VALUE 'PREMIUM/'.
		   05 FILLER               PIC X(53)  VALUE SPACES.
		   05 FILLER               PIC X(5)   VALUE 'TOTAL'.

       01 COL-HEADING2.
		   05 FILLER               PIC X(15)  VALUE 'RENTAL BUILDING'.
		   05 FILLER               PIC X      VALUE SPACES.
		   05 FILLER               PIC X(4)   VALUE 'UNIT'.
		   05 FILLER               PIC X(3)   VALUE SPACES.
		   05 FILLER               PIC X(4)   VALUE 'RENT'.

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
			       COMPUTE ROUNDED C-BASE-RATE = 650
			       IF I-TENANTS 2 THRU 3
				       MOVE 25 T0 C-TENANT-CHARGE
			       ELSE
				       IF I-TENANTS >= 4
					       MOVE 83.45 TO C-TENANT-CHARGE
				       END-IF
			       END-IF
		       WHEN 9 THRU 16
			       COMPUTE ROUNDED C-BASE-RATE = 700
			       IF I-TENANTS 2 THRU 3
				       MOVE 35.55 T0 C-TENANT-CHARGE
			       ELSE
				       IF I-TENANTS >= 4
					       MOVE 135 TO C-TENANT-CHARGE
				       END-IF
			       END-IF
		       WHEN 17 THRU 25
			       COMPUTE ROUNDED C-BASE-RATE = 825
			       IF I-TENANTS 2 THRU 3
				       MOVE 50 T0 C-TENANT-CHARGE
			       ELSE
				       IF I-TENANTS >= 4
					      MOVE 185.60 TO C-TENANT-CHARGE
				       END-IF
			       END-IF
	       END EVALUATE.
	
	       EVALUATE I-BLD-CODE
		       WHEN 'AA'
			       MOVE 'PALACE PLACE' TO O-BLD
		       WHEN 'GG'
			       MOVE 'GEORIGA' TO O-BLD
		       WHEN 'PP'
			       MOVE 'PARK PLACE' TO O-BLD
			       IF I-UNIT = 23 0R I-UNIT 25
				       COMPUTE ROUNDED C-PREM-DISC = C-BASE-RATE * 0.12
				       ADD 1 TO C-GT-PREM-CTR
		       WHEN 'IA'
			       MOVE 'IOWA CONDO' TO O-BLD
			       IF CURRENT-MONTH = 7 OR CURRENT-MONTH = 12
				       COMPUTE ROUNDED C-PREM-DISC =
                       (C-BASE-RATE * 0.5) * -1
				   ADD 1 TO C-GT-DISC-CTR
			       END-IF
		       WHEN 'MS'
			       MOVE 'MARKET STREET' TO O-BLD
		       WHEN 'HH'
			       MOVE 'HIGH TOWER' TO O-BLD
		       WHEN 'R7'
			       MOVE 'UPTOWN CONDOS' TO O-BLD
			       IF I-UNIT = 23 0R I-UNIT 25
				       COMPUTE ROUNDED C-PREM-DISC = C-BASE-RATE * 0.12
				       ADD 1 TO C-GT-PREM-CTR
		       WHEN 'GM'
			       MOVE 'GANDER MOUNTAIN' TO O-BLD
		       WHEN 'BP'
			       MOVE 'BENTON PLACE' TO O-BLD
			       COMPUTE ROUNDED C-PREM-DISC =
                   (C-BASE-RATE * 0.33) * -1
			       ADD 1 TO C-GT-DISC-CTR
		       WHEN 'GA'
			       MOVE 'GRAND AVENUE' TO O-BLD
		       WHEN 'JK'
			       MOVE 'JACKS PLACE' TO O-BLD
			       IF CURRENT-MONTH = 7 OR CURRENT-MONTH = 12
				       COMPUTE ROUNDED C-PREM-DISC =
                       (C-BASE-RATE * 0.5) * -1
				       ADD 1 TO C-GT-DISC-CTR
			       END-IF
		       WHEN 'UN'
			       MOVE 'UNDERGROUND SAM' TO O-BLD
		       WHEN 'YD'
			       MOVE 'YANKEE DOODLE' TO O-BLD
		       WHEN 'YT'
			       MOVE 'YAHTZEE AVE' TO O-BLD
			       IF I-UNIT = 23 0R I-UNIT 25
				       COMPUTE ROUNDED C-PREM-DISC = C-BASE-RATE * 0.12
				       ADD 1 TO C-GT-PREM-CTR
		        WHEN 'CP'
			       MOVE 'COURT PLACE' TO O-BLD
		       WHEN 'NZ'
			       MOVE 'NEW ZOO' TO O-BLD
		       WHEN 'VV'
			       MOVE 'VERMONT' TO O-BLD
		       WHEN 'CT'
			       MOVE 'CHINA TOWN' TO O-BLD
			       COMPUTE ROUNDED C-PREM-DISC =
                   (C-BASE-RATE * 0.33) * -1
			       ADD 1 TO C-GT-DISC-CTR
		       WHEN 'YS'
			       MOVE 'YORKSHIRE' TO O-BLD
		       WHEN 'ME'
			       MOVE 'MAINE APT' TO O-BLD
	       END-EVALUATE.
	
	       COMPUTE ROUNDED C-SUBTOTAL =
               C-TENANT-CHARGE + C-BASE-RATE + C-PREM-DISC.             
	       COMPUTE ROUNDED C-TOTAL-UTIL-COST =
               I-ELECTRIC + I-GAS + I-WATER + I-GARBAGE.
	       COMPUTE ROUNDED C-RENT-DUE = C-TOTAL-UTIL-COST + C-SUBTOTAL.
	
       L3-MOVE-PRINT.
	       MOVE I AND C FIELDS               TO D FIELDS.
	       WRITE PRTLINE FROM DETAIL-LINE
	           AFTER ADVANCING 2 LINES
		           AT EOP
		               PERFORM L4-HEADING.
			
       L3-TOTALS.
	       MOVE C-GT FIELDS                  TO GT FIELDS.
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