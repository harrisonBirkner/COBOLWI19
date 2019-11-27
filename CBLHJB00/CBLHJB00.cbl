       IDENTIFICATION DIVISION.
      *THIS PROGRAM READS A FILE AND CALCULATES A TOTAL NUMBER OF 
      *STUDENTS AND CREATES A REPORT.
       PROGRAM-ID. CBLHJB00.
	   AUTHOR. HARRISON BIRKNER.

       ENVIRONMENT DIVISION.
		   SELECT STUDENT-MASTER
			   ASSIGN TO 'C:\COBOLWI19\STDNTMST.DAT'
				   ORGANIZATION IS LINE SEQUENTIAL.

		   SELECT PRTOUT
	   			   ASSIGN TO 'C:\COBOLWI19\STDNTRPT.PRT'
	   				   ORGANIZATION IS RECORD SEQUENTIAL.

       DATA DIVISION.
	   FILE SECTION.
	   FD  STUDENT-MASTER
		   LABEL RECORD IS STANDARD
		   DATA RECORD IS I-REC
		   RECORD CONTAINS 49 CHARACTERS.

       01 I-REC.
	      05 I-ID                      PIC X(7).
		  05 I-NAME.
		      10 I-LNAME               PIC X(15).
			  10 I-FNAME               PIC X(15).
			  10 I-INIT                PIC X.
	      05 I-GPA                     PIC 9V99.
		  05 I-EX-STRT-SAL             PIC 9(6)V99.

	   FD PRTOUT
		   LABEL RECORD IS OMITTED
		   DATA RECORD IS PRTLINE
		   RECORD CONTAINS 132 CHARACTERS
		   LINAGE IS 60 WITH FOOTING AT 56.

	   01 PRTLINE                      PIC X(132).

       WORKING-STORAGE SECTION.
	   01  MISC.
		   05  MORE-RECS               PIC X(3)    VALUE 'YES'.
		   05  PAGE-CTR                PIC 99      VALUE 0.
		   05  C-STUD-CTR              PIC 999    VALUE 0.
		   05  CURRENT-DATE-AND-TIME.
			   10 CURRENT-YEAR         PIC X(4).
			   10 CURRENT-MONTH        PIC XX.
			   10 CURRENT-DAY          PIC XX.
			   10 CURRENT-TIME         PIC X(11).

	   01  TITLE-LINE.
		   05  FILLER                  PIC X(6)    VALUE 'DATE: '.
		   05  TITLE-DATE.
		       10  TITLE-MONTH         PIC XX.
			   10  FILLER              PIC X       VALUE '/'.
			   10  TITLE-DAY           PIC XX.
		       10  FILLER              PIC X       VALUE '/'.
			   10  TITLE-YEAR          PIC X(4).
		   05  FILLER                  PIC X(35)   VALUE SPACES.
		   05  FILLER                  PIC X(29)
			   VALUE 'WILSON S COBOL STUDENT ROSTER'.
		   05  FILLER                  PIC X(44)   VALUE SPACES.
		   05  FILLER                  PIC X(6)    VALUE 'PAGE: '.
		   05  TITLE-PAGE              PIC Z9.

	   01  COL-HEADING1.
		   05  FILLER                  PIC X(119)  VALUE SPACES.
		   05  FILLER                  PIC X(11)   VALUE 'ANTICIPATED'.
		   05  FILLER                  PIC XX      VALUE SPACES.

	   01  COL-HEADING2.
		   05  FILLER                  PIC XX      VALUE SPACES.
		   05  FILLER                  PIC XX      VALUE 'ID'.
		   05  FILLER                  PIC X(23)   VALUE SPACES.
		   05  FILLER                  PIC X(9)    VALUE 'LAST NAME'.
		   05  FILLER                  PIC X(26)   VALUE SPACES.
		   05  FILLER                  PIC X(10)   VALUE 'FIRST NAME'.
		   05  FILLER                  PIC X(26)   VALUE SPACES.
		   05  FILLER                  PIC XXX     VALUE 'GPA'.
		   05  FILLER                  PIC X(16)   VALUE SPACES.
		   05  FILLER                  PIC X(15)
		       VALUE 'STARTING SALARY'.

	   01  DETAIL-LINE.
		   05  D-ID                    PIC X(7).
		   05  FILLER                  PIC X(20)     VALUE SPACES.
		   05  D-LAST-NAME             PIC X(15).
		   05  FILLER                  PIC X(20)     VALUE SPACES.
		   05  D-FIRST-NAME            PIC X(15).
		   05  FILLER                  PIC X(20)     VALUE SPACES.
		   05  D-GPA                   PIC Z.99.
		   05  FILLER                  PIC X(18)     VALUE SPACES.
		   05  D-START-SALARY          PIC ZZZ,ZZZ.99.
		   05  FILLER                  PIC XX        VALUE SPACES.

	   01  TOTAL-LINE.
		   05  FILLER                  PIC X(54)     VALUE SPACES.
		   05  FILLER                  PIC X(15)
			   VALUE 'STUDENT COUNT: '.
		   05  T-TOTAL-COUNT           PIC ZZ9.
		   05  FILLER                  PIC X(60)     VALUE SPACES.

       PROCEDURE DIVISION.
	   L1-MAIN.
		   PERFORM L2-INIT.
		   PERFORM L2-MAINLINE
			   UNTIL MORE-RECS = "NO".
		   PERFORM L2-CLOSING.
           STOP RUN.

	   L2-INIT.
		   OPEN INPUT STUDENT-MASTER.
		   OPEN OUTPUT PRTOUT.
		   MOVE FUNCTION CURRENT-DATE  TO CURRENT-DATE-AND-TIME.
		   MOVE CURRENT-MONTH          TO TITLE-MONTH.
		   MOVE CURRENT-DAY            TO TITLE-DAY.
		   MOVE CURRENT-YEAR           TO TITLE-YEAR.
		   PERFORM L4-HEADING.
		   PERFORM L3-READ-INPUT.

	   L2-MAINLINE.
		   PERFORM L3-CALCS.
		   PERFORM L3-MOVE-PRINT.
		   PERFORM L3-READ-INPUT.

	   L2-CLOSING.
		   PERFORM L3-TOTALS.
		   CLOSE STUDENT-MASTER.
		   CLOSE PRTOUT.

       L3-READ-INPUT.
		   READ STUDENT-MASTER
			   AT END
				   MOVE 'NO' TO MORE-RECS.

	   L3-CALCS.
		   ADD 1 TO C-STUD-CTR.

	   L3-MOVE-PRINT.
		   MOVE I-ID                   TO D-ID.
		   MOVE I-FNAME                TO D-FIRST-NAME.
		   MOVE I-LNAME                 TO D-LAST-NAME.
		   MOVE I-GPA                  TO D-GPA.
		   MOVE I-EX-STRT-SAL          TO D-START-SALARY.
		   WRITE PRTLINE FROM DETAIL-LINE
			   AFTER ADVANCING 2 LINES
				   AT EOP
					   PERFORM L4-HEADING.

	   L3-TOTALS.
		   MOVE C-STUD-CTR             TO T-TOTAL-COUNT.
		   WRITE PRTLINE FROM TOTAL-LINE
			   AFTER ADVANCING 3 LINES.

	   L4-HEADING.
		   ADD 1 TO PAGE-CTR.
		   MOVE PAGE-CTR               TO TITLE-PAGE.
		   WRITE PRTLINE FROM TITLE-LINE
			   AFTER ADVANCING PAGE.
		   WRITE PRTLINE FROM COL-HEADING1
			   AFTER ADVANCING 2 LINES.
		   WRITE PRTLINE FROM COL-HEADING2
			   AFTER ADVANCING 1 LINE.
      *    THIS WOULD BE NECESSARY IF DETAIL LINE WAS SINGLE SPACED. 
	  *    WRITE PRTLINE FROM SPACES
	  *	       AFTER ADVANCING 1 LINE.

       END PROGRAM CBLHJB00.