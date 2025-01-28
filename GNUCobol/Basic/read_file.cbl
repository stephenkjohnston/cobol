      *******************************************************************
      *                   IDENTIFICATION DIVISION                       *
      *******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. READ_FILE.

      *******************************************************************
      *                   ENVIRONMENT DIVISION                          *
      *******************************************************************
      * The ENVIRONMENT DIVISION describes the computing environment    *
      * for the program, including file handling specifications. It     *
      * contains sections like CONFIGURATION SECTION and INPUT-OUTPUT   *
      * SECTION where hardware, software, and data file characteristics *
      * are specified.                                                  *
      *******************************************************************
       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION.
       FILE-CONTROL. 
           SELECT CUSTOMER-FILE
           ASSIGN TO 'DATA/READ_CUSTOMERS.dat'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-FILE-STATUS.

      *******************************************************************
      *                        DATA DIVISION                            *
      *******************************************************************
       DATA DIVISION. 
       FILE SECTION. 
       FD CUSTOMER-FILE.
       01 CUSTOMER-RECORD.
           02 CUST-ID PIC 9(5).
           02 CUST-NAME PIC X(30).

       WORKING-STORAGE SECTION. 
       01 WS-EOF PIC X VALUE 'N'.
           88 END-OF-FILE VALUE 'Y'.
       01 WS-FILE-STATUS PIC X(2).

      *******************************************************************
      *                       PROCEDURE DIVISION                        *
      *******************************************************************
       PROCEDURE DIVISION.
           OPEN INPUT CUSTOMER-FILE
           IF WS-FILE-STATUS NOT = '00'
               DISPLAY 'Error opening file. Status: ' WS-FILE-STATUS
               STOP RUN
           END-IF
           PERFORM UNTIL END-OF-FILE
               READ CUSTOMER-FILE 
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       DISPLAY "ID: " CUST-ID
                       DISPLAY "Name: " CUST-NAME
               END-READ 
           END-PERFORM
           CLOSE CUSTOMER-FILE
           STOP RUN.
