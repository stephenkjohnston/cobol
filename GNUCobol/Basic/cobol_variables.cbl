      *******************************************************************
      *                   IDENTIFICATION DIVISION                       *
      *******************************************************************
      * The IDENTIFICATION DIVISION provides metadata about our program,*
      * and is one of two mandatory DIVISIONS in COBOL. The PROGRAM-ID  *
      * is the only required entry within this DIVISION.                *
      *******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBOLVARIABLES.


      *******************************************************************
      *                        DATA DIVISION                            *
      *******************************************************************
      * The DATA DIVISION is where variables are declared. The DATA     *
      * DIVISION is optional. The DATA DIVISION has three sections, but *
      * we'll only use the WORKING-STORAGE SECTION to declare our       *
      * variables.                                                      *
      *******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *******************************************************************
      *                         VARIABLES                               *
      * Variables have a level number (like 01), a name, and a PICTURE  *
      * clause for data type/format, except for GROUP variables which   *
      * don't have a PICTURE clause. In COBOL, the PICTURE clause       *
      * defines what kind of data (like numbers or letters) and how     *
      * much of it (its length) a variable can hold.                    *
      *                                                                 *
      * Basic data types:                                               *
      *                                                                 *
      * - 'X' for alphanumeric (letters, numbers, symbols)              *
      * - 'A' for alphabetic (letters and spaces)
      * - '9' for numeric digits                                        *
      * - 'V' for implied decimal point (used with '9')                 *
      *                                                                 *      
      *******************************************************************

      *******************************************************************
      *                   INDIVIDUAL VARIABLE                           *
      * This is an example of an individual variable with a fixed       *
      * length of 15 characters. The 'X' means this variable            *
      * can hold any combination of letters, numbers, or symbols.       *
      *******************************************************************

      * An alphanumeric variable that can hold up to 15 characters.
       01 WS-PET-NAME         PIC A(15).

      * A numeric variable that can hold an integer value up to 99.
       01 WS-CRAYON-COUNT     PIC 99.

      * A decimal variable that can store numbers like 99.99
       01 WS-GROCERY-BILL     PIC 9(2)V9(2).

      *******************************************************************
      *                      GROUP VARIABLE                             *
      * A group variable, similar to an object in other languages,      *
      * lacks a PICTURE clause and is used to organize related data. It *
      * includes multiple elementary items, which can be seen as        *
      * attributes or properties of the group.                          *
      *******************************************************************
       01 WS-CUSTOMER.
           02 WS-CUST-ID PIC X(6).
           02 WS-CUST-FNAME PIC A(15).
      
      
      *******************************************************************
      *                     PROCEDURE DIVISION                          *
      *******************************************************************
      * The PROCEDURE DIVISION is where the program's logic is written. *
      *******************************************************************
       PROCEDURE DIVISION.
      
      * The MOVE statement is used to assign a value to the variable.
           MOVE "Bear" TO WS-PET-NAME.
           MOVE 24 TO WS-CRAYON-COUNT.
           MOVE 19.83 TO WS-GROCERY-BILL.

           MOVE "CS0001" TO WS-CUST-ID.
           MOVE "Stephen" TO WS-CUST-FNAME.

      * The DISPLAY statement prints the contents to the output device,
      * in this case, the console window.
           DISPLAY "Pet Name: " WS-PET-NAME.
           DISPLAY "Crayon Count: " WS-CRAYON-COUNT.
           DISPLAY "Kevin's Grocery Bill: " WS-GROCERY-BILL.

           DISPLAY "Customer ID: " WS-CUST-ID.
           DISPLAY "Customer First Name: " WS-CUST-FNAME.

      * The STOP RUN statement ends the program
           STOP RUN.
