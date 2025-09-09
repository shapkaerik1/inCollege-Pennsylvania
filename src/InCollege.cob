*> This is free-form
IDENTIFICATION DIVISION.
PROGRAM-ID. InCollege.

ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
                  SELECT INPUT-FILE
                      ASSIGN TO "input.txt"
                      ORGANIZATION IS LINE SEQUENTIAL
                      FILE STATUS IS INPUT-FILE-STATUS.
                  SELECT OUTPUT-FILE
                      ASSIGN TO "output.txt"
                      ORGANIZATION IS LINE SEQUENTIAL
                      FILE STATUS IS OUTPUT-FILE-STATUS.
                  SELECT ACCOUNTS-FILE
                      ASSIGN TO "ACCOUNTS.DAT"
                      ORGANIZATION IS LINE SEQUENTIAL
                      FILE STATUS IS ACCOUNTS-STATUS.

DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 FILE-RECORD PIC X(80).

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD PIC X(80).

       FD ACCOUNTS-FILE.
       01 ACCOUNT-RECORD.
           05 AR-USERNAME PIC X(20).
           05 AR-PASSWORD PIC X(12).

       WORKING-STORAGE SECTION.
       *> variables for file handling
       01 INPUT-FILE-STATUS PIC XX.
       01 OUTPUT-FILE-STATUS PIC XX.

       *> end of file flag to control main loop
       01 WS-EOF-FLAG PIC A(1) VALUE 'N'.
           88 EOF VALUE 'Y'.

       *> variables for parsing the line
       01 WS-INPUT-LINE PIC X(80).
       01 USER-ACTION PIC X(80).
       01 USERNAME PIC X(80).
       01 PASSWORD PIC X(80).

       *>  to help with valid password validation
       01 VALID-PASSWORD  PIC X VALUE 'N'.

       *> variables for password validation
       01 PASSWORD-LENGTH PIC 99 VALUE ZERO.

       *> line to hold text before its displayed and written
       01 OUTPUT-LINE PIC X(80).

       *> in memory table (max 5 accounts)
       01 ACCOUNT-TABLE.
           05 ACCOUNTS OCCURS 5 TIMES INDEXED BY I.
               10 WS-USERNAME PIC X(20).
               10 WS-PASSWORD PIC X(12).
       01 ACCOUNT-COUNT PIC 9 value 0.
       01 ACCOUNTS-STATUS PIC X(2).

PROCEDURE DIVISION.
       PERFORM OPEN-FILES.
       PERFORM LOAD-ACCOUNTS.
       PERFORM MAIN-MENU.
       CLOSE INPUT-FILE OUTPUT-FILE ACCOUNTS-FILE.
       STOP RUN.

OPEN-FILES.
       OPEN INPUT INPUT-FILE
       IF INPUT-FILE-STATUS NOT = '00'
           MOVE "Error: Input file 'input.txt' not found or could not be opened." TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           CLOSE INPUT-FILE
           STOP RUN
       END-IF.

       OPEN OUTPUT OUTPUT-FILE.
       IF OUTPUT-FILE-STATUS NOT = '00'
           MOVE "Error: File 'output.txt' could not be opened for writing." TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           STOP RUN
       END-IF.

WRITE-AND-DISPLAY.
       DISPLAY OUTPUT-LINE.
       MOVE OUTPUT-LINE TO OUTPUT-RECORD.
       WRITE OUTPUT-RECORD.
       MOVE SPACES TO OUTPUT-LINE.

LOAD-ACCOUNTS.
       OPEN INPUT ACCOUNTS-FILE
       IF ACCOUNTS-STATUS NOT = "00"
           MOVE 0 TO ACCOUNT-COUNT
           EXIT PARAGRAPH
       END-IF

       MOVE 0 TO ACCOUNT-COUNT
       PERFORM UNTIL 1 = 2
           READ ACCOUNTS-FILE
               AT END
                   EXIT PERFORM
               NOT AT END
                   IF ACCOUNT-COUNT < 5
                       ADD 1 TO ACCOUNT-COUNT
                       MOVE AR-USERNAME TO WS-USERNAME(ACCOUNT-COUNT)
                       MOVE AR-PASSWORD TO WS-PASSWORD(ACCOUNT-COUNT)
                   END-IF
           END-READ
    END-PERFORM
    CLOSE ACCOUNTS-FILE.
ADD-AND-SAVE-ACCOUNT.
       MOVE USERNAME TO AR-USERNAME
       MOVE PASSWORD TO AR-PASSWORD

       OPEN EXTEND ACCOUNTS-FILE
       IF ACCOUNTS-STATUS = "35"
           OPEN OUTPUT ACCOUNTS-FILE      *> create it
           CLOSE ACCOUNTS-FILE
           OPEN EXTEND ACCOUNTS-FILE
       END-IF

       WRITE ACCOUNT-RECORD
       CLOSE ACCOUNTS-FILE.

       ADD 1 TO ACCOUNT-COUNT
       MOVE USERNAME TO WS-USERNAME(ACCOUNT-COUNT)
       MOVE PASSWORD TO WS-PASSWORD(ACCOUNT-COUNT).

MAIN-MENU.
       MOVE "****************************************" TO OUTPUT-LINE.
       PERFORM WRITE-AND-DISPLAY.
       MOVE "*                                      *" TO OUTPUT-LINE.
       PERFORM WRITE-AND-DISPLAY.
       MOVE "*         Welcome to InCollege!        *" TO OUTPUT-LINE.
       PERFORM WRITE-AND-DISPLAY.
       MOVE "*                                      *" TO OUTPUT-LINE.
       PERFORM WRITE-AND-DISPLAY.
       MOVE "****************************************" TO OUTPUT-LINE.
       PERFORM WRITE-AND-DISPLAY.

       MOVE "Log In" TO OUTPUT-LINE.
       PERFORM WRITE-AND-DISPLAY.
       MOVE "Create New Account" TO OUTPUT-LINE.
       PERFORM WRITE-AND-DISPLAY.
       MOVE "Please enter your username:" TO OUTPUT-LINE.
       PERFORM WRITE-AND-DISPLAY.
       MOVE "Please enter your password:" TO OUTPUT-LINE.
       PERFORM WRITE-AND-DISPLAY.

       PERFORM UNTIL EOF
              *> read first line to get user action (new account or login)
              READ INPUT-FILE
                  AT END
                      SET EOF TO TRUE
                  NOT AT END
                      MOVE FUNCTION TRIM(FILE-RECORD) TO USER-ACTION

                      *> direct program flow based on user choice
                      EVALUATE USER-ACTION
                          WHEN "new account"
                              PERFORM CREATE-ACCOUNT-SECTION
                          WHEN "login"
                              PERFORM LOGIN-SECTION
                          WHEN OTHER
                              MOVE "Error: first input must be 'new account' or 'login'." TO OUTPUT-LINE
                              PERFORM WRITE-AND-DISPLAY
                      END-EVALUATE
              END-READ
       END-PERFORM.

GET-USERNAME.
       READ INPUT-FILE
           AT END
               SET EOF TO TRUE
           NOT AT END
               MOVE FILE-RECORD TO USERNAME
       END-READ.

GET-PASSWORD.
       *> read the next line for the password
       READ INPUT-FILE
           AT END
               SET EOF TO TRUE
           NOT AT END
               MOVE FILE-RECORD TO PASSWORD
       END-READ.

CREATE-ACCOUNT-SECTION.
           IF ACCOUNT-COUNT >= 5
               MOVE "All permitted accounts have been created, please come back later" TO OUTPUT-LINE
               PERFORM WRITE-AND-DISPLAY
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO VALID-PASSWORD
           PERFORM GET-USERNAME
           PERFORM GET-PASSWORD
           PERFORM VALIDATE-PASSWORD UNTIL VALID-PASSWORD = 'Y' OR EOF
           IF VALID-PASSWORD = 'Y'
                PERFORM ADD-AND-SAVE-ACCOUNT
                MOVE "Account created successfully." TO OUTPUT-LINE
                PERFORM WRITE-AND-DISPLAY
                MOVE "You have successfully logged in." TO OUTPUT-LINE
                PERFORM WRITE-AND-DISPLAY
                MOVE "WELCOME [USERNAME] !" TO OUTPUT-LINE
                PERFORM WRITE-AND-DISPLAY
                PERFORM POST-LOGIN-MENU
           END-IF.

VALIDATE-PASSWORD.
       *> must have length between 8 and 12, inclusively
       COMPUTE PASSWORD-LENGTH = FUNCTION LENGTH(FUNCTION TRIM(PASSWORD))
       IF PASSWORD-LENGTH < 8 OR PASSWORD-LENGTH > 12
           MOVE "Error: Password must be between 8 and 12 characters long." TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           PERFORM GET-PASSWORD
           EXIT PARAGRAPH
       END-IF.

       *> must contain at least one capital letter
       IF PASSWORD = FUNCTION LOWER-CASE(PASSWORD)
           MOVE "Error: Password must have at leat one capital letter." TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           PERFORM GET-PASSWORD
           EXIT PARAGRAPH
       END-IF.

       MOVE 'Y' TO VALID-PASSWORD.

       *> must contain at least one digit

       *> must contain at leat one special character

LOGIN-SECTION.
       PERFORM GET-USERNAME.
       PERFORM GET-PASSWORD.

POST-LOGIN-MENU.
       MOVE "Search for a job" TO OUTPUT-LINE
       PERFORM WRITE-AND-DISPLAY
       MOVE "Find someone you know" TO OUTPUT-LINE
       PERFORM WRITE-AND-DISPLAY
       MOVE "Learn a new skill" TO OUTPUT-LINE
       PERFORM WRITE-AND-DISPLAY
       MOVE "Enter your choice:" TO OUTPUT-LINE
       PERFORM WRITE-AND-DISPLAY.
