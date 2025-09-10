*> This is free-form
IDENTIFICATION DIVISION.
PROGRAM-ID. InCollege.

ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
                 SELECT INPUT-FILE
                     ASSIGN TO "InCollege-Input.txt"
                     ORGANIZATION IS LINE SEQUENTIAL
                     FILE STATUS IS INPUT-FILE-STATUS.
                 SELECT OUTPUT-FILE
                     ASSIGN TO "InCollege-Output.txt"
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
       01 ACCOUNTS-STATUS PIC X(2).

       *> end of file flag to control main loop
       01 WS-EOF-FLAG PIC A(1) VALUE 'N'.
           88 EOF VALUE 'Y'.

       *> variables for parsing the line
       01 WS-INPUT-LINE PIC X(80).
       01 USER-ACTION PIC X(80).
       01 USERNAME PIC X(20).
       01 PASSWORD PIC X(12).

       *> variables for password validation
       01 VALID-PASSWORD PIC X VALUE 'N'.
       01 PASSWORD-LENGTH PIC 99.
       01 I PIC 99.
       01 WS-CURRENT-CHAR PIC X.
       01 WS-HAS-CAPITAL PIC X.
       01 WS-HAS-DIGIT PIC X.
       01 WS-HAS-SPECIAL PIC X.

       *> flags to track the state of login and registration attempts
       01 WS-LOGIN-SUCCESSFUL PIC X.
       01 WS-USERNAME-IS-UNIQUE PIC X.

       *> general purpose varibale to build a line of text before displaying or writing it
       01 OUTPUT-LINE PIC X(80).

       *> in memory table (max 5 accounts)
       01 ACCOUNT-TABLE.
           *> create space for 6 ACCOUNTS records and create a pointer for the account table
           05 ACCOUNTS OCCURS 5 TIMES INDEXED BY TBL-IDX.
               10 WS-USERNAME PIC X(20).
               10 WS-PASSWORD PIC X(12).
       01 ACCOUNT-COUNT PIC 9 VALUE 0.


PROCEDURE DIVISION.
       PERFORM OPEN-FILES.
       PERFORM LOAD-ACCOUNTS.
       PERFORM MAIN-MENU-DISPLAY.
       *> main loop that drives the program, processing one command per iteration
       PERFORM PROCESS-INPUT-COMMANDS UNTIL EOF.

       *> cleanup and termination
       CLOSE INPUT-FILE OUTPUT-FILE ACCOUNTS-FILE.
       STOP RUN.

OPEN-FILES.
       *> this paragraph handles the opening of input and output files and checks for errors
       OPEN INPUT INPUT-FILE.
       IF INPUT-FILE-STATUS NOT = '00'
           MOVE "Error: Input file 'InCollege-Input.txt' not found." TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           STOP RUN
       END-IF.

       OPEN OUTPUT OUTPUT-FILE.
       IF OUTPUT-FILE-STATUS NOT = '00'
           MOVE "Error: Output file 'InCollege-Output.txt' could not be opened." TO OUTPUT-LINE
           DISPLAY OUTPUT-LINE
           STOP RUN
       END-IF.

WRITE-AND-DISPLAY.
       *> a reusable utility paragraph to both display a line to the screen and write it to the output file
       DISPLAY OUTPUT-LINE.
       MOVE OUTPUT-LINE TO OUTPUT-RECORD.
       WRITE OUTPUT-RECORD.
       MOVE SPACES TO OUTPUT-LINE.

LOAD-ACCOUNTS.
       *> This paragraph reads all existing user accounts from ACCOUNTS.DAT into the in-memory ACCOUNT-TABLE
       OPEN INPUT ACCOUNTS-FILE.
       IF ACCOUNTS-STATUS NOT = "00"
           *> If file doesn't exist (e.g first time running),exit since table is already empty
           MOVE 0 TO ACCOUNT-COUNT
           EXIT PARAGRAPH
       END-IF.

       MOVE 0 TO ACCOUNT-COUNT.
       *>loop to read every record from the file until the end.
       PERFORM UNTIL 1 = 2
           READ ACCOUNTS-FILE
               AT END
                   EXIT PERFORM
               NOT AT END
                   ADD 1 TO ACCOUNT-COUNT
                   IF ACCOUNT-COUNT <= 5
                       MOVE AR-USERNAME TO WS-USERNAME(ACCOUNT-COUNT)
                       MOVE AR-PASSWORD TO WS-PASSWORD(ACCOUNT-COUNT)
                   END-IF
           END-READ
       END-PERFORM.
       CLOSE ACCOUNTS-FILE.

ADD-AND-SAVE-ACCOUNT.
       *> this paragraph handels writing to a new account to the ACCOUNTS.DAT file for persistence
       *> and also adds it to the in-memory table for the current session
       MOVE USERNAME TO AR-USERNAME.
       MOVE PASSWORD TO AR-PASSWORD.

       *> OPEN EXTEND appends to the file and creates it if the file doesnt exist
       OPEN EXTEND ACCOUNTS-FILE.
       IF ACCOUNTS-STATUS = "35" *> File not found
           OPEN OUTPUT ACCOUNTS-FILE
       ELSE
           CLOSE ACCOUNTS-FILE
           OPEN EXTEND ACCOUNTS-FILE
       END-IF.

       WRITE ACCOUNT-RECORD.
       CLOSE ACCOUNTS-FILE.

       *> update the in-memory table and count
       ADD 1 TO ACCOUNT-COUNT.
       MOVE USERNAME TO WS-USERNAME(ACCOUNT-COUNT).
       MOVE PASSWORD TO WS-PASSWORD(ACCOUNT-COUNT).

MAIN-MENU-DISPLAY.
       *> displays the initial welcome screen and static text prompt
       MOVE "****************************************" TO OUTPUT-LINE.
       PERFORM WRITE-AND-DISPLAY.
       MOVE "*         Welcome to InCollege!        *" TO OUTPUT-LINE.
       PERFORM WRITE-AND-DISPLAY.
       MOVE "****************************************" TO OUTPUT-LINE.
       PERFORM WRITE-AND-DISPLAY.
       PERFORM WRITE-AND-DISPLAY.
       MOVE "Log In" TO OUTPUT-LINE.
       PERFORM WRITE-AND-DISPLAY.
       MOVE "Create New Account" TO OUTPUT-LINE.
       PERFORM WRITE-AND-DISPLAY.
       MOVE "Please enter your username:" TO OUTPUT-LINE.
       PERFORM WRITE-AND-DISPLAY.
       MOVE "Please enter your password:" TO OUTPUT-LINE.
       PERFORM WRITE-AND-DISPLAY.

PROCESS-INPUT-COMMANDS.
       *> The main command processor. It reads a line and decides which action to take.
       READ INPUT-FILE
           AT END
               SET EOF TO TRUE
           NOT AT END
               MOVE FUNCTION TRIM(FILE-RECORD) TO USER-ACTION

               *> switch case
               EVALUATE USER-ACTION
                   WHEN "new account"
                       PERFORM CREATE-ACCOUNT-SECTION
                   WHEN "login"
                       PERFORM LOGIN-SECTION
                   WHEN OTHER
                       MOVE "Error: Input must be 'new account' or 'login'."
                           TO OUTPUT-LINE
                       PERFORM WRITE-AND-DISPLAY
               END-EVALUATE
       END-READ.

GET-USERNAME.
       *> reads the next line from the input file and store it as the username
       READ INPUT-FILE
           AT END
               SET EOF TO TRUE
           NOT AT END
               MOVE FILE-RECORD TO USERNAME
       END-READ.

GET-PASSWORD.
       *> reads the next line from the input file and store it as the password
       READ INPUT-FILE
           AT END
               SET EOF TO TRUE
           NOT AT END
               MOVE FILE-RECORD TO PASSWORD
       END-READ.

CREATE-ACCOUNT-SECTION.
       *> handles the entire account creation workflow
       *> check if the maximum number of accounts has been reached
       IF ACCOUNT-COUNT >= 5
           MOVE "All permitted accounts have been created, please come back later."
               TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           *> Consume the next two lines (username/password) from the input file
           PERFORM GET-USERNAME
           PERFORM GET-PASSWORD
           EXIT PARAGRAPH
       END-IF.

       *> Get the first username attempt before the loop
       PERFORM GET-USERNAME.

       *> initialize the flag to 'N' to ensure the loop runs at least once
       MOVE 'N' TO WS-USERNAME-IS-UNIQUE.

       *> loop until a unique username is found or we hit the end of the file
       PERFORM UNTIL WS-USERNAME-IS-UNIQUE = 'Y' OR EOF
           *> assume the current username is unique until proven otherwise
           MOVE 'Y' TO WS-USERNAME-IS-UNIQUE
           IF ACCOUNT-COUNT > 0
               *> point to first row of the table
               SET TBL-IDX TO 1

               *> SEARCH is used is for table lookups
               SEARCH ACCOUNTS
                   AT END
                       CONTINUE
                   *> look if inputted username is already in the account table
                   WHEN WS-USERNAME(TBL-IDX) = FUNCTION TRIM(USERNAME) *>TRIM removes trailing blankspaces
                       MOVE 'N' TO WS-USERNAME-IS-UNIQUE
               END-SEARCH
           END-IF

           *> if the username was a duplicate, print an error and get the next attempt.
           IF WS-USERNAME-IS-UNIQUE = 'N' AND NOT EOF
               MOVE "Error: Username is already taken. Please try a different one." TO OUTPUT-LINE
               PERFORM WRITE-AND-DISPLAY
               PERFORM GET-USERNAME *> get the next username attempt
           END-IF
       END-PERFORM.

       *> after the loop, only proceed if a unique username was found before the EOF
       IF WS-USERNAME-IS-UNIQUE = 'Y'
           *> username is unique, so proceed with password validation
           PERFORM GET-PASSWORD
           MOVE 'N' TO VALID-PASSWORD

           *> loop until password is valid
           PERFORM UNTIL VALID-PASSWORD = 'Y' OR EOF
               PERFORM VALIDATE-PASSWORD
               IF VALID-PASSWORD = 'N' AND NOT EOF
                   PERFORM GET-PASSWORD *> get next password attempt
               END-IF
           END-PERFORM

               IF VALID-PASSWORD = 'Y'
               PERFORM ADD-AND-SAVE-ACCOUNT
               MOVE "Account created successfully." TO OUTPUT-LINE
               PERFORM WRITE-AND-DISPLAY
               MOVE "You have successfully logged in." TO OUTPUT-LINE
               PERFORM WRITE-AND-DISPLAY
               PERFORM POST-LOGIN-MENU
               PERFORM MAIN-MENU-DISPLAY
           ELSE
               MOVE "Account creation failed: A valid password was not provided."
                   TO OUTPUT-LINE
               PERFORM WRITE-AND-DISPLAY
           END-IF
       ELSE
           MOVE "Account creation failed: No unique username provided before end of file."
               TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
       END-IF.

VALIDATE-PASSWORD.
       *> this paragraph checks a password against a set of rules. It doesn't read any files
       MOVE 'N' TO VALID-PASSWORD.
       MOVE 'N' TO WS-HAS-CAPITAL.
       MOVE 'N' TO WS-HAS-DIGIT.
       MOVE 'N' TO WS-HAS-SPECIAL.
       COMPUTE PASSWORD-LENGTH = FUNCTION LENGTH(FUNCTION TRIM(PASSWORD)).

       IF PASSWORD-LENGTH < 8 OR PASSWORD-LENGTH > 12
           MOVE "Error: Password must be 8-12 characters."
               TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           EXIT PARAGRAPH
       END-IF.

       *> loop through the password one character at a time
       *> PERFORM VARYING is a for loop, with i = 1 and inecrement by 1
       PERFORM VARYING I FROM 1 BY 1 UNTIL I > PASSWORD-LENGTH
           *> (I:1) means start at the position i is currently on and take 1 character
           MOVE PASSWORD(I:1) TO WS-CURRENT-CHAR
           IF WS-CURRENT-CHAR >= 'A' AND <= 'Z'
               MOVE 'Y' TO WS-HAS-CAPITAL
           END-IF
           IF WS-CURRENT-CHAR >= '0' AND <= '9'
               MOVE 'Y' TO WS-HAS-DIGIT
           END-IF
           IF WS-CURRENT-CHAR IS NOT ALPHABETIC AND
              WS-CURRENT-CHAR IS NOT NUMERIC
               MOVE 'Y' TO WS-HAS-SPECIAL
           END-IF
       END-PERFORM.

       *> check if password met all criteria
       IF WS-HAS-CAPITAL = 'N'
           MOVE "Error: Password must have a capital letter."
               TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           EXIT PARAGRAPH
       END-IF.
       IF WS-HAS-DIGIT = 'N'
           MOVE "Error: Password must have a digit." TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           EXIT PARAGRAPH
       END-IF.
       IF WS-HAS-SPECIAL = 'N'
           MOVE "Error: Password must have a special character."
               TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           EXIT PARAGRAPH
       END-IF.

       MOVE 'Y' TO VALID-PASSWORD.

LOGIN-SECTION.
    *> this paragraph handles multiple login attempts
    *> initialize the flag to 'N' to ensure the loop runs at least once
    MOVE 'N' TO WS-LOGIN-SUCCESSFUL.

    *> loop until the user logs in successfully or EOF
    PERFORM UNTIL WS-LOGIN-SUCCESSFUL = 'Y' OR EOF
        *> get credentials inside the loop for each attempt.
        PERFORM GET-USERNAME
        IF NOT EOF
            PERFORM GET-PASSWORD
        END-IF

        *> check credentials
        IF NOT EOF
            *> inner loop searches the account table
            PERFORM VARYING I FROM 1 BY 1 UNTIL I > ACCOUNT-COUNT
                IF FUNCTION TRIM(USERNAME) = FUNCTION TRIM(WS-USERNAME(I)) AND
                   FUNCTION TRIM(PASSWORD) = FUNCTION TRIM(WS-PASSWORD(I))
                        MOVE 'Y' TO WS-LOGIN-SUCCESSFUL
                        EXIT PERFORM
                END-IF
            END-PERFORM

            *> if the inner loop finished and the flag is still 'N', the login failed
            IF WS-LOGIN-SUCCESSFUL = 'N'
                MOVE "Incorrect username/password, please try again."
                    TO OUTPUT-LINE
                PERFORM WRITE-AND-DISPLAY
            END-IF
        END-IF
    END-PERFORM.

       *> after the loop, if login sucessful, show the post-login menu
        IF WS-LOGIN-SUCCESSFUL = 'Y'
        MOVE "You have successfully logged in." TO OUTPUT-LINE
        PERFORM WRITE-AND-DISPLAY
        PERFORM POST-LOGIN-MENU
        PERFORM MAIN-MENU-DISPLAY
    END-IF.

POST-LOGIN-MENU.
       *> this paragraph is shown after a user successfully logs in.
       STRING "Welcome, " DELIMITED BY SIZE
              FUNCTION TRIM(USERNAME) DELIMITED BY SIZE
              "!" DELIMITED BY SIZE
              INTO OUTPUT-LINE.
       PERFORM WRITE-AND-DISPLAY.

       *> This loop redisplays the menu after every action
       PERFORM UNTIL USER-ACTION = "Go Back" OR EOF
           MOVE SPACES TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           MOVE "Search for a job" TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           MOVE "Find someone you know" TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           MOVE "Learn a new skill" TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           MOVE "Enter your choice:" TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY

           READ INPUT-FILE
               AT END SET EOF TO TRUE
               NOT AT END MOVE FUNCTION TRIM(FILE-RECORD) TO USER-ACTION
           END-READ

           IF NOT EOF
               EVALUATE USER-ACTION
                   WHEN "Search for a job"
                       MOVE "Job search/internship is under construction." TO OUTPUT-LINE
                       PERFORM WRITE-AND-DISPLAY
                   WHEN "Find someone you know"
                       MOVE "Find someone you know is under construction." TO OUTPUT-LINE
                       PERFORM WRITE-AND-DISPLAY
                   WHEN "Learn a new skill"
                       PERFORM LEARN-A-SKILL-SUB-MENU
                   WHEN "Go Back"
                       EXIT PERFORM
                   WHEN OTHER
                       MOVE "Invalid choice. Please try again."
                           TO OUTPUT-LINE
                       PERFORM WRITE-AND-DISPLAY
               END-EVALUATE
           END-IF
       END-PERFORM.
       MOVE SPACES TO USER-ACTION. *> reset for next main loop

LEARN-A-SKILL-SUB-MENU.
       *> this is the sub-menu for learning skills.
      PERFORM UNTIL USER-ACTION = "Go Back" OR EOF
           MOVE SPACES TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           MOVE "Learn a New Skill:" TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           MOVE "Resume Writing" TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           MOVE "Agile and Scrum Basics" TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           MOVE "Git and GitHub Fundamentals" TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           MOVE "Cloud Fundamentals" TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           MOVE "Networking Basics" TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           MOVE "Go Back" TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           MOVE "Enter your choice:" TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY

           READ INPUT-FILE
               AT END SET EOF TO TRUE
               NOT AT END MOVE FUNCTION TRIM(FILE-RECORD) TO USER-ACTION
           END-READ

           IF NOT EOF
               EVALUATE USER-ACTION
                   WHEN "Go Back"
                       EXIT PERFORM
                   WHEN "Resume Writing"
                   WHEN "Agile and Scrum Basics"
                   WHEN "Git and GitHub Fundamentals"
                   WHEN "Cloud Fundamentals"
                   WHEN "Networking Basics"
                       MOVE "This skill is under construction." TO OUTPUT-LINE
                       PERFORM WRITE-AND-DISPLAY
                   WHEN OTHER
                       MOVE "Error: Skill does not exist. Please try again." TO OUTPUT-LINE
                       PERFORM WRITE-AND-DISPLAY
               END-EVALUATE
           END-IF
      END-PERFORM.
      PERFORM POST-LOGIN-MENU.
