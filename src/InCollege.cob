*> This is free-form cobol program
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
                 SELECT PROFILES-FILE
                     ASSIGN TO "PROFILES.DAT"
                     ORGANIZATION IS LINE SEQUENTIAL
                     FILE STATUS IS PROFILES-STATUS.
                 SELECT TEMP-PROFILES-FILE
                     ASSIGN TO "PROFILES.TMP"
                     ORGANIZATION IS LINE SEQUENTIAL
                     FILE STATUS IS TEMP-PROFILES-STATUS.

DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 FILE-RECORD PIC X(256).

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD PIC X(256).

       FD ACCOUNTS-FILE.
       01 ACCOUNT-RECORD.
           05 AR-USERNAME PIC X(20).
           05 AR-PASSWORD PIC X(12).

       FD PROFILES-FILE.
       01 PROFILE-RECORD.
           05 PR-USERNAME             PIC X(20).
           05 PR-FIRST-NAME           PIC X(20).
           05 PR-LAST-NAME            PIC X(20).
           05 PR-UNIVERSITY           PIC X(40).
           05 PR-MAJOR                PIC X(30).
           05 PR-GRAD-YEAR            PIC 9(4).
           05 PR-ABOUT                PIC X(200).
           05 PR-EXP-COUNT            PIC 9.
           05 PR-EXP                  OCCURS 3 TIMES.
              10 PR-EXP-TITLE         PIC X(30).
              10 PR-EXP-COMPANY       PIC X(30).
              10 PR-EXP-DATES         PIC X(30).
              10 PR-EXP-DESC          PIC X(100).
           05 PR-EDU-COUNT            PIC 9.
           05 PR-EDU                  OCCURS 3 TIMES.
              10 PR-EDU-DEGREE        PIC X(30).
              10 PR-EDU-UNIV          PIC X(40).
              10 PR-EDU-YEARS         PIC X(20).

       FD TEMP-PROFILES-FILE.
       01 TMP-PROFILE-RECORD.
           05 TMP-USERNAME             PIC X(20).
           05 TMP-FIRST-NAME           PIC X(20).
           05 TMP-LAST-NAME            PIC X(20).
           05 TMP-UNIVERSITY           PIC X(40).
           05 TMP-MAJOR                PIC X(30).
           05 TMP-GRAD-YEAR            PIC 9(4).
           05 TMP-ABOUT                PIC X(200).
           05 TMP-EXP-COUNT            PIC 9.
           05 TMP-EXP                  OCCURS 3 TIMES.
              10 TMP-EXP-TITLE         PIC X(30).
              10 TMP-EXP-COMPANY       PIC X(30).
              10 TMP-EXP-DATES         PIC X(30).
              10 TMP-EXP-DESC          PIC X(100).
           05 TMP-EDU-COUNT            PIC 9.
           05 TMP-EDU                  OCCURS 3 TIMES.
              10 TMP-EDU-DEGREE        PIC X(30).
              10 TMP-EDU-UNIV          PIC X(40).
              10 TMP-EDU-YEARS         PIC X(20).

       WORKING-STORAGE SECTION.
       *> variables for file handling
       01 INPUT-FILE-STATUS PIC XX.
       01 OUTPUT-FILE-STATUS PIC XX.
       01 ACCOUNTS-STATUS PIC X(2).
       01 PROFILES-STATUS  PIC X(2).
       01 TEMP-PROFILES-STATUS PIC X(2).

       *> end of file flag to control main loop
       01 WS-EOF-FLAG PIC A(1) VALUE 'N'.
           88 EOF VALUE 'Y'.

       *> variables for parsing the line
       01 WS-INPUT-LINE PIC X(256).
       01 USER-ACTION PIC X(80).
       01 USERNAME PIC X(20).
       01 PASSWORD PIC X(80).

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
       01 OUTPUT-LINE PIC X(256).

       *> in memory table (max 5 accounts)
       01 ACCOUNT-TABLE.
           *> create space for 6 ACCOUNTS records and create a pointer for the account table
           05 ACCOUNTS OCCURS 5 TIMES INDEXED BY TBL-IDX.
               10 WS-USERNAME PIC X(20).
               10 WS-PASSWORD PIC X(12).
       01 ACCOUNT-COUNT PIC 9 VALUE 0.

       *> working storage for profile editing/viewing
       01 WS-PROFILE.
           05 WS-FIRST-NAME           PIC X(20).
           05 WS-LAST-NAME            PIC X(20).
           05 WS-UNIVERSITY           PIC X(40).
           05 WS-MAJOR                PIC X(30).
           05 WS-GRAD-YEAR-STR        PIC X(4).
           05 WS-GRAD-YEAR-NUM        PIC 9(4).
           05 WS-ABOUT                PIC X(200).
           05 WS-EXP-COUNT            PIC 9.
           05 WS-EXP                  OCCURS 3 TIMES.
              10 WS-EXP-TITLE         PIC X(30).
              10 WS-EXP-COMPANY       PIC X(30).
              10 WS-EXP-DATES         PIC X(30).
              10 WS-EXP-DESC          PIC X(100).
           05 WS-EDU-COUNT            PIC 9.
           05 WS-EDU                  OCCURS 3 TIMES.
              10 WS-EDU-DEGREE        PIC X(30).
              10 WS-EDU-UNIV          PIC X(40).
              10 WS-EDU-YEARS         PIC X(20).
       01 WS-PROFILE-FOUND            PIC X VALUE 'N'.
       01 WS-INDEX-TEXT               PIC 9.

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
            DISPLAY FUNCTION TRIM(OUTPUT-LINE TRAILING)
            STOP RUN
        END-IF.

WRITE-AND-DISPLAY.
       *> a reusable utility paragraph to both display a line to the screen and write it to the output file
       *> trim trailing spaces for cleaner output
       DISPLAY FUNCTION TRIM(OUTPUT-LINE TRAILING).
       MOVE FUNCTION TRIM(OUTPUT-LINE TRAILING) TO OUTPUT-RECORD.
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

       *> appends to the file but creates it if the file doesnt exist
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
       MOVE "*                                      *" TO OUTPUT-LINE.
       PERFORM WRITE-AND-DISPLAY.
       MOVE "*         Welcome to InCollege!        *" TO OUTPUT-LINE.
       PERFORM WRITE-AND-DISPLAY.
       MOVE "*                                      *" TO OUTPUT-LINE.
       PERFORM WRITE-AND-DISPLAY.
       MOVE "****************************************" TO OUTPUT-LINE.
       PERFORM WRITE-AND-DISPLAY.
       PERFORM WRITE-AND-DISPLAY.
       MOVE "Log In" TO OUTPUT-LINE.
       PERFORM WRITE-AND-DISPLAY.
       MOVE "Create New Account" TO OUTPUT-LINE.
       PERFORM WRITE-AND-DISPLAY.
       MOVE "Enter your selection:" TO OUTPUT-LINE.
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
                   WHEN "Create New Account"
                       PERFORM CREATE-ACCOUNT-SECTION
                   WHEN "Log In"
                       PERFORM LOGIN-SECTION
                   WHEN OTHER
                       MOVE "Error: Input must be 'Log In' or 'Create New Account'."
                           TO OUTPUT-LINE
                       PERFORM WRITE-AND-DISPLAY
               END-EVALUATE
       END-READ.

GET-USERNAME.
       *> reads the next line from the input file and store it as the username
       MOVE "Please enter your username:" TO OUTPUT-LINE.
       PERFORM WRITE-AND-DISPLAY.
       READ INPUT-FILE
           AT END
               SET EOF TO TRUE
           NOT AT END
               MOVE FILE-RECORD TO USERNAME
       END-READ.

GET-PASSWORD.
       *> reads the next line from the input file and store it as the password
       MOVE "Please enter your password:" TO OUTPUT-LINE.
       PERFORM WRITE-AND-DISPLAY.
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
               *>assume user will input username and password
               PERFORM GET-PASSWORD *> read old password and discard
               PERFORM GET-USERNAME *> get new username
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
           MOVE "Error: Password must be 8-12 characters." TO OUTPUT-LINE
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
    END-IF.

POST-LOGIN-MENU.
       MOVE SPACES TO OUTPUT-LINE
       PERFORM WRITE-AND-DISPLAY
       *> this paragraph is shown after a user successfully logs in.
       STRING "Welcome, " DELIMITED BY SIZE
              FUNCTION TRIM(USERNAME) DELIMITED BY SIZE
              "!" DELIMITED BY SIZE
              INTO OUTPUT-LINE.
       PERFORM WRITE-AND-DISPLAY.

       *> This loop redisplays the menu after every action
       PERFORM UNTIL EOF
           MOVE SPACES TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           MOVE "Create/Edit My Profile" TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           MOVE "View My Profile" TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           MOVE "Search for User" TO OUTPUT-LINE
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
               MOVE SPACES TO OUTPUT-LINE
               PERFORM WRITE-AND-DISPLAY
               EVALUATE USER-ACTION
                   WHEN "Create/Edit My Profile"
                       PERFORM CREATE-OR-EDIT-PROFILE
                   WHEN "View My Profile"
                       PERFORM VIEW-MY-PROFILE
                   WHEN "Search for User"
                       MOVE "Search for user is under construction." TO OUTPUT-LINE
                       PERFORM WRITE-AND-DISPLAY
                   WHEN "Learn a new skill"
                       PERFORM LEARN-A-SKILL-SUB-MENU
                   WHEN "Go Back"
                       MOVE SPACES TO OUTPUT-LINE
                       PERFORM WRITE-AND-DISPLAY
                       PERFORM MAIN-MENU-DISPLAY
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
                   WHEN "Resume Writing"
                   WHEN "Agile and Scrum Basics"
                   WHEN "Git and GitHub Fundamentals"
                   WHEN "Cloud Fundamentals"
                   WHEN "Networking Basics"
                       MOVE "This skill is under construction." TO OUTPUT-LINE
                       PERFORM WRITE-AND-DISPLAY
                   WHEN "Go Back"
                       EXIT PERFORM
                   WHEN OTHER
                       MOVE "Error: Skill does not exist. Please try again." TO OUTPUT-LINE
                       PERFORM WRITE-AND-DISPLAY
               END-EVALUATE
           END-IF
      END-PERFORM.

CREATE-OR-EDIT-PROFILE.
       MOVE "--- Create/Edit Profile ---" TO OUTPUT-LINE
       PERFORM WRITE-AND-DISPLAY
       *> First Name (Required)
       MOVE SPACES TO WS-FIRST-NAME
       PERFORM UNTIL FUNCTION TRIM(WS-FIRST-NAME) NOT = SPACE OR EOF
           MOVE "Enter First Name:" TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           READ INPUT-FILE
               AT END SET EOF TO TRUE
               NOT AT END MOVE FUNCTION TRIM(FILE-RECORD) TO WS-FIRST-NAME
           END-READ
           IF FUNCTION TRIM(WS-FIRST-NAME) = SPACE AND NOT EOF
               MOVE "Error: First Name is required." TO OUTPUT-LINE
               PERFORM WRITE-AND-DISPLAY
           END-IF
       END-PERFORM
       IF EOF EXIT PARAGRAPH END-IF
       *> Last Name (Required)
       MOVE SPACES TO WS-LAST-NAME
       PERFORM UNTIL FUNCTION TRIM(WS-LAST-NAME) NOT = SPACE OR EOF
           MOVE "Enter Last Name:" TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           READ INPUT-FILE
               AT END SET EOF TO TRUE
               NOT AT END MOVE FUNCTION TRIM(FILE-RECORD) TO WS-LAST-NAME
           END-READ
           IF FUNCTION TRIM(WS-LAST-NAME) = SPACE AND NOT EOF
               MOVE "Error: Last Name is required." TO OUTPUT-LINE
               PERFORM WRITE-AND-DISPLAY
           END-IF
       END-PERFORM
       IF EOF EXIT PARAGRAPH END-IF
       *> University (Required)
       MOVE SPACES TO WS-UNIVERSITY
       PERFORM UNTIL FUNCTION TRIM(WS-UNIVERSITY) NOT = SPACE OR EOF
           MOVE "Enter University/College Attended:" TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           READ INPUT-FILE
               AT END SET EOF TO TRUE
               NOT AT END MOVE FUNCTION TRIM(FILE-RECORD) TO WS-UNIVERSITY
           END-READ
           IF FUNCTION TRIM(WS-UNIVERSITY) = SPACE AND NOT EOF
               MOVE "Error: University/College is required." TO OUTPUT-LINE
               PERFORM WRITE-AND-DISPLAY
           END-IF
       END-PERFORM
       IF EOF EXIT PARAGraph END-IF
       *> Major (Required)
       MOVE SPACES TO WS-MAJOR
       PERFORM UNTIL FUNCTION TRIM(WS-MAJOR) NOT = SPACE OR EOF
           MOVE "Enter Major:" TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           READ INPUT-FILE
               AT END SET EOF TO TRUE
               NOT AT END MOVE FUNCTION TRIM(FILE-RECORD) TO WS-MAJOR
           END-READ
           IF FUNCTION TRIM(WS-MAJOR) = SPACE AND NOT EOF
               MOVE "Error: Major is required." TO OUTPUT-LINE
               PERFORM WRITE-AND-DISPLAY
           END-IF
       END-PERFORM
       IF EOF EXIT PARAGRAPH END-IF
       *> Grad Year (Required, numeric 4-digits, reasonable range)
       MOVE SPACES TO WS-GRAD-YEAR-STR
       MOVE ZEROS  TO WS-GRAD-YEAR-NUM
       PERFORM UNTIL (WS-GRAD-YEAR-STR IS NUMERIC AND
                       FUNCTION LENGTH(WS-GRAD-YEAR-STR) = 4 AND
                       WS-GRAD-YEAR-NUM >= 1900 AND WS-GRAD-YEAR-NUM <= 2100)
                       OR EOF
           MOVE "Enter Graduation Year (YYYY):" TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           READ INPUT-FILE
               AT END SET EOF TO TRUE
               NOT AT END MOVE FUNCTION TRIM(FILE-RECORD) TO WS-GRAD-YEAR-STR
           END-READ
           IF NOT EOF AND WS-GRAD-YEAR-STR IS NUMERIC AND
             FUNCTION LENGTH(WS-GRAD-YEAR-STR) = 4
               MOVE WS-GRAD-YEAR-STR TO WS-GRAD-YEAR-NUM
           END-IF
           IF NOT EOF AND NOT (WS-GRAD-YEAR-STR IS NUMERIC AND
                               FUNCTION LENGTH(WS-GRAD-YEAR-STR) = 4)
               MOVE "Error: Graduation Year must be a 4-digit number." TO OUTPUT-LINE
               PERFORM WRITE-AND-DISPLAY
           ELSE
               IF NOT EOF AND (WS-GRAD-YEAR-NUM < 1900 OR WS-GRAD-YEAR-NUM > 2100)
                   MOVE "Error: Graduation Year out of valid range (1900-2100)." TO OUTPUT-LINE
                   PERFORM WRITE-AND-DISPLAY
               END-IF
           END-IF
       END-PERFORM
       IF EOF EXIT PARAGRAPH END-IF
       *> About Me (Optional)
       MOVE "Enter About Me (optional, max 200 chars, enter blank line to skip):" TO OUTPUT-LINE
       PERFORM WRITE-AND-DISPLAY
       READ INPUT-FILE
           AT END SET EOF TO TRUE
           NOT AT END MOVE FUNCTION TRIM(FILE-RECORD) TO WS-ABOUT
       END-READ
       IF EOF EXIT PARAGRAPH END-IF

       *> Experiences (Optional up to 3)
       MOVE 0 TO WS-EXP-COUNT
       PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3 OR EOF
           MOVE "Add Experience (optional, max 3 entries. Enter 'DONE' to finish):" TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           MOVE I TO WS-INDEX-TEXT

           STRING "Experience #" WS-INDEX-TEXT " - Title:" INTO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY

           PERFORM UNTIL 1 = 2
               READ INPUT-FILE
                   AT END SET EOF TO TRUE
                   NOT AT END MOVE FUNCTION TRIM(FILE-RECORD) TO WS-INPUT-LINE
               END-READ
               IF EOF EXIT PERFORM END-IF

               IF FUNCTION TRIM(WS-INPUT-LINE) NOT = SPACE
                   EXIT PERFORM
               ELSE
                   MOVE "Error: Title is required." TO OUTPUT-LINE
                   PERFORM WRITE-AND-DISPLAY
                   STRING "Experience #" WS-INDEX-TEXT " - Title:" INTO OUTPUT-LINE
                   PERFORM WRITE-AND-DISPLAY
               END-IF
           END-PERFORM
           IF EOF EXIT PERFORM END-IF

           IF FUNCTION UPPER-CASE(FUNCTION TRIM(WS-INPUT-LINE)) = "DONE"
               EXIT PERFORM
           END-IF

           ADD 1 TO WS-EXP-COUNT
           MOVE WS-INPUT-LINE TO WS-EXP-TITLE(WS-EXP-COUNT)

           STRING "Experience #" WS-INDEX-TEXT " - Company/Organization:" INTO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           MOVE SPACES TO WS-EXP-COMPANY(WS-EXP-COUNT)
           PERFORM UNTIL FUNCTION TRIM(WS-EXP-COMPANY(WS-EXP-COUNT)) NOT = SPACE OR EOF
               READ INPUT-FILE
                   AT END SET EOF TO TRUE
                   NOT AT END MOVE FUNCTION TRIM(FILE-RECORD) TO WS-EXP-COMPANY(WS-EXP-COUNT)
               END-READ
               IF FUNCTION TRIM(WS-EXP-COMPANY(WS-EXP-COUNT)) = SPACE AND NOT EOF
                   MOVE "Error: Company/Organization is required." TO OUTPUT-LINE
                   PERFORM WRITE-AND-DISPLAY
               END-IF
           END-PERFORM
           IF EOF EXIT PERFORM END-IF

           STRING "Experience #" WS-INDEX-TEXT " - Dates (e.g., Summer 2024):" INTO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           MOVE SPACES TO WS-EXP-DATES(WS-EXP-COUNT)
           PERFORM UNTIL FUNCTION TRIM(WS-EXP-DATES(WS-EXP-COUNT)) NOT = SPACE OR EOF
               READ INPUT-FILE
                   AT END SET EOF TO TRUE
                   NOT AT END MOVE FUNCTION TRIM(FILE-RECORD) TO WS-EXP-DATES(WS-EXP-COUNT)
               END-READ
               IF FUNCTION TRIM(WS-EXP-DATES(WS-EXP-COUNT)) = SPACE AND NOT EOF
                   MOVE "Error: Dates are required." TO OUTPUT-LINE
                   PERFORM WRITE-AND-DISPLAY
               END-IF
           END-PERFORM
           IF EOF EXIT PERFORM END-IF

           STRING "Experience #" WS-INDEX-TEXT " - Description (optional, max 100 chars, blank to skip):" INTO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           READ INPUT-FILE
               AT END SET EOF TO TRUE
               NOT AT END MOVE FUNCTION TRIM(FILE-RECORD) TO WS-EXP-DESC(WS-EXP-COUNT)
           END-READ
           IF EOF EXIT PERFORM END-IF
       END-PERFORM
       IF EOF EXIT PARAGRAPH END-IF

       IF WS-EXP-COUNT = 3
           MOVE "The maximum number of experience entries have been inputted." TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
       END-IF

       *> Education (Optional up to 3)
       MOVE 0 TO WS-EDU-COUNT
       PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3 OR EOF
           MOVE "Add Education (optional, max 3 entries. Enter 'DONE' to finish):" TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           MOVE I TO WS-INDEX-TEXT

           STRING "Education #" WS-INDEX-TEXT " - Degree:" INTO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY

           PERFORM UNTIL 1 = 2
               READ INPUT-FILE
                   AT END SET EOF TO TRUE
                   NOT AT END MOVE FUNCTION TRIM(FILE-RECORD) TO WS-INPUT-LINE
               END-READ
               IF EOF EXIT PERFORM END-IF

               IF FUNCTION TRIM(WS-INPUT-LINE) NOT = SPACE
                   EXIT PERFORM
               ELSE
                   MOVE "Error: Degree is required." TO OUTPUT-LINE
                   PERFORM WRITE-AND-DISPLAY
                   STRING "Education #" WS-INDEX-TEXT " - Degree:" INTO OUTPUT-LINE
                   PERFORM WRITE-AND-DISPLAY
               END-IF
           END-PERFORM
           IF EOF EXIT PERFORM END-IF

           IF FUNCTION UPPER-CASE(FUNCTION TRIM(WS-INPUT-LINE)) = "DONE"
               EXIT PERFORM
           END-IF

           ADD 1 TO WS-EDU-COUNT
           MOVE WS-INPUT-LINE TO WS-EDU-DEGREE(WS-EDU-COUNT)

           STRING "Education #" WS-INDEX-TEXT " - University/College:" INTO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           MOVE SPACES TO WS-EDU-UNIV(WS-EDU-COUNT)
           PERFORM UNTIL FUNCTION TRIM(WS-EDU-UNIV(WS-EDU-COUNT)) NOT = SPACE OR EOF
               READ INPUT-FILE
                   AT END SET EOF TO TRUE
                   NOT AT END MOVE FUNCTION TRIM(FILE-RECORD) TO WS-EDU-UNIV(WS-EDU-COUNT)
               END-READ
               IF FUNCTION TRIM(WS-EDU-UNIV(WS-EDU-COUNT)) = SPACE AND NOT EOF
                   MOVE "Error: University/College is required." TO OUTPUT-LINE
                   PERFORM WRITE-AND-DISPLAY
               END-IF
           END-PERFORM
           IF EOF EXIT PERFORM END-IF

           STRING "Education #" WS-INDEX-TEXT " - Years Attended (e.g., 2023-2025):" INTO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           MOVE SPACES TO WS-EDU-YEARS(WS-EDU-COUNT)
           PERFORM UNTIL FUNCTION TRIM(WS-EDU-YEARS(WS-EDU-COUNT)) NOT = SPACE OR EOF
               READ INPUT-FILE
                   AT END SET EOF TO TRUE
                   NOT AT END MOVE FUNCTION TRIM(FILE-RECORD) TO WS-EDU-YEARS(WS-EDU-COUNT)
               END-READ
               IF FUNCTION TRIM(WS-EDU-YEARS(WS-EDU-COUNT)) = SPACE AND NOT EOF
                   MOVE "Error: Years Attended are required." TO OUTPUT-LINE
                   PERFORM WRITE-AND-DISPLAY
               END-IF
           END-PERFORM
           IF EOF EXIT PERFORM END-IF
       END-PERFORM

       IF WS-EDU-COUNT = 3
           MOVE "The maximum number of education entries have been inputted." TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
       END-IF

       *> Save profile
       PERFORM SAVE-CURRENT-PROFILE
       MOVE "Profile saved successfully!" TO OUTPUT-LINE
       PERFORM WRITE-AND-DISPLAY.

VIEW-MY-PROFILE.
       *> Enhanced profile display with improved formatting for better readability
       MOVE "======================================" TO OUTPUT-LINE
       PERFORM WRITE-AND-DISPLAY
       MOVE "            YOUR PROFILE" TO OUTPUT-LINE
       PERFORM WRITE-AND-DISPLAY
       MOVE "======================================" TO OUTPUT-LINE
       PERFORM WRITE-AND-DISPLAY
       MOVE SPACES TO OUTPUT-LINE
       PERFORM WRITE-AND-DISPLAY

       PERFORM LOAD-PROFILE-FOR-CURRENT-USER
       IF WS-PROFILE-FOUND = 'N'
           MOVE "Profile not found. Please create your profile first." TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           MOVE SPACES TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           EXIT PARAGRAPH
       END-IF

       *> Personal Information Section
       MOVE "PERSONAL INFORMATION:" TO OUTPUT-LINE
       PERFORM WRITE-AND-DISPLAY
       MOVE "--------------------------------------" TO OUTPUT-LINE
       PERFORM WRITE-AND-DISPLAY

       MOVE SPACES TO OUTPUT-LINE
       STRING "First Name:       " DELIMITED BY SIZE
              FUNCTION TRIM(PR-FIRST-NAME) DELIMITED BY SIZE
              INTO OUTPUT-LINE
       END-STRING
       PERFORM WRITE-AND-DISPLAY

       MOVE SPACES TO OUTPUT-LINE
       STRING "Last Name:        " DELIMITED BY SIZE
              FUNCTION TRIM(PR-LAST-NAME) DELIMITED BY SIZE
              INTO OUTPUT-LINE
       END-STRING
       PERFORM WRITE-AND-DISPLAY

       MOVE SPACES TO OUTPUT-LINE
       STRING "University:       " DELIMITED BY SIZE
              FUNCTION TRIM(PR-UNIVERSITY) DELIMITED BY SIZE
              INTO OUTPUT-LINE
       END-STRING
       PERFORM WRITE-AND-DISPLAY

       MOVE SPACES TO OUTPUT-LINE
       STRING "Major:            " DELIMITED BY SIZE
              FUNCTION TRIM(PR-MAJOR) DELIMITED BY SIZE
              INTO OUTPUT-LINE
       END-STRING
       PERFORM WRITE-AND-DISPLAY

       MOVE PR-GRAD-YEAR TO WS-GRAD-YEAR-STR
       MOVE SPACES TO OUTPUT-LINE
       STRING "Graduation Year:  " DELIMITED BY SIZE
              FUNCTION TRIM(WS-GRAD-YEAR-STR) DELIMITED BY SIZE
              INTO OUTPUT-LINE
       END-STRING
       PERFORM WRITE-AND-DISPLAY

       IF FUNCTION TRIM(PR-ABOUT) NOT = SPACE
           MOVE SPACES TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           MOVE "ABOUT ME:" TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           MOVE "--------------------------------------" TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           MOVE SPACES TO OUTPUT-LINE
           STRING FUNCTION TRIM(PR-ABOUT) DELIMITED BY SIZE
                  INTO OUTPUT-LINE
           END-STRING
           PERFORM WRITE-AND-DISPLAY
       END-IF

       *> Experience Section
       IF PR-EXP-COUNT > 0
           MOVE SPACES TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           MOVE "EXPERIENCE:" TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           MOVE "--------------------------------------" TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > PR-EXP-COUNT
               MOVE I TO WS-INDEX-TEXT
               MOVE SPACES TO OUTPUT-LINE
               STRING "Experience #" WS-INDEX-TEXT ":" DELIMITED BY SIZE
                      INTO OUTPUT-LINE
               END-STRING
               PERFORM WRITE-AND-DISPLAY

               MOVE SPACES TO OUTPUT-LINE
               STRING "  Title:         " DELIMITED BY SIZE
                      FUNCTION TRIM(PR-EXP-TITLE(I)) DELIMITED BY SIZE
                      INTO OUTPUT-LINE
               END-STRING
               PERFORM WRITE-AND-DISPLAY

               MOVE SPACES TO OUTPUT-LINE
               STRING "  Company:       " DELIMITED BY SIZE
                      FUNCTION TRIM(PR-EXP-COMPANY(I)) DELIMITED BY SIZE
                      INTO OUTPUT-LINE
               END-STRING
               PERFORM WRITE-AND-DISPLAY

               MOVE SPACES TO OUTPUT-LINE
               STRING "  Dates:         " DELIMITED BY SIZE
                      FUNCTION TRIM(PR-EXP-DATES(I)) DELIMITED BY SIZE
                      INTO OUTPUT-LINE
               END-STRING
               PERFORM WRITE-AND-DISPLAY

               IF FUNCTION TRIM(PR-EXP-DESC(I)) NOT = SPACE
                   MOVE SPACES TO OUTPUT-LINE
                   STRING "  Description:   " DELIMITED BY SIZE
                          FUNCTION TRIM(PR-EXP-DESC(I)) DELIMITED BY SIZE
                          INTO OUTPUT-LINE
                   END-STRING
                   PERFORM WRITE-AND-DISPLAY
               END-IF

               IF I < PR-EXP-COUNT
                   MOVE SPACES TO OUTPUT-LINE
                   PERFORM WRITE-AND-DISPLAY
               END-IF
           END-PERFORM
       END-IF

       *> Education Section
       IF PR-EDU-COUNT > 0
           MOVE SPACES TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           MOVE "EDUCATION:" TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY
           MOVE "--------------------------------------" TO OUTPUT-LINE
           PERFORM WRITE-AND-DISPLAY

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > PR-EDU-COUNT
               MOVE I TO WS-INDEX-TEXT
               MOVE SPACES TO OUTPUT-LINE
               STRING "Education #" WS-INDEX-TEXT ":" DELIMITED BY SIZE
                      INTO OUTPUT-LINE
               END-STRING
               PERFORM WRITE-AND-DISPLAY

               MOVE SPACES TO OUTPUT-LINE
               STRING "  Degree:        " DELIMITED BY SIZE
                      FUNCTION TRIM(PR-EDU-DEGREE(I)) DELIMITED BY SIZE
                      INTO OUTPUT-LINE
               END-STRING
               PERFORM WRITE-AND-DISPLAY

               MOVE SPACES TO OUTPUT-LINE
               STRING "  University:    " DELIMITED BY SIZE
                      FUNCTION TRIM(PR-EDU-UNIV(I)) DELIMITED BY SIZE
                      INTO OUTPUT-LINE
               END-STRING
               PERFORM WRITE-AND-DISPLAY

               MOVE SPACES TO OUTPUT-LINE
               STRING "  Years:         " DELIMITED BY SIZE
                      FUNCTION TRIM(PR-EDU-YEARS(I)) DELIMITED BY SIZE
                      INTO OUTPUT-LINE
               END-STRING
               PERFORM WRITE-AND-DISPLAY

               IF I < PR-EDU-COUNT
                   MOVE SPACES TO OUTPUT-LINE
                   PERFORM WRITE-AND-DISPLAY
               END-IF
           END-PERFORM
       END-IF

       MOVE SPACES TO OUTPUT-LINE
       PERFORM WRITE-AND-DISPLAY
       MOVE "======================================" TO OUTPUT-LINE
       PERFORM WRITE-AND-DISPLAY.

SAVE-CURRENT-PROFILE.
       *> populate PR- fields from working copy
       MOVE FUNCTION TRIM(USERNAME)      TO PR-USERNAME
       MOVE WS-FIRST-NAME                TO PR-FIRST-NAME
       MOVE WS-LAST-NAME                 TO PR-LAST-NAME
       MOVE WS-UNIVERSITY                TO PR-UNIVERSITY
       MOVE WS-MAJOR                     TO PR-MAJOR
       IF WS-GRAD-YEAR-STR IS NUMERIC AND FUNCTION LENGTH(WS-GRAD-YEAR-STR) = 4
           MOVE WS-GRAD-YEAR-STR TO PR-GRAD-YEAR
       ELSE
           MOVE ZEROS TO PR-GRAD-YEAR
       END-IF
       MOVE WS-ABOUT                     TO PR-ABOUT
        MOVE WS-EXP-COUNT                TO PR-EXP-COUNT
       PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
           MOVE WS-EXP-TITLE(I)          TO PR-EXP-TITLE(I)
           MOVE WS-EXP-COMPANY(I)        TO PR-EXP-COMPANY(I)
           MOVE WS-EXP-DATES(I)          TO PR-EXP-DATES(I)
           MOVE WS-EXP-DESC(I)           TO PR-EXP-DESC(I)
       END-PERFORM
       MOVE WS-EDU-COUNT                 TO PR-EDU-COUNT
       PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
           MOVE WS-EDU-DEGREE(I)         TO PR-EDU-DEGREE(I)
           MOVE WS-EDU-UNIV(I)           TO PR-EDU-UNIV(I)
           MOVE WS-EDU-YEARS(I)          TO PR-EDU-YEARS(I)
       END-PERFORM

       *> rebuild profiles with update-or-insert
       MOVE 'N' TO WS-PROFILE-FOUND
       OPEN INPUT PROFILES-FILE
       OPEN OUTPUT TEMP-PROFILES-FILE
       IF PROFILES-STATUS = "00"
           PERFORM UNTIL 1 = 2
               READ PROFILES-FILE
                   AT END EXIT PERFORM
                   NOT AT END
                       IF FUNCTION TRIM(PR-USERNAME) = FUNCTION TRIM(USERNAME)
                           MOVE 'Y' TO WS-PROFILE-FOUND
                           *> write UPDATED values from WS- fields
                           MOVE FUNCTION TRIM(USERNAME) TO TMP-USERNAME
                           MOVE WS-FIRST-NAME  TO TMP-FIRST-NAME
                           MOVE WS-LAST-NAME   TO TMP-LAST-NAME
                           MOVE WS-UNIVERSITY  TO TMP-UNIVERSITY
                           MOVE WS-MAJOR       TO TMP-MAJOR
                           IF WS-GRAD-YEAR-STR IS NUMERIC AND FUNCTION LENGTH(WS-GRAD-YEAR-STR) = 4
                               MOVE WS-GRAD-YEAR-STR TO TMP-GRAD-YEAR
                           ELSE
                               MOVE ZEROS TO TMP-GRAD-YEAR
                           END-IF
                           MOVE WS-ABOUT       TO TMP-ABOUT
                           MOVE WS-EXP-COUNT   TO TMP-EXP-COUNT
                           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
                               MOVE WS-EXP-TITLE(I)   TO TMP-EXP-TITLE(I)
                               MOVE WS-EXP-COMPANY(I) TO TMP-EXP-COMPANY(I)
                               MOVE WS-EXP-DATES(I)   TO TMP-EXP-DATES(I)
                               MOVE WS-EXP-DESC(I)    TO TMP-EXP-DESC(I)
                           END-PERFORM
                           MOVE WS-EDU-COUNT   TO TMP-EDU-COUNT
                           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
                               MOVE WS-EDU-DEGREE(I)  TO TMP-EDU-DEGREE(I)
                               MOVE WS-EDU-UNIV(I)    TO TMP-EDU-UNIV(I)
                               MOVE WS-EDU-YEARS(I)   TO TMP-EDU-YEARS(I)
                           END-PERFORM
                           WRITE TMP-PROFILE-RECORD
                       ELSE
                           MOVE PR-USERNAME   TO TMP-USERNAME
                           MOVE PR-FIRST-NAME TO TMP-FIRST-NAME
                           MOVE PR-LAST-NAME  TO TMP-LAST-NAME
                           MOVE PR-UNIVERSITY TO TMP-UNIVERSITY
                           MOVE PR-MAJOR      TO TMP-MAJOR
                           MOVE PR-GRAD-YEAR  TO TMP-GRAD-YEAR
                           MOVE PR-ABOUT      TO TMP-ABOUT
                           MOVE PR-EXP-COUNT  TO TMP-EXP-COUNT
                           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
                               MOVE PR-EXP-TITLE(I)   TO TMP-EXP-TITLE(I)
                               MOVE PR-EXP-COMPANY(I) TO TMP-EXP-COMPANY(I)
                               MOVE PR-EXP-DATES(I)   TO TMP-EXP-DATES(I)
                               MOVE PR-EXP-DESC(I)    TO TMP-EXP-DESC(I)
                           END-PERFORM
                           MOVE PR-EDU-COUNT  TO TMP-EDU-COUNT
                           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
                               MOVE PR-EDU-DEGREE(I)  TO TMP-EDU-DEGREE(I)
                               MOVE PR-EDU-UNIV(I)    TO TMP-EDU-UNIV(I)
                               MOVE PR-EDU-YEARS(I)   TO TMP-EDU-YEARS(I)
                           END-PERFORM
                           WRITE TMP-PROFILE-RECORD
                       END-IF
               END-READ
           END-PERFORM
       END-IF
       CLOSE PROFILES-FILE

       IF WS-PROFILE-FOUND = 'N'
           *> append as new using WS- fields
           MOVE FUNCTION TRIM(USERNAME) TO TMP-USERNAME
           MOVE WS-FIRST-NAME  TO TMP-FIRST-NAME
           MOVE WS-LAST-NAME   TO TMP-LAST-NAME
           MOVE WS-UNIVERSITY  TO TMP-UNIVERSITY
           MOVE WS-MAJOR       TO TMP-MAJOR
           IF WS-GRAD-YEAR-STR IS NUMERIC AND FUNCTION LENGTH(WS-GRAD-YEAR-STR) = 4
               MOVE WS-GRAD-YEAR-STR TO TMP-GRAD-YEAR
           ELSE
               MOVE ZEROS TO TMP-GRAD-YEAR
           END-IF
           MOVE WS-ABOUT       TO TMP-ABOUT
           MOVE WS-EXP-COUNT   TO TMP-EXP-COUNT
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
               MOVE WS-EXP-TITLE(I)   TO TMP-EXP-TITLE(I)
               MOVE WS-EXP-COMPANY(I) TO TMP-EXP-COMPANY(I)
               MOVE WS-EXP-DATES(I)   TO TMP-EXP-DATES(I)
               MOVE WS-EXP-DESC(I)    TO TMP-EXP-DESC(I)
           END-PERFORM
           MOVE WS-EDU-COUNT   TO TMP-EDU-COUNT
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
               MOVE WS-EDU-DEGREE(I)  TO TMP-EDU-DEGREE(I)
               MOVE WS-EDU-UNIV(I)    TO TMP-EDU-UNIV(I)
               MOVE WS-EDU-YEARS(I)   TO TMP-EDU-YEARS(I)
           END-PERFORM
           WRITE TMP-PROFILE-RECORD
       END-IF
       CLOSE TEMP-PROFILES-FILE

       OPEN INPUT TEMP-PROFILES-FILE
       OPEN OUTPUT PROFILES-FILE
       PERFORM UNTIL 1 = 2
           READ TEMP-PROFILES-FILE
               AT END EXIT PERFORM
               NOT AT END
                   MOVE TMP-USERNAME   TO PR-USERNAME
                   MOVE TMP-FIRST-NAME TO PR-FIRST-NAME
                   MOVE TMP-LAST-NAME  TO PR-LAST-NAME
                   MOVE TMP-UNIVERSITY TO PR-UNIVERSITY
                   MOVE TMP-MAJOR      TO PR-MAJOR
                   MOVE TMP-GRAD-YEAR  TO PR-GRAD-YEAR
                   MOVE TMP-ABOUT      TO PR-ABOUT
                   MOVE TMP-EXP-COUNT  TO PR-EXP-COUNT
                   PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
                       MOVE TMP-EXP-TITLE(I)   TO PR-EXP-TITLE(I)
                       MOVE TMP-EXP-COMPANY(I) TO PR-EXP-COMPANY(I)
                       MOVE TMP-EXP-DATES(I)   TO PR-EXP-DATES(I)
                       MOVE TMP-EXP-DESC(I)    TO PR-EXP-DESC(I)
                   END-PERFORM
                   MOVE TMP-EDU-COUNT  TO PR-EDU-COUNT
                   PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
                       MOVE TMP-EDU-DEGREE(I)  TO PR-EDU-DEGREE(I)
                       MOVE TMP-EDU-UNIV(I)    TO PR-EDU-UNIV(I)
                       MOVE TMP-EDU-YEARS(I)   TO PR-EDU-YEARS(I)
                   END-PERFORM
                   WRITE PROFILE-RECORD
           END-READ
       END-PERFORM
       CLOSE PROFILES-FILE
       CLOSE TEMP-PROFILES-FILE.

LOAD-PROFILE-FOR-CURRENT-USER.
       MOVE 'N' TO WS-PROFILE-FOUND
       OPEN INPUT PROFILES-FILE
       IF PROFILES-STATUS NOT = "00"
           EXIT PARAGRAPH
       END-IF
       PERFORM UNTIL 1 = 2
           READ PROFILES-FILE
               AT END EXIT PERFORM
               NOT AT END
                   IF FUNCTION TRIM(PR-USERNAME) = FUNCTION TRIM(USERNAME)
                       MOVE 'Y' TO WS-PROFILE-FOUND
                       EXIT PERFORM
                   END-IF
           END-READ
       END-PERFORM
       CLOSE PROFILES-FILE.
