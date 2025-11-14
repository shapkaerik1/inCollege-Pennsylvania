# InCollege COBOL Program

This is a free-form COBOL program that simulates a simple, text-based professional networking application called "InCollege." It manages user accounts, profiles, job postings, network connections, and basic messaging.

## How It Works

This program is **not interactive**. It operates in a batch-processing style. It reads all commands and user inputs sequentially from a single file (`InCollege-Input.txt`) and writes all prompts, messages, and results to an output file (`InCollege-Output.txt`).

1.  The program starts and loads existing accounts (up to 5) from `ACCOUNTS.DAT` into memory.
2.  It reads the first command (e.g., "Log In") from `InCollege-Input.txt`.
3.  It processes the command and writes the result or the next prompt (e.g., "Please enter your username:") to `InCollege-Output.txt`.
4.  It then reads the *next* line from `InCollege-Input.txt` as the response to that prompt (e.g., "myusername").
5.  This process continues, reading one line at a time from the input file, until the end of the file is reached.

## Features

* **User Authentication:**
    * Create a new user account (up to a max of 5 accounts).
    * Validate password complexity (8-12 chars, 1 capital, 1 digit, 1 special char).
    * Log in with existing credentials.
* **Profile Management:**
    * Create and edit a user profile, including:
        * First Name, Last Name
        * University, Major, Graduation Year
        * "About Me" section
        * Up to 3 Experience entries
        * Up to 3 Education entries
    * View your own profile.
* **Networking:**
    * Search for other users by their full name.
    * Send connection requests to users you find.
    * View your list of pending connection requests.
    * Accept or reject pending requests.
    * View your network (a list of all established connections).
* **Job Board:**
    * Post a new job/internship (Title, Description, Employer, Location, Salary).
    * Browse all job listings.
    * Select a job to view its full details.
    * Apply for a job.
    * View a list of all jobs you have applied to.
* **Messaging:**
    * Send a text-based message to any user you are connected with.
    * View an inbox of all messages sent to you.
* **Learning:**
    * A placeholder "Learn a New Skill" menu (all items are "under construction").

## Data Files Used

This program uses several line-sequential files to persist data:

* `InCollege-Input.txt`: **(Required)** The input file containing all user commands and responses, one per line.
* `InCollege-Output.txt`: The main output file where all program prompts, errors, and results are written.
* `ACCOUNTS.DAT`: Stores user account usernames and passwords.
* `PROFILES.DAT`: Stores all user profile information.
* `JOBS.DAT`: Stores all job postings.
* `JOB_APPLICATIONS.DAT`: Stores records of which user applied to which job.
* `CONNECTIONS.DAT`: Stores established connections (pairs of usernames).
* `CONNECTION_REQUESTS.DAT`: Stores pending connection requests.
* `MESSAGES.DAT`: Stores all messages sent between users.
* `PROFILES.TMP`: A temporary file used when editing/saving profiles.
* `CONNECTIONS.TMP`: A temporary file used when managing connection requests.

## Limitations

* **Max 5 Accounts:** The program is hard-coded to load a maximum of 5 user accounts into its in-memory table.
* **Non-Interactive:** All user input must be pre-written in `InCollege-Input.txt` in the exact order the program expects.

## How to Run (using VSCode .devcontainer)

These instructions are for running the program inside the provided development container in Visual Studio Code.

1.  **Open in Container:** Open the project folder in VSCode and use the command palette (`Ctrl+Shift+P`) to select **"Dev Containers: Re-open in Container"**.
2.  **Build the Program:** With the `incollege.cob` file open, run the build task. This is often bound to `Ctrl+Shift+B`. This will compile the COBOL file into the `./bin/` directory.
3.  **Run:** In the VSCode terminal (which is now inside the container), run the compiled executable:
    ```sh
    ./bin/InCollege
    ```
4.  **Check Output:** Open `InCollege-Output.txt` to see the results.

## Example Input File (`InCollege-Input.txt`)

The program reads this file from top to bottom. Every line is a response to a prompt you would see in the output file.