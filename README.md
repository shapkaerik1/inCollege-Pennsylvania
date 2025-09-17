# inCollege-Pennsylvania



This project is a COBOL application that simulates a simplified version of LinkedIn for college students.
This project demonstrates login, registration, and initial navigation functionality, while ensuring:

User inputs comes from a file.
Output is displayed to the console and written identically to an output file.
Registration and password rules are enforced.
Navigation menus allow exploration of under-construction features.

->->->-> Requirements

COBOL compiler (GnuCOBOL recommended)

Install on Linux/macOS:

sudo apt-get install open-cobol

or

brew install gnu-cobol


Input and output text files (InCollege-Input.txt, InCollege-Test.txt) must exist in the project root.

 ->->->Compilation

Navigate into the src folder and compile the program:

cobc -x -o ../bin/InCollege InCollege.cob


cobc = COBOL compiler (GnuCOBOL)

-x = create an executable

-o ../bin/InCollege = output the binary in the bin directory


->->->-> Running the Program

From the project root, run:

./bin/InCollege < InCollege-Input.txt > InCollege-Output.txt


Reads inputs from InCollege-Input.txt

Displays output to the console

Redirects output into InCollege-Output.txt

To test with edge cases:

./bin/InCollege < InCollege-Test.txt > InCollege-Output.txt
