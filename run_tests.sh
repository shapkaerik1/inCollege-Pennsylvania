#!/bin/bash

# ==============================================================================
# InCollege COBOL Program - Test Automation Script
# ==============================================================================
# This script compiles the InCollege program, then runs a series of automated
# tests. For each test, it cleans up old data, sets up the correct input file,
# runs the program, and saves the output to a dedicated folder.
# ==============================================================================

# --- Run these commands! ---
# chmod +x run_tests.sh
# ./run_tests.sh

# --- Configuration ---
# Variables to make the script easy to modify.
SRC_DIR="/workspace/src"
BIN_DIR="bin"
PROG_NAME="InCollege"
INPUT_DIR="Epic6-Storyx-Test-Input"
OUTPUT_DIR="Epic6-Storyx-Test-Output"

# --- 1. Backup Original Files ---
echo "--- Backing up original input/output files (if they exist)... ---"
if [ -f "InCollege-Input.txt" ]; then
    mv "InCollege-Input.txt" "InCollege-Input.txt.bak"
    echo "Original InCollege-Input.txt saved as .bak"
fi
if [ -f "InCollege-Output.txt" ]; then
    mv "InCollege-Output.txt" "InCollege-Output.txt.bak"
    echo "Original InCollege-Output.txt saved as .bak"
fi
echo


# --- 2. Compilation ---
echo "--- Compiling the InCollege program... ---"
mkdir -p "${BIN_DIR}"
if ! cobc -x -free -o "${BIN_DIR}/${PROG_NAME}" "${SRC_DIR}/${PROG_NAME}.cob"; then
    echo "!!! Compilation failed. Exiting script. !!!"
    # Restore files before exiting on failure
    if [ -f "InCollege-Input.txt.bak" ]; then mv "InCollege-Input.txt.bak" "InCollege-Input.txt"; fi
    if [ -f "InCollege-Output.txt.bak" ]; then mv "InCollege-Output.txt.bak" "InCollege-Output.txt"; fi
    exit 1
fi
echo "Compilation successful."
echo

# --- 3. Setup Test Environment ---
mkdir -p "${OUTPUT_DIR}"

# --- 4. Run Test Cases ---
# Loop through test cases from 1 to 4. You can change the '4' to match
# the number of test input files you have.
for i in {1..4}
do
    echo "--- Running Test Case ${i} ---"

    # CRITICAL: Reset the environment by deleting all data files from previous runs.
    # This ensures each test starts from a clean slate.
    # BUT: Keep ACCOUNTS.DAT because tests need existing users to log in!
    rm -f JOBS.DAT PROFILES.DAT CONNECTIONS.DAT CONNECTION_REQUESTS.DAT JOB_APPLICATIONS.DAT *.TMP

    # Check if the input file for this test case exists.
    if [ ! -f "${INPUT_DIR}/test${i}.txt" ]; then
        echo "Warning: Test input file '${INPUT_DIR}/test${i}.txt' not found. Skipping."
        continue
    fi

    # Copy the specific test case's input to the name the program expects.
    cp "${INPUT_DIR}/test${i}.txt" "InCollege-Input.txt"

    # Run the compiled program.
    "./${BIN_DIR}/${PROG_NAME}"

    # Move and rename the output file to the designated output directory.
    if [ -f "InCollege-Output.txt" ]; then
        mv "InCollege-Output.txt" "${OUTPUT_DIR}/output${i}.txt"
        echo "Test Case ${i} finished. Output saved to '${OUTPUT_DIR}/output${i}.txt'."
    else
        echo "Warning: No output file was generated for Test Case ${i}."
    fi
    echo
done

# --- 5. Final Cleanup ---
# Remove any leftover files from the last test run.
rm -f *.DAT *.TMP

# --- 6. Restore Original Files ---
echo "--- Restoring original input/output files... ---"
if [ -f "InCollege-Input.txt.bak" ]; then
    mv "InCollege-Input.txt.bak" "InCollege-Input.txt"
    echo "Original InCollege-Input.txt restored."
fi
if [ -f "InCollege-Output.txt.bak" ]; then
    mv "InCollege-Output.txt.bak" "InCollege-Output.txt"
    echo "Original InCollege-Output.txt restored."
fi
echo

echo "--- All test cases completed. ---"
