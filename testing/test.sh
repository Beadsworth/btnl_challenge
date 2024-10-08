#!/bin/bash

# group by start time
start_time="$(date +"%y_%m_%d-%H_%M_%S")"

# Path to the binary executable
BIN_NAME="VWAP"
BINARY="$(cabal list-bin $BIN_NAME)"
# BINARY="$(/util/prof.sh "name")"

# Directory containing the test CSV files
CSV_DIR="/testing/test_csvs"

# Directory containing the expected JSON output files (for valid inputs)
EXPECTED_DIR="/testing/expected_jsons"

# Function to normalize JSON for comparison (ignoring order)
normalize_json() {
    echo "$1" | jq -S .  # Using jq to sort JSON keys
}


run_prof() {
    name="$1"
    csv_path="$2"
    container_base_dir="/vwap"
    prof_dir="/prof"
    prof_sub_dir="$prof_dir/$start_time/$name"
    prof_path="$prof_sub_dir/profiling"
    
    # create prof dir if needed
    if [[ ! -d $prof_sub_dir ]]
    then
        mkdir -p $prof_sub_dir
    fi

    cat "$csv_path" | cabal run VWAP -- +RTS -p -po"$prof_path" -s"$prof_path.s"
}


# Run the tests
run_tests() {
    for csv_file in "$CSV_DIR"/*.csv; do
        # Get the base filename without extension
        base_name=$(basename "$csv_file" .csv)

        # Check for poorly formatted CSV files
        if ! is_well_formatted_csv "$csv_file"; then
            # echo "Test $base_name: Poorly formatted CSV - expecting failure"
            if ! output_json=$(run_prof "$base_name" "$csv_file" 2>&1); then
                echo "Test $base_name: PASS (correctly failed for poorly formatted input)"
            else
                echo "Test $base_name: FAIL (should have failed for poorly formatted input)"
                echo "Output: $output_json"
            fi
            continue
        fi

        # Expected output file
        expected_file="$EXPECTED_DIR/$base_name.json"

        # Check if expected output file exists
        if [[ ! -f "$expected_file" ]]; then
            echo "Expected output file $expected_file does not exist. Skipping."
            continue
        fi

        # Run the binary and capture the output
        output_json=$(run_prof "$base_name" "$csv_file")

        # Normalize the output and expected JSON for comparison
        normalized_output=$(normalize_json "$output_json")
        normalized_expected=$(normalize_json "$(cat "$expected_file")")

        # Compare the normalized output with the expected output
        if [[ "$normalized_output" == "$normalized_expected" ]]; then
            echo "Test $base_name: PASS"
        else
            echo "Test $base_name: FAIL"
            echo "Expected:"
            echo "$normalized_expected"
            echo "Got:"
            echo "$normalized_output"
        fi
    done
}

# Function to check if a CSV file is well formatted
is_well_formatted_csv() {
    # Basic check: for this example, we assume a well-formed CSV has a header and at least one data row
    local csv_file="$1"
    # Check if the first line has at least one comma (indicating multiple columns)
    if [[ -s "$csv_file" && $(head -n 1 "$csv_file" | grep -c ',') -eq 0 ]]; then
        return 1  # Not well formatted
    fi
    # Additional checks can be implemented as needed
    return 0  # Well formatted
}

# Run the tests
run_tests
