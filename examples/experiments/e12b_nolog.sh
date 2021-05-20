# line_length_ssh.sh
# Define hosts of machine in cluster
m1=machine1.cs.williams.edu
m2=machine2.cs.williams.edu
m3=machine3.cs.williams.edu
m4=machine4.cs.williams.edu

# Cleanup from prior runs
rm -f map_*.txt
rm -f all_keys.txt
rm -f unique_keys.txt
rm -f result.txt

map () {
    # Choose a random host
    # Note that the POSIX shell does not support arrays
    target=$(echo -e "$m1\n$m2\n$m3\n$m4" | shuf -n 1)
    # Execute map job on remote host
    result=$(echo "" | ssh "$target" "(echo \"$1\" | wc -c; echo 1)")
    # Retreive the key
    key=$(echo "$result" | head -n 1)
    # Retreive the value
    value=$(echo "$result" | tail -n 1)
    # Generate the output file depending on the key
    outfile="map_$key.txt"
    # Replace newline with comma, and append to temporary file
    echo "$value" >> $outfile
    # Keep track of the set of all keys
    echo "$key" >> all_keys.txt
}

reduce () {
    # Choose a random host
    # Note that the POSIX shell does not support arrays
    target=$(echo -e "$m1\n$m2\n$m3\n$m4" | shuf -n 1)
    # Generate the input file depending on the key
    infile="map_$1.txt"
    # Execute reduce job on remote host
    result=$(cat "$infile" | ssh "$target" "(paste -sd+ - | bc)")
    # Print result to the standard output
    echo "$line,$result" >> result.txt
}

# Map operation
# Loop through each line of the input
# Make sure to read last line if the file does not end in EOF
while IFS= read -r line || [[ -n "$line" ]]; do
    # Asynchronously call the map function, which executes a map task
    map "$line" &
done < input.txt

# Wait for tasks to complete
wait

# Deduplicate key list
sort all_keys.txt | uniq > unique_keys.txt

# Reduce operation
# Loop through each key
# Make sure to read last line if the file does not end in EOF
while IFS= read -r line || [[ -n "$line" ]]; do
    # Asynchronously call the reduce function, which executes a reduce task
    reduce "$line" &
done < unique_keys.txt

# Wait for tasks to complete
wait

# Sort results
sort -t, -nk1 result.txt