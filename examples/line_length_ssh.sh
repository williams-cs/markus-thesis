# line_length_ssh.sh
# Define hosts of machine in cluster
m1=machine1.cs.williams.edu
m2=machine2.cs.williams.edu
m3=machine3.cs.williams.edu
m4=machine4.cs.williams.edu

# Map operation
# Loop through each line of the input
while IFS= read -r line; do
    # Choose a random host
    # Note that the POSIX shell does not support arrays
    target=$(echo -e "$m1\n$m2\n$m3\n$m4" | shuf)
    # Execute map job on remote host
    result=$(ssh "$target" "echo '$line' | wc -c; echo 1")
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
done < input.txt

# Deduplicate key list
sort all_keys.txt | uniq -u > unique_keys.txt

# Reduce operation
# Loop through each key
while IFS= read -r line; do
    # Choose a random host
    # Note that the POSIX shell does not support arrays
    target=$(echo -e "$m1\n$m2\n$m3\n$m4" | shuf)
    # Generate the input file depending on the key
    infile="map_$line.txt"
    # Execute reduce job on remote host
    result=$(cat "$infile" | ssh "$target" "paste -sd+ - | bc")
    # Print result to the standard output
    echo "$line,$result"
done < unique_keys.txt
