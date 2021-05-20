# line_length_ssh.sh
# Define hosts of machine in cluster
m1=21myf1@devon.cs.williams.edu
m2=21myf1@brava.cs.williams.edu
m3=21myf1@rathi.cs.williams.edu
m4=21myf1@guernsey.cs.williams.edu

# Cleanup from prior runs
rm -f map_*.txt
rm -f all_keys.txt
rm -f unique_keys.txt
rm -f log.txt

# Start the timer
i=0
tsstart=$(date '+%s%N')

# Map operation
# Loop through each line of the input
# Make sure to read last line if the file does not end in EOF
while IFS= read -r line || [[ -n "$line" ]]; do
    # Choose a random host
    # Note that the POSIX shell does not support arrays
    target=$(echo -e "$m1\n$m2\n$m3\n$m4" | shuf -n 1)
    # Execute map job on remote host
    result=$(echo "" | ssh "$target" "(echo \"$line\" | wc -c; echo 1)")
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
    tsnow=$(date '+%s%N')
    python3 -c "print('{t},{i}'.format(t=($tsnow - $tsstart) / 1000000000, i=$i))" >> log.txt
    i=$(echo "$i+1" | bc)
done < input.txt

# Deduplicate key list
sort all_keys.txt | uniq > unique_keys.txt

# Reduce operation
# Loop through each key
# Make sure to read last line if the file does not end in EOF
while IFS= read -r line || [[ -n "$line" ]]; do
    # Choose a random host
    # Note that the POSIX shell does not support arrays
    target=$(echo -e "$m1\n$m2\n$m3\n$m4" | shuf -n 1)
    # Generate the input file depending on the key
    infile="map_$line.txt"
    # Execute reduce job on remote host
    result=$(cat "$infile" | ssh "$target" "(paste -sd+ - | bc)")
    # Print result to the standard output
    echo "$line,$result"
    tsnow=$(date '+%s%N')
    python3 -c "print('{t},{i}'.format(t=($tsnow - $tsstart) / 1000000000, i=$i))" >> log.txt
    i=$(echo "$i+1" | bc)
done < unique_keys.txt
