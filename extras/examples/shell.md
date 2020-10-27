## Distributed Reliable Easy Shell

Interactive Mode
```
# Start up dresh
sh> dresh

# Create a cluster
dresh> dresh create cs-lab

# Add a machine to the cluster
dresh> dresh add cs-lab machine1.cs.williams.edu

# Reliable execution mode
# Execute the program on all remote systems in cluster
# Control passes back when all executions are complete
dresh> cs-lab@users

# This program gets all of the users running on the machines and writes them all to a local file.
dresh> cs-lab@users > users.txt

# Compare this to the following program, which writes on each remote machine.
dresh> cs-lab@(users > users.txt)

# Job execution mode
# Make sure to complete at least the nunber of jobs as specified by the number
# Greedily attempts the job on that many machines (randomly selected/depending on the cluster settings)
# If a job fails or takes a long time, try on a different machine
# The index of the job is passed to the $i variable if the job passed in is a function or a script
# Control passes back when all executions are complete
# This pipe pipes the result to a single instance of the next program
dresh> cs-lab#10@calculate.dresh | sum
# This pipe splits up each job result into its own instance of the next program
dresh> cs-lab#10@calculate.dresh @| cat

# Create a number of jobs depending on the $jobs variable
dresh> cs-lab#$jobs@calculate.dresh

# Create a number of jobs depending on the input
dresh> echo "$@" | xargs printf "%s\n" | cs-lab#@calculate.dresh

# Set default cluster
dresh> dresh default cs-lab

# Run without having to specify cluster
dresh @users > users.txt
```

Non interactive mode
```
#!/bin/dresh
# Distributed calculation example

dresh create example-cluster
dresh add example-cluster machine1@cs.williams.edu
dresh add example-cluster machine2@cs.williams.edu
dresh add example-cluster machine3@cs.williams.edu
dresh add example-cluster machine4@cs.williams.edu

dresh default example-cluster

calc()
{
    python3 -c "import math; print(math.sqrt($v))"
}

sum()
{
    python3 -c "arr = '$@'.split(' '); print(sum([int(n) for n in arr]))"
}

echo "$@" | xargs printf "%s\n" | #@calc | xargs sum
```