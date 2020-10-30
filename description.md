# Distributed, Reliable, Easy Shell

Performing tasks that are distributed among many systems currently fail to achieve each of the goals of being reliable, easy to set up, closely resembling and existing techonlogy, and friendly to users without distributed systems knowledge. This thesis project attempts to investigate the feasibility and difficulties in implementing a shell-based programming language that tries to tackle each of the previously stated goals, with the goal being to provide a working implementation that can handle tasks in real-world conditions. Using this language, anyone with basic shell knowledge should be able to write and run scripts that runs over multiple machines reliably. This will greatly aid in increasing the accessibility and simplifying the process of running distributed programs in domains including systems administration and scientific computing.

The `dresh` shell, standing for "Distributed, Reliable, Easy, SHell", is a modification of the `sh` shell that adds features specifically to make distributed operations easy and reliable. The rest of this document describes an incomplete overview of the tentatively planned semantics for the language, with the goal of showing some examples of how `dresh` can be easy to pick up and use for users that know the basics of shell.

The user definds a "cluster" of machines that can then be used as a target for running commands, the basic building block of distributed computation. These can then be composed with redirections, with updated semantics to support intuitive usage for distributed commands.

## Commands

In `dresh`, commands fall under one of the following categories: user-defined functions in the same script, script files on the local machine (to be copied to the target machine), shell commands, or programs on the target machine. The user can choose to execute the command in "reliable mode", which goes to great lengths to attempt to make the command run on every machine in a cluster, or "jobs mode", which completes a fixed number of jobs distributed in some way among the machines, which goes to great lengths to make sure all jobs have successfully executed at least once. These functionality will be implemented as syntax extensions on top of the `sh` language, with the tentative syntax looking like the following:

### Run a command in "reliable mode"
```
# syntax: cluster@command
# example: gets the users on each machine in the cluster "test"
test@users
```

### Run a program in "jobs mode"
```
# syntax cluster#num_jobs@command
# example: run "compute.dresh" on four remote machines in the cluster "cs"
cs#4@compute.dresh

# example: do the same as above but perform a number of jobs equal to the "$jobs" variable
cs#$jobs@compute.dresh
```

## Redirections

In `dresh`, redirections are updated to intuitively support usage with distributed commands. The pipe operator `|` can be used to pass values between various stages of distributed commands, while the file redirection operatore `>` can be used to collect values into a local file. When piping values to `dresh` functions and scripts, the value is stored in a shell variable `$v` so that it can be accessed in the scope of the function or script. The redirection operators work asynchronously to increase efficiency, but coordination works in a simple and intuitive way, with the `dresh` command completing when all distributed computation is complete.

### Example of using pipes
```
# example: pipe the input to the cluster "cloud", running the command "fetchbyid", and stores the result in a local file "result.txt"
# $@ indicates the input array
# no job number indicates running on "jobs mode" using one job for each input value

$@ | cloud#@fetchbyid > result.txt

# example: do the same as above, but store the result on each remote machine instead
$@ | cloud#@(fetchbyid > result.txt)
```

