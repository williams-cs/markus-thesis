# Distributed, Reliable, Easy Shell

Performing tasks that are distributed among many systems currently fail to achieve each of the goals of being reliable, easy to set up, closely resembling an existing techonlogy, and friendly to users without distributed systems knowledge.
Because distributed systems are inherently unreliable, succeeding in these goals require the system to provide an abstraction for error handling and recovery that account for the many failure cases that may arise.
This thesis project attempts to define a set of error handling semanics that is simple to use, but powerful enough to be applied to a diverse set of use cases.
The project will also involve implementing a shell-based programming language to demonstrate the usefulness of the error handling sematics as defined.
Using this language, anyone with basic shell knowledge should be able to write and run scripts that runs over multiple machines reliably.
This will greatly aid in increasing the accessibility and simplifying the process of running distributed programs in domains including systems administration and scientific computing.

## Semantics

In distributed programs, many types of errors can occur that do not exist in programs running on a single machine.
These error can be split into several categories:
- Connectivity
  - Intermittent network problems
  - Remote machine not responding
  - Connection dropped
- Authentication
  - Incorrect password
  - Access forbidden
- Computational
  - Exception thrown on remote machine
- No response
  - Execution time taking too long
  - Execution stuck in loop

In our sematics, we define two execution modes that should combine to handle a large proportion of the real world use cases for simple distributed programming.

The first execution mode is "reliable mode", where the goal is to make the best effort to complete execution of the job on every machine in a cluster.

The second execution mode is "jobs mode", where the goal is to make the best effort to complete a set number of jobs on any of the machines of the cluster.

The combination of these two executions modes should cover a wide set of tasks, while exhibhiting intuitive behavior for the user. There should be a clear choice for the user as to which execution mode is desired when running a distributed command.

The specifics of each execution mode will be investigated as a part of this project, along with the potential to add more execution modes, or variations to be used to tune the existing execution modes.

## Implementation

The `dresh` shell, standing for "Distributed, Reliable, Easy, SHell", is a modification of the `sh` shell that adds features specifically to make distributed operations easy and reliable.
The `dresh` shell uses the error handling semantics as previously defined to provide a simple interface for distributed programming in a shell.

The rest of this document describes an incomplete overview of the tentatively planned semantics for the language, with the goal of showing some examples of how `dresh` can be easy to pick up and use for users that know the basics of shell.

As a prerequisite to using the distributed features of `dresh`, the user definds a "cluster" of machines that can then be used as a target for running commands, the basic building block of distributed computation. Commands can then be composed with redirections, with updated semantics to support intuitive usage for distributed commands.

## Commands

In `dresh`, commands fall under one of the following categories: user-defined functions in the same script, script files on the local machine (to be copied to the target machine), shell commands, or programs on the target machine. The user can choose to execute the command in "reliable mode", which attempts to reliably run the command on every machine in a cluster, or "jobs mode", which completes a fixed number of jobs among the machines in a cluster. These functionality will be implemented as syntax extensions on top of the `sh` language, with the tentative syntax looking like the following:

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

In `dresh`, redirections are updated to intuitively support usage with distributed commands. The pipe operator `|` can be used to pass values between various stages of distributed commands, while the file redirection operator `>` can be used to collect values into a local file. When piping values to `dresh` functions and scripts, the value is stored in a shell variable `$v` so that it can be accessed in the scope of the function or script. The redirection operators work asynchronously to increase efficiency, but coordination works in a simple and intuitive way, with the `dresh` command completing when all distributed computation is complete.

### Example of using pipes
```
# example: pipe the input to the cluster "cloud", running the command "fetchbyid", and stores the result in a local file "result.txt"
# $@ indicates the input array
# no job number indicates running on "jobs mode" using one job for each input value

$@ | cloud#@fetchbyid > result.txt

# example: do the same as above, but store the result on each remote machine instead
$@ | cloud#@(fetchbyid > result.txt)
```

## Validation

The plan for validating the error handling semantics is to use the `dresh` shell to write a suite of test programs to demonstrate that the `dresh` language is easy to use, while evaluating how the programs react to different types of failures.

Some of types errors that can be simulated to test the `dresh` program include:
- Network failures (individual/widespread, temporary/permanent)
- Abnormally slow execution
- Misconfiguration of cluster
- Random exceptions in code

In the successful scenario, these test programs can serve as examples for building practical, real world distributed scripts. Future work can include expanding on or modifying the error handling semantics as defined here, and investigating ways to improve on the existing areas of focus (including reliability and ease of use) or look into new areas of potential improvement (such as scalability or performance).
