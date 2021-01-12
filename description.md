# Accessible, Reliable, Distributed Shell

Performing tasks that are distributed among many systems currently fail to achieve each of the goals of being reliable, easy to set up, closely resembling an existing technology, and friendly to users without distributed systems knowledge.
Because distributed systems are inherently unreliable, succeeding in these goals require the system to provide an abstraction for error handling and recovery that account for the many failure cases that may arise.
This thesis project attempts to define a set of error handling semantics that is simple to use, but powerful enough to be applied to a diverse set of use cases.
The project will also involve implementing a shell-based programming language to demonstrate the usefulness of the error handling semantics as defined.
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

In our semantics, we define two execution modes that should combine to handle a large proportion of the real world use cases for simple distributed programming.

The first execution mode is "reliable mode", where the goal is to make the best effort to complete execution of the job on every machine in a cluster.

The second execution mode is "jobs mode", where the goal is to make the best effort to complete a set number of jobs on any of the machines of the cluster.

The combination of these two executions modes should cover a wide set of tasks, while exhibiting intuitive behavior for the user. There should be a clear choice for the user as to which execution mode is desired when running a distributed command.

The specifics of each execution mode will be investigated as a part of this project, along with the potential to add more execution modes, or variations to be used to tune the existing execution modes.

## Implementation

The `shard` shell, standing for "SHell that is Accessible, Reliable, and Distributed" is a modification of the `sh` shell that adds features specifically to make distributed operations easy and reliable.
The `shard` shell uses the error handling semantics as previously defined to provide a simple interface for distributed programming in a shell.

The rest of this document describes an incomplete overview of the tentatively planned semantics for the language, with the goal of showing some examples of how `shard` can be easy to pick up and use for users that know the basics of shell.

As a prerequisite to using the distributed features of `shard`, the user defines a "cluster" of machines that can then be used as a target for running commands, the basic building block of distributed computation. Commands can then be composed with redirections, with updated semantics to support intuitive usage for distributed commands.

## Commands

In `shard`, commands fall under one of the following categories: user-defined functions in the same script, script files on the local machine (to be copied to the target machine), shell commands, or programs on the target machine. The user can choose to execute the command in "reliable mode", which attempts to reliably run the command on every machine in a cluster, or "jobs mode", which completes a fixed number of jobs among the machines in a cluster. These functionality will be implemented as syntax extensions on top of the `sh` language, with the tentative syntax looking like the following:

### Run a command in "reliable mode"
```
# syntax: cluster@command
# example: gets the users on each machine in the cluster "test"
test@users
```

### Run a program in "jobs mode"
```
# syntax cluster#num_jobs@command
# example: run "compute.shard" on four remote machines in the cluster "cs"
cs#4@compute.shard

# example: do the same as above but perform a number of jobs equal to the "$jobs" variable
cs#$jobs@compute.shard
```

## Redirections

In `shard`, redirections are updated to intuitively support usage with distributed commands. The pipe operator `|` can be used to pass values between various stages of distributed commands, while the file redirection operator `>` can be used to collect values into a local file. When piping values to `shard` functions and scripts, the value is stored in a shell variable `$v` so that it can be accessed in the scope of the function or script. The redirection operators work asynchronously to increase efficiency, but coordination works in a simple and intuitive way, with the `shard` command completing when all distributed computation is complete.

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

The plan for validating the error handling semantics is to use the `shard` shell to write a suite of test programs to demonstrate that the `shard` language is easy to use, while evaluating how the programs react to different types of failures.

Some of types errors that can be simulated to test the `shard` program include:
- Network failures (individual/widespread, temporary/permanent)
- Abnormally slow execution
- Misconfiguration of cluster
- Random exceptions in code

In the successful scenario, these test programs can serve as examples for building practical, real world distributed scripts. Future work can include expanding on or modifying the error handling semantics as defined here, and investigating ways to improve on the existing areas of focus (including reliability and ease of use) or look into new areas of potential improvement (such as scalability or performance).

## Related work

There has been some existing progress towards designing error handling semantics for distributed systems and creating a usable implementation for these that demonstrates the effectiveness of these semantics.
One example is the Distributed Command Line Interface, `distsh` [1]. This work focused more on implementing a generic distributed shell environment, with steps taken to make accessing a remote machine just like accessing a local one. This interface is also much more restrictive, because it only specifies semantics for connecting to a single remote machine at a time, rather than having the unit remote machine being a cluster as is the case in this project. Therefore, the end user would still have to solve the coordination and error handling issues from working with multiple remote machines simultaneously. Finally, the paper specifies that the implementation currently only works when the machine to access is localhost, so there is no evidence that `distsh` continues to work in the unreliable conditions of a real world distributed system.

Other existing shell programs exist that can be used for distributed computation. The `dsh` [2] unix program allows the user to send a shell command to all machines in a defined cluster, but there does not exist the same emphasis of reliable execution and error recovery compare to this project, and it exists as a program that can be run in a shell, rather than a shell language itself. Similarly, it is possible to use an existing shell combined with the `ssh` [3] unix program to run commands on remote machines, but there remains a similar problem with reliability and error handling.

There are other distributed systems frameworks that have an emphasis on scalable, reliable computation. Each of these frameworks have some sort of mechanism to be able to detect and handle failures that may occur in a distributed environment.
One example is `hadoop` [4], an open source distributed program framework that can be used to run MapReduce style computation on large datasets. This framework does have an emphasis on being able to detect and handle failures in a distributed environment.
Another example is `ganglia` [5] a framework for developing scalable high-performance distributed programs. Within the `ganglia` project, there is a `gexec` utility that allows executing jobs of machines on a cluster, providing data forwarding between the machines and mechanisms to be robust even in unreliable environments.
One more example is `plush` [6], a unified framework for managing various classes of programs to be executed in a wide variety of distributed environments.
Compared to one of the aforementioned full-fledged frameworks, `shard` is positioned to be a more lightweight alternative that sacrifices some of the power of managing the distributed application in exchange for being easier to setup and use, making it more suitable as an ad-hoc scripting platform rather than a high-performance computing platform.

## References

[1]
: https://pdos.csail.mit.edu/archive/6.824-2001/projects/paper-3.ps 

[2]
: https://www.netfort.gr.jp/~dancer/software/dsh.html.en

[3]
: https://www.ssh.com/ssh/

[4]
: https://hadoop.apache.org/

[5]
: https://github.com/simplegeo/ganglia

[6]
: https://static.usenix.org/event/lisa07/tech/full_papers/albrecht/albrecht_html/