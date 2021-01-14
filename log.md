# Research Log

## 2020-01-12
- Weekly meeting
  - Jeannie meeting on Friday
  - Mathematical model
  - Implementation
    - LGPL not a problem for research
    - For next week: send work over cluster, job control, fix hacks
    - Verbose mode
    - Propogate over errors

## 2020-01-11
- Current network implementation
  - Using `libssh` binding `mllibssh`
    - Bindings are kinda buggy, looking for better ones and/or fix it myself
    - GNU LGPL, any issues?
  - Hacky on various levels
    - AST to string needed for execution (or send AST as Sexp + Sexp execution support)
    - Right now only automatic auth supported
    - Need good error messages - including instructions to set up automatic auth
    - Relies on ssh config
    - Name parsing fixes (specifically for special characters)
  - Issue with executable size (~20MB currently)
    - Caching is a must, based on hashing by version?
    - OCaml uses static linking
  - Features such as piping, connection restoration, etc down the line
  - Need to have data transmission rather than only command execution, probably using ssh channel/messaging features
- Network implementation plan
  - Example program flow
  - Command run
    - `hostname@@cs`
    - Figure out destination
      - Check cluster definitions
      - Check ssh config for single-machine cluster
      - Resolve hostname/address
    - Add a command send to queue, with a prepared I/O stream for the program itself
  - Command send
    - Check for existing connection with destination
      - If none exist, establish connection with destination
        - Check for running instances of `shard` with the same environment ID
          - If none exist, ensure installation of `shard`
            - Check for `shard` installation of correct version on destination
            - If none exist, copy over `shard` installation from local machine
          - Spin up instance of `shard`
        - Establish connection with said instance of `shard`
      - Set existing connection as active
    - Send over command with connection, forwarding streams
    - Streams should be on same connection as "packets" of data?
    - If fail, notify parent to resend command as needed

## 2020-01-05
- "Break" over, back to daily progress on thesis
- Back to work on implementation
  - File redirection working (only default stream)
  - Variable assignments (no scope)
- Weekly meeting
  - Thesis writing
    - Semantics section: table
    - Local: effect
    - Mathematical formulation of error handling / progress
    - Probabilistic progress
      - Random variable for every host of that host failing
      - Job: "minimize latency"
      - Compute an expectation
      - Bernoulli model
    - Monte Carlo simulation
    - Bayesian system
    - Propose a mathematical model on how well it handles failures

## 2020-12-26
- Progress towards implementing I/O redirection
  - Syntax for I/O redirection complete
  - Currently empty semantics
  - Research on method of implementation
    - Fork/open file descriptors/exec
    - May need to rewrite "new process" library to support this specific workflow
    - Future integration with remote files
    - Probably will be qutie difficult, may need to compromise on features such as "here redirection"

## 2020-12-18
- Added roadmap/implementation to do list

## 2020-12-15
- Discussion
  - Implementation to do list for Friday 
  - Background chapter: contrast stuff with my own work
  - Compare at the end of each section / on each featuby re
  - LaTeX todonotes
  - Flesh out on the distributed systems section / deeper
  - Make a table of problems and recovery mechanism

## 2020-12-13
- Draft for background chapter of the thesis

## 2020-12-13
- Progress on writing the background chapter of the thesis

## 2020-12-12
- Progress on writing the background chapter of the thesis

## 2020-12-01
- Discussion
  - Work on remote pipes, file indrection
  - Write background due 12-11
    - Also write abstract
    - Minimum 3 pages? Very loose requirement
    - Show that my work is original work by looking at related work

## 2020-11-30
- Progress towards implementing shell
  - Added `|` to pipe I/O between programs
  - Added `(` and `)`
  - Added partial inputs

## 2020-11-27
- Progress towards implementing shell
  - Added `&&` and `||`
  - Added syntax for `|` but functionality does not work
    - Need to investigate indirection (joining inputs and outputs)
  - Added `&` and `;`
  - Commands now return codes
  - Test cases for new features

## 2020-11-20
- Discussion
  - Next steps
    - Indirection
    - Start on the network stuff once indirection works
    - Similar SSH mechanism as previous work
      - Look into SSH library for OCaml
    - Thought: multiple instances on one machine?
  - PAXOS
  - The problem with distributed shells is reliability
  - Idea: google scholar alerts

## 2020-11-19
- Work on implementation of shell to conform to specs
  - Link to specs: https://pubs.opengroup.org/onlinepubs/009604499/utilities/xcu_chap02.html

## 2020-11-10
- Discussion
  - Unique niche in error handling / good frontend
  - Thesis chapters
    - Intro
    - Related work
    - Semantics
    - Types of errors
    - How the semantics handle each type of error
    - Implementation
    - Validation of implementation
    - Conclusion
  - Presentation over 
  - Implementation
    - To do list
    - Unit testing
  - Classes of applications and faults that can happen

## 2020-11-09
- Read papers on data-flow graphs and DAG pipelines in shell
- Shell implementation: Added support for subshells, updated parser

## 2020-11-08
- Shell implementation: Converted language implementation to using the `Angstrom` parser combinator library

## 2020-11-06
- Discussion
  - Existing progress
  - Making low level system calls
  - Interative mode/`curses`
  - Building an AST
  - Using a parser (parser combinators)

## 2020-11-05
- Started working on OCaml lab
  - Installation instructions:
    - Install `opam`
    - Use `opam` to install `base utop core async yojson core_extended core_bench cohttp-async async_graphics cryptokit menhir merlin ocamlformat ocaml-lsp-server`
      - Install any necessary dependencies in the shell
    - Install VSCode with the `OCaml Platform` extension by `OCamlLabs`

## 2020-11-02
- Submitted one-page description
- Set up machine

## 2020-10-30
- Add thesis one-page description
- Discussion
  - Start implementing lab
    - Potentially use OCaml
  - Focus on error semantics
  - Finish one-page description

## 2020-10-27
- Discussion
  - Table of semantics
  - Start to implement `dresh`? Maybe a bit later
  - Draft of 1 page proposal for thesis (due 2020-11-02)
  - Read `smoosh` paper

## 2020-10-26
- Added `dresh` (Distributed Reliable Easy Shell) example

## 2020-10-23
- Added error handler example
- Discussion: shift focus to distributed shell
  - Problem with distributed system / PL components
  - Primary goals: Easy to use & reliable

## 2020-10-20
- CDE paper review
- Thought experiment on how the language looks like
- What would I do in various real world situations
- Read distributed command line interface paper

## 2020-10-16
- Disussion of CDE paper
- Potentially look for new avenues for project

## 2020-10-11
- Add ssh file transfer implementation with Boost library
- Added basic remote execution over ssh

## 2020-10-06
- Project Pitch
  - Let's say you just wrote a program and you want to run it on another machine. My research is into trying to figure
  out how to do so with no dependency or build requirements. By tracking the execution of the program on the new
  machine and connecting it with the old, we can send over dependencies dynamically as they are needed, and only what
  is needed. This can help make it much easier to make prototype deployment of applications, or run programs as scripts
  on other machines, without extensive setup or prior assumptions of what is on the other machine.
  - Good parts
    - Narrative
      - Affect them personally
    - Pose questions and answer them yourself
      - Direct engagement
  - Improve
    - Slow down
      - Give them time to think and respond
    - Avoid technical terms if possible
    - Pose a scenario/as if
    - New abstraction
    - Give them a fantasy/sci-fi
  - "Allowing you to write your program to work on many machines as if it was on your own machine."
  - Implemetation
    - Start with `ssh` unix program
    - Make tunnel
  - PL and Distributed Systems communities
    - Ask Jeannie
  - "Big road theory"
    - If the road gets bigger, you're probably going in the right way

## 2020-10-03

- Research of implementation - network
  - C SSH library: libssh2
  - Cannot find an established C++ library for abstracted RPC over SSH
  - Article about using SSH for chat (https://medium.com/@shazow/ssh-how-does-it-even-9e43586e4ffc)
  - Limitations of SSH
- Program Flow
  - Local establishes ssh connection with remote
  - Open a channel that spins up a Linux Container (LXC) on the remote
  - Send program over SSH
  - Open a separate channel that captures stdio for remote program
  - Start execution of program on remote
  - Send resource requests back to local 
    - Local responds with resources as needed and give control back to program

## 2020-10-02

- Weekly secondary meeting.
  - Met with Charlie!
  - Suggestions for Implementation
    - Run on remote machine, with a "promise" from the developer that all necessary resources are local
    - Look into using chroot/Linux Containers for virtual filesystem
    - SSH Tunnel for transferring information from local to remote
      - Choose better words for the two ends involved: local = Server, remote = Client?

## 2020-09-28

- Weekly meeting.
  - System of overrides.
    - Use `dpkg` to find dependencies.
    - If no override, copy the file over.
  - Use IR versus data structure
    - Use case for data structure: simpler
    - Use case for language: optimization
  - Do something on a file on remote versus dependency
  - Default copy and send it over
  - Have a special "filesystem" indicates remote
- Weekly focus: come up with some related work
  - "Distributed computation locally"
  - Chroot jails
  - Steal syscall code
  - Get "Hello world" to run on a remote machine
    - Copy small parts

## 2020-09-22

- Added argument reading functionality to `extras/trace` program.
  - Used code from the `dodo` repository.

## 2020-09-21

- Read papers in `related_work`.
- Weekly meeting.
  - `dodo` repository.
  - Try to incorporate the argument processing from registers code.
  - Programming Language Techniques
    - Intermediate Representation (IR)
      - Define a language that captures the problem.
    - Program Synthesis
      - Find the sweet spot of a simple but powerful language
  - Process
    - Formulate a language
    - Capture dependency of original machine
    - How to change IR to run on target machine?  
    - Synthesize the change of the IR and run on target machine
  - Questions
    - What is different on the machines?
    - How to go about generating changes?
      - Enumerate exhausively
      - Find heuristics
      - Find easier way to simulate
      - Come up with algorithm
    - How do we know that we did it correctly?
  - Related Work
    - Write an annotated bibliography
    - Talk about the things that are actually related
    - Write a little blurb for each paper I read (~5 sentences)
    - Write down the key contribution from the paper
  - Novelty
    - Using syscalls to gather this info
- Description of project
  - "Execution Context Wrapper"
  - Run a program with specific arguments
  - Record down the system calls made by the program
  - Make copies of the resources used and compress
  - Generate a wrapper program that, when the original program is executing, intercepts the system calls to use the
    copied resources if the original is not present on the target system
  - Connect with a remote machine to send over the original program, the wrapper program, and the copied resources in
    a bundle
  - Potential stretch goals
    - Scan the remote machine to figure out if the programs already exist
    - Figure out which resources are not needed (e.g. selecting a document to open for an editor)
    - Fetch dynamic resources as needed

## 2020-09-15

- Wrote tracing program in `extras/trace` to experiment with the `ptrace` library.

## 2020-09-14

- First weekly meeting.
