# Research Log

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
