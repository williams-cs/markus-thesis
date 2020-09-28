# Research Log

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
