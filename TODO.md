# TODO List

## Roadmap

- [ ] Network communication infrastucture
  - [ ] SSH communcation
    - Built-in authentication and security
    - Commonly used and widely present
    - Some reliability measures
  - [ ] Sending a copy of program
  - [ ] Splitting shell into server model
    - Similar to a Jupyter notebook server / Programming language server
    - Useful for continued computation across multiple SSH sessions
  - [ ] Communication with parent node
    - I/O Stream forwarding
    - Environment variables?
  - [ ] Cleanup on completion/failure
    - Use `tmp` folder?
    - Tradeoff of caching vs cleaning up
- [ ] Error handling semantics
  - [ ] Research into existing systems
  - [ ] "Jobs mode" vs "Reliable mode"
  - [ ] Design/description of system on top of network infrastructure
  - [ ] Testing against failure circumstances
- [ ] Shell features
  - See shell feature table for "mandatory features"
- [ ] Thesis writing
  - [ ] Introduction
  - [x] Background
  - [ ] Error handling semantics
  - [ ] Network communication infrastructure
  - [ ] Implementation
  - [ ] Evaluation
  - [ ] Conclusion

## Shell feature table
| Feature                | Syntax | Local | Remote | Testing |
|------------------------|--------|-------|--------|---------|
| Basic commands         | Y      | Y     |        |         |
| Pipes                  | Y      | Y     |        |         |
| File redirection       |        |       |        |         |
| Variables              |        |       |        |         |
| Globbing               |        |       |        |         |
| Environment            |        |       |        |         |
| Functions              |        |       |        |         |
| Job control            |        |       |        |         |
| Built-in functions     |        |       |        |         |

## Distributed failure modes
| Failure kind | Recovery mechanism |Citation|
|--------------|--------------------|--------|
| stragglers   |                    |        |
| node crash   |                    |        |
| node nonresponse |                |        |
| communication failure |           |        |
| program failure (crash) |         |        |
| program hang |                    |        |
| program killed by OS (e.g., segfault, OOM) | | |
| ... |                             |        |

## Shell feature dump

Superset of existing shell

Features
- Commands
- Strings
- Escaping
- Subshell
- Variable
- Globbing
- I/O Redirection
  - Pipes
- Job control
  - Jobs builtin
  - Distributed jobs information
  - Heartbeat job information
- Return codes
- Global information about machines in cluster
- Backgrounding processes
- Shell functions
- Control flow
- Shell environment

Optional features
- Named pipes
- Sockets
- Interactive mode
- Raw character devices
- Tab completion
