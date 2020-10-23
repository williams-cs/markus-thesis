## Error handling in remote execution

We can define a cluster as a unit that can perform a computation.
A common type of cluster is a set of remote machines that are interchangable.
Some failure modes for individual machines in clusters (ranked from hardest to fix to easiest to fix):
- Inaccessible: The remote machine cannot be accessed.
- Permission: The local does not have permission to access the remote machine.
- Dropped: The connection between the local and remote machine is dropped.
- Exception: The code successfully runs, but running it results in an exception.
- Slow: The machine processes commands too slowly.


main.program
```
import handler h

def calculate(a,b):
    return 3 * a + b

def main():
    c = cluster([ssh("mfeng@machine1.cs.williams.edu")])
    data = [[1, 3]]
    result = run i in c with h:
        calculate(data[i][0], data[i][1])
    for n in range(result):
        print(n)
```

main.program.handlers
```
handler h:
    case 
    default:
        prompt()

```

We can have a language where, upon encountering an error that has not been handled, automatically interactively
prompts the user on what option to take. This is partially inspired by the CLisp REPL, where the user can choose to
continue execution upon encountering an exception.
Some ways to handle an issue:
- Ignore: Ignore the promblem and keep going.
- Throw: Throw an exception.
- Retry: Try again on the same machine.
- Substitute: Try again on a different machine in the cluster.
- Prompt: Interactively prompt the user for more action.

This interactive prompt can also optionally modify the code so that future encounters of the same issue can be
automatically resolved. The program can also programmatically specify how to handle each situation.
The user can also specify a custom handler file to use/store the handlers in an interactive session.