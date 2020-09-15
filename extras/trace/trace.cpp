#include <iostream>

#include <errno.h>
#include <sys/ptrace.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <sys/user.h>
#include <sys/wait.h>
#include <unistd.h>

using namespace std;

long throw_if_error(long maybe_err) {
    if (maybe_err == -1) {
        throw system_error(error_code(errno, system_category()));
    }
    return maybe_err;
}

// Resources:
// https://nullprogram.com/blog/2018/06/23/
// https://cpp.hotexamples.com/examples/-/-/ptrace/cpp-ptrace-function-examples.html

// True on successful completion, false on error
void trace(pid_t pid) {
    cout << "Starting trace." << endl;
    int status;
    while (true) {
        throw_if_error(ptrace(PTRACE_SYSCALL, pid, 0, 0));
        throw_if_error(waitpid(pid, &status, 0));
        if (WIFEXITED(status)) {
            break;
        }
        struct user_regs_struct regs;
        throw_if_error(ptrace(PTRACE_GETREGS, pid, 0, &regs));
        long syscall = regs.orig_rax;
        switch (syscall) {
            case SYS_open:
                cout << "Open" << endl;
                break;
            case SYS_close:
                cout << "Close" << endl;
                break;
            case SYS_read:
                cout << "Read" << endl;
                break;
            case SYS_write:
                cout << "Write" << endl;
                break;
            default:
                // unknown syscall, skip
                break;
        }
        throw_if_error(ptrace(PTRACE_SETREGS, pid, 0, &regs));
    }
}

int main(int argc, char **argv) {
    if (argc < 2) {
        cerr << "Usage: trace <program> [args]" << endl;
        return 1;
    }
    cout << "Starting trace program." << endl;
    const char* program = argv[1];
    char* const* args = &argv[1];
    pid_t child;
    switch (child = throw_if_error(fork())) {
        case 0:
            // child process
            cout << "Starting child process." << endl;
            throw_if_error(ptrace(PTRACE_TRACEME, 0, 0, 0));
            throw_if_error(execv(program, args));
            cout << "Child process done." << endl;
            break;
        default:
            throw_if_error(waitpid(child, 0, 0));
            trace(child);
            cout << "Parent process done." << endl;
            break;
    }
    return 0;
}