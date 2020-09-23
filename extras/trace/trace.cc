#include <iostream>
#include <vector>

#include <errno.h>
#include <sys/ptrace.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <sys/uio.h>
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

#define SYSCALL_NUMBER orig_rax
#define SYSCALL_RETURN rax
#define SYSCALL_ARG1 rdi
#define SYSCALL_ARG2 rsi
#define SYSCALL_ARG3 rdx
#define SYSCALL_ARG4 r10
#define SYSCALL_ARG5 r8
#define SYSCALL_ARG6 r9

// Code copied from the Dodo repository

template <typename T, T Terminator, size_t BatchSize = 128>
vector<T> readTerminatedArray(pid_t t, uintptr_t tracee_pointer) noexcept;

string readString(pid_t pid, uintptr_t tracee_pointer) noexcept {
  // Strings are just char arrays terminated by '\0'
  auto data = readTerminatedArray<char, '\0'>(pid, tracee_pointer);

  // Convert the result to a string
  return string(data.begin(), data.end());
}

// Read a value of type T from this process
template <typename T>
T readData(pid_t pid, uintptr_t tracee_pointer) noexcept {
  // Reserve space for the value we will read
  T result{};

  // If the tracee pointer is null, return the defult value
  if (tracee_pointer == 0) return result;

  // Set up iovec structs for the remote read and local write
  struct iovec local = {.iov_base = &result, .iov_len = sizeof(T)};
  struct iovec remote = {.iov_base = (void*)tracee_pointer, .iov_len = sizeof(T)};

  // Do the read
  auto rc = process_vm_readv(pid, &local, 1, &remote, 1, 0);
  if (rc != sizeof(T)) {
      throw_if_error(-1);
  }
  // Check the result
//   FAIL_IF(rc != sizeof(T)) << this << ": Error in readData(" << (void*)tracee_pointer << "). "
//                            << ERR;

  return result;
}

// Read an array of values up to a terminating value
template <typename T, T Terminator, size_t BatchSize>
vector<T> readTerminatedArray(pid_t pid, uintptr_t tracee_pointer) noexcept {
  // If the pointer is null, return an empty array
  if (tracee_pointer == 0) return vector<T>();

  // We will read BatchSize values at a time into this buffer
  T buffer[BatchSize];

  // As we go, we'll build the vector of values we read
  vector<T> result;

  // Keep track of our position in the remote array
  size_t position = 0;

  while (true) {
    // Set up iovecs to read from the array into buffer
    struct iovec local = {.iov_base = buffer, .iov_len = sizeof(buffer)};
    struct iovec remote = {.iov_base = (T*)tracee_pointer + position, .iov_len = sizeof(buffer)};

    // Do the read. The result is the number of bytes read, or -1 on failure.
    auto rc = process_vm_readv(pid, &local, 1, &remote, 1, 0);

    // Check for failure
    // FAIL_IF(rc == -1) << this << ": Error in readTerminatedArray(" << (void*)tracee_pointer << "). "
    //                   << ERR;
    throw_if_error(rc);

    // The return code is the number of bytes read. This will often be BatchSize * sizeof(T), but
    // can be smaller. Advance position (the index into the output array) by the number of
    // complete elements read.
    position += rc / sizeof(T);

    // Let the result vector know we're about to append a bunch of data
    result.reserve(result.size() + rc / sizeof(T));

    // Scan for a terminator
    for (size_t i = 0; i < rc / sizeof(T); i++) {
      // If we find a terminator, it's time to return
      if (buffer[i] == Terminator) {
        // Insert all elements from buffer up to (but not including) the terminator
        result.insert(result.end(), buffer, buffer + i);
        return result;
      }
    }

    // No terminator found. We'll do another round of reading.

    // Copy all elements from buffer into the result vector
    result.insert(result.end(), buffer, buffer + BatchSize);
  }
}

vector<string> readArgvArray(pid_t pid, uintptr_t tracee_pointer) noexcept {
  auto arg_pointers = readTerminatedArray<uintptr_t, 0>(pid, tracee_pointer);

  vector<string> args;
  for (const auto& arg_ptr : arg_pointers) {
    args.push_back(readString(pid, arg_ptr));
  }
  return args;
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
        long syscall = regs.SYSCALL_NUMBER;
        switch (syscall) {
            case SYS_open: {
                int fd = static_cast<int>(regs.SYSCALL_ARG1);
                cout << "Open" << " (fd: " << fd << ")" << endl;
                break;
            }
            case SYS_openat: {
                // int fd = static_cast<int>(regs.SYSCALL_ARG1);
                string fileName = readString(pid, regs.SYSCALL_ARG2);
                // int flags = static_cast<int>(regs.SYSCALL_ARG3);
                // mode_t mode = readData<mode_t>(pid, regs.SYSCALL_ARG4);
                cout << "Open" << " (File name: " << fileName << ")" << endl;
                break;
            }
            case SYS_close: {
                int fd = static_cast<int>(regs.SYSCALL_ARG1);
                cout << "Close" << " (fd: " << fd << ")" << endl;
                break;
            }
            case SYS_read: {
                int fd = static_cast<int>(regs.SYSCALL_ARG1);
                cout << "Read" << " (fd: " << fd << ")" << endl;
                break;
            }
            case SYS_write: {
                int fd = static_cast<int>(regs.SYSCALL_ARG1);
                cout << "Write" << " (fd: " << fd << ")" << endl;
                break;
            }
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