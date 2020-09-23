#include <iostream>

using namespace std;

int main(int argc, char **argv) {
    if (argc > 1) {
        cout << "Hello, " << argv[1] << "!" << endl;
    }
    else {
        cout << "Hello, world!" << endl;
    }
    return 0;
}