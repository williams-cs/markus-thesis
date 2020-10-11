#include <iostream>

#include "network.hh"

int main() {
  std::cout << "Test program." << std::endl;

  Network network("localhost");

  network.ssh_transfer_file("tmp/in.txt", "~/programming/markus-thesis/tmp/out.txt");

  std::cout << "Done." << std::endl;
  
  return 0;
}
