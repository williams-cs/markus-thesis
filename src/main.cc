#include <iostream>
#include <boost/program_options.hpp>

#include "network.hh"

std::string REMOTE_PROGRAM_ROOT = "/tmp/remote-tmp/program/";

namespace po = boost::program_options;

int main(int argc, char **argv) {
    std::cout << "Test program" << std::endl;

    po::options_description desc("Program options");
    desc.add_options()
        ("help", "help message")
        ("host", po::value<std::string>(), "host to connect to")
        ("program", po::value<std::string>(), "program to run")
    ;

    po::variables_map vm;
    po::store(po::parse_command_line(argc, argv, desc), vm);
    po::notify(vm);

    if (vm.count("help")) {
        std::cout << desc << std::endl;
        return 1;
    }

    if (!vm.count("program")) {
        std::cout << "Program not set." << std::endl;
        std::cout << desc << std::endl;
        return 1;
    }

    std::string host;
    if (vm.count("host")) {
        host = vm["host"].as<std::string>();
    }
    else {
        host = "localhost";
    }

    Network network(host);

    std::string program = vm["program"].as<std::string>();

    std::cout << "Transferring to remote." << std::endl;

    network.transfer_file(program, REMOTE_PROGRAM_ROOT + program);

    network.remote_exec(REMOTE_PROGRAM_ROOT + program);

    std::cout << "Done." << std::endl;
    
    return 0;
}
