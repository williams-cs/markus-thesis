#include <iostream>
#include <boost/date_time.hpp>
#include <boost/filesystem.hpp>
#include <boost/program_options.hpp>

#include "network.hh"

std::string REMOTE_TMP = "/tmp/remote-tmp/";
std::string LOCAL_TMP = "/tmp/local-tmp/";

namespace fs = boost::filesystem;
namespace po = boost::program_options;
namespace pt = boost::posix_time;

uint64_t current_timestamp() {
    pt::ptime time_since_epoch(boost::gregorian::date(1970,1,1));
    pt::ptime time_now = pt::microsec_clock::local_time();
    pt::time_duration diff = time_now - time_since_epoch;
    return diff.total_milliseconds();
}

std::string local_tmp_folder(uint64_t id) {
    boost::format output("%1%%2%/");
    output % LOCAL_TMP % id;
    return output.str();
}

std::string remote_tmp_folder(uint64_t id) {
    boost::format output("%1%%2%/");
    output % REMOTE_TMP % id;
    return output.str();
}

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

    uint64_t timestamp = current_timestamp();
    std::string tmp = local_tmp_folder(timestamp);
    fs::create_directories(tmp);

    Network network(host, tmp + "multiplex");

    std::string program = vm["program"].as<std::string>();

    std::cout << "Transferring to remote." << std::endl;

    boost::format remoteRoot("%1%program/%2%");
    remoteRoot % remote_tmp_folder(timestamp) % program;

    network.transfer_file(program, remoteRoot.str());

    network.remote_exec(remoteRoot.str());

    std::cout << "Done." << std::endl;
    
    return 0;
}
