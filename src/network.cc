#include "network.hh"

#include <boost/format.hpp>
#include <boost/process.hpp>

namespace bp = boost::process;

void Network::ssh_transfer_file(std::string local_path, std::string remote_path) {
    boost::format program("scp %1% %3%:%2%");
    program % local_path % remote_path % _connection_string;
    bp::system(program.str());
}