#include "network.hh"

#include <boost/filesystem.hpp>
#include <boost/format.hpp>
#include <boost/process.hpp>

namespace fs = boost::filesystem;
namespace bp = boost::process;

void Network::remote_exec(std::string remote_path) {
    boost::format program("ssh %2% %1%");
    program % remote_path % _connection_string;
    bp::system(program.str());
}

void Network::mkdirs(std::string remote_path) {
    fs::path p(remote_path);
    fs::path parent = p.parent_path();
    boost::format program("mkdir -p %1%");
    program % parent.string();
    bp::system(program.str());
}

void Network::transfer_file(std::string local_path, std::string remote_path) {
    mkdirs(remote_path);
    boost::format program("scp %1% %3%:%2%");
    program % local_path % remote_path % _connection_string;
    bp::system(program.str());
}
