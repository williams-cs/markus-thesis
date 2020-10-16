#include <string>

#include <boost/format.hpp>

class Network {
    public:
        Network(std::string connection_string, std::string multiplex_path) noexcept :
            _connection_string(connection_string) {
                boost::format options("-o \"ControlMaster=auto\" -o \"ControlPath=%1%\" -o \"ControlPersist=10m\"");
                options % multiplex_path;
                _ssh_options = options.str();
            }

        void init();
        void mkdirs(std::string remote_path);
        void transfer_file(std::string local_path, std::string remote_path);
        void remote_exec(std::string remote_path);

    private:
        std::string _connection_string;
        std::string _ssh_options;
};