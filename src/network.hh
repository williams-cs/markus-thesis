#include <string>

class Network {
    public:
        Network(std::string connection_string) noexcept :
            _connection_string(connection_string) {}

        void ssh_transfer_file(std::string local_path, std::string remote_path);

    private:
        std::string _connection_string;
};