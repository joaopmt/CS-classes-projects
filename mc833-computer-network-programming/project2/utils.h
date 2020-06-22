#define MAX_MSG_SIZE 10000 // Max number of bytes for a message exchanged by client and server.
#define MAX_FIELD_SIZE 5000 // Max number of bytes for each data field for the movies.
#define INITIAL_MSG "Hello, Server!" // Initial message that clients can send to the server to kickoff the communication.
#define PORT "3490"  // The port the client will be connecting to.

int rcv_msg(int sock_fd, char *msg, struct sockaddr *addr, socklen_t *addr_len);
int rcv_msg_polling(int sock_fd, char *msg, struct sockaddr *addr, socklen_t *addr_len);
int send_msg(int sock_fd, char *msg, struct sockaddr *addr, socklen_t addr_len);