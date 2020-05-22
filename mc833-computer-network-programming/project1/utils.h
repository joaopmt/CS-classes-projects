#define MAX_MSG_SIZE 10000 // Max number of bytes each recv expects in both client and server.
#define MAX_FIELD_SIZE 5000 // Max number of bytes for each data field for the movies.

int send_msg(int new_fd, char *msg);
int rcv_msg(int new_fd, char *buffer);