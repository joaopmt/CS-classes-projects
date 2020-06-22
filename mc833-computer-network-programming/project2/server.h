void initialize_server_state(short int *ids);
short int *load_ids_from_file(short int *ids);

void process_req(int new_fd, short int *ids);
void add_movie(int sock_fd, struct sockaddr *client_addr, socklen_t client_addr_len, short int *ids);
void delete_movie(int sock_fd, struct sockaddr *client_addr, socklen_t client_addr_len, short int *ids);
void list_titles_and_rooms(int sock_fd, struct sockaddr *client_addr, socklen_t client_addr_len, short int *ids);
void list_titles_for_genre(int sock_fd, struct sockaddr *client_addr, socklen_t client_addr_len, short int *ids);
void get_movie_title(int sock_fd, struct sockaddr *client_addr, socklen_t client_addr_len, short int *ids);
void get_movie_info(int sock_fd, struct sockaddr *client_addr, socklen_t client_addr_len, short int *ids);
void list_all_info(int sock_fd, struct sockaddr *client_addr, socklen_t client_addr_len, short int *ids);
void send_usage_msg(int sock_fd, struct sockaddr *addr, socklen_t addr_len);
int next_free_id(short int *ids);

void save_ids_to_file(short int *ids);
int write_file(char *filename, char *s);
int read_file(char *filename, char *buffer);

int path_exists(char *path);
// Get sockaddr, IPv4 or IPv6.
void *get_in_addr(struct sockaddr *sa);