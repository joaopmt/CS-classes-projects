#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <sys/wait.h>
#include <signal.h>
#include <sys/stat.h>
#include <poll.h>

#include "server.h"
#include "utils.h"

#define MAX_MOVIE_COUNT 20 // Max number of movies that can be stored at once.

#define POLL_TIMEOUT_MS 60000 // Timeout used for non-blocking message receivements.

int main(void) {
	int sock_fd;  // All socket communication will be done by this descriptor.
	struct addrinfo hints, *servinfo, *p;
	int rv;

	memset(&hints, 0, sizeof hints);
	hints.ai_family = AF_INET;
	hints.ai_socktype = SOCK_DGRAM;
	hints.ai_flags = AI_PASSIVE; // Use my IP.

	if ((rv = getaddrinfo(NULL, PORT, &hints, &servinfo)) != 0) {
		fprintf(stderr, "getaddrinfo: %s\n", gai_strerror(rv));
		return 1;
	}

	// loop through all the results and bind to the first we can
	for(p = servinfo; p != NULL; p = p->ai_next) {
		if ((sock_fd = socket(p->ai_family, p->ai_socktype,
				p->ai_protocol)) == -1) {
			perror("server: socket");
			continue;
		}
		if (bind(sock_fd, p->ai_addr, p->ai_addrlen) == -1) {
			close(sock_fd);
			perror("server: bind");
			continue;
		}
		break;
	}
	freeaddrinfo(servinfo); // all done with this structure

	if (p == NULL)  {
		fprintf(stderr, "server: failed to bind\n");
		exit(1);
	}

    // Array of available IDs where ids[i] is 1 if id i is occupied and 0 if id i is free.
    short int *ids = calloc(MAX_MOVIE_COUNT, sizeof(short int));

    initialize_server_state(ids);

    printf("Server: waiting for datagrams...\n");

    // Communication loop.
    // Clients can't stop the Server. It can be killed by sending SIGINT signal to process (Ctrl + C in terminal).
	while (1) {
        load_ids_from_file(ids);

        process_req(sock_fd, ids);

        save_ids_to_file(ids);
	}
	return 0;
}

// Get server ready to perform the operations.
void initialize_server_state(short int *ids) {
    if (path_exists("ids")) {
        // Load previous state.
        load_ids_from_file(ids);
    }
    // Create directories that will hold movies info if they don't already exist.
    if (!path_exists("titles")) {
        mkdir("titles", 0700);
    }
    if (!path_exists("synopses")) {
        mkdir("synopses", 0700);
    }
    if (!path_exists("genres")) {
        mkdir("genres", 0700);
    }
    if (!path_exists("rooms")) {
        mkdir("rooms", 0700);
    }
}

// Load previous state of the ids array, stored in file.
short int *load_ids_from_file(short int *ids) {
    int id, index = 0;
    char ids_str[MAX_MOVIE_COUNT + 1];

    read_file("ids", ids_str);
    for (id = 0; id < MAX_MOVIE_COUNT; id++) {
        ids[id] = ids_str[id] == '0' ? 0 : 1;
    }
}

// Save ids to file to preserve state.
void save_ids_to_file(short int *ids) {
    int i, index = 0;
    char ids_str[MAX_MOVIE_COUNT];

    for (i = 0; i < MAX_MOVIE_COUNT; i++)
        index += sprintf(&ids_str[index], "%d", ids[i]);

    write_file("ids", ids_str);
}

// Receives client's operation request and call the appropriate function to handle it. 
void process_req(int sock_fd, short int *ids) {
    struct sockaddr client_addr;
    socklen_t client_addr_len = sizeof client_addr;
    char *msg;
	
    msg = malloc(MAX_MSG_SIZE * sizeof(char));
    if (!msg) {
        perror("malloc");
        exit(0);
    }

    // This is blocking since Server has nothing better to do than to wait for a datagram...
    rcv_msg(sock_fd, msg, &client_addr, &client_addr_len);

    if (!strcmp(msg, "add")) {
        add_movie(sock_fd, &client_addr, client_addr_len, ids); 
    } else if (!strcmp(msg, "delete")) {
        delete_movie(sock_fd, &client_addr, client_addr_len, ids); 
    } else if (!strcmp(msg, "list titles and rooms")) {
        list_titles_and_rooms(sock_fd, &client_addr, client_addr_len, ids); 
    } else if (!strcmp(msg, "list titles for genre")) {
        list_titles_for_genre(sock_fd, &client_addr, client_addr_len, ids); 
    } else if (!strcmp(msg, "get movie title")) {
        get_movie_title(sock_fd, &client_addr, client_addr_len, ids); 
    } else if (!strcmp(msg, "get movie info")) {
        get_movie_info(sock_fd, &client_addr, client_addr_len, ids); 
    } else if (!strcmp(msg, "list all info")) {
        list_all_info(sock_fd, &client_addr, client_addr_len, ids); 
    } else if (!strcmp(msg, INITIAL_MSG)) {
        send_usage_msg(sock_fd, &client_addr, client_addr_len); 
    } else {
        send_msg(sock_fd, "\n--x Unrecognized operation.\n\nEnter next operation:\n", &client_addr, client_addr_len); 
    }

    free(msg);
}

// Receive movie info from client and add movie with that info to the catalog.
void add_movie(int sock_fd, struct sockaddr *client_addr, socklen_t client_addr_len, short int *ids) {
    char *msg;
    char *title;
    char filename[20];

    msg = malloc(MAX_MSG_SIZE * sizeof(char));
    title = malloc(MAX_FIELD_SIZE * sizeof(char));
    if (!msg || !title) {
        perror("malloc");
        exit(0);
    }

    int id = next_free_id(ids);
    if (id == -1) {
        sprintf(msg, "\n--x Sorry, the movie catalog is full. (Max number of movies supported: %d).\n\nEnter next operation:\n", MAX_MOVIE_COUNT);
        send_msg(sock_fd, msg, client_addr, client_addr_len);
        return;
    }
    
    // Ask client for movie info.
    send_msg(sock_fd, "Title: ", client_addr, client_addr_len);
    if (rcv_msg_polling(sock_fd, msg, client_addr, &client_addr_len) == -1) {
        send_msg(sock_fd, "\n--x Server poll timed out! Operation has been canceled. \n\nEnter next operation:\n", client_addr, client_addr_len);
        free(msg);
        free(title);
        return;
    }
    // Store title to use later.
    strcpy(title, msg);
    // Create title_<id> file to store movie <id>'s title.
    sprintf(filename, "titles/title_%d", id);
    write_file(filename, msg);

    send_msg(sock_fd, "Synopsis: ", client_addr, client_addr_len);
    if (rcv_msg_polling(sock_fd, msg, client_addr, &client_addr_len) == -1) {
        send_msg(sock_fd, "\n--x Server poll timed out! Operation has been canceled. \n\nEnter next operation:\n", client_addr, client_addr_len);
        free(msg);
        free(title);
        return;
    }
    // Create synopsis_<id> file to store movie <id>'s synopsis.
    sprintf(filename, "synopses/synopsis_%d", id);
    write_file(filename, msg);

    send_msg(sock_fd, "Genre: ", client_addr, client_addr_len);
    if (rcv_msg_polling(sock_fd, msg, client_addr, &client_addr_len) == -1) {
        send_msg(sock_fd, "\n--x Server poll timed out! Operation has been canceled. \n\nEnter next operation:\n", client_addr, client_addr_len);
        free(msg);
        free(title);
        return;
    }
    // Create genre_<id> file to store movie <id>'s genre.
    sprintf(filename, "genres/genre_%d", id);
    write_file(filename, msg);

    send_msg(sock_fd, "Room: ", client_addr, client_addr_len);
    if (rcv_msg_polling(sock_fd, msg, client_addr, &client_addr_len) == -1) {
        send_msg(sock_fd, "\n--x Server poll timed out! Operation has been canceled. \n\nEnter next operation:\n", client_addr, client_addr_len);
        free(msg);
        free(title);
        return;
    }
    // Create room_<id> file to store movie <id>'s room.
    sprintf(filename, "rooms/room_%d", id);
    write_file(filename, msg);

    // Mark id as used.
    ids[id] = 1;

    sprintf(msg, "\n--> Movie '%s' has been added with id = %d.\n\nEnter next operation:\n", title, id);
    send_msg(sock_fd, msg, client_addr, client_addr_len);

    free(msg);
    free(title);
}

// Receive id from client and delete movie with that id.
void delete_movie(int sock_fd, struct sockaddr *client_addr, socklen_t client_addr_len, short int *ids) {
    char *msg;
    char *title;
    char filename[20];

    msg = malloc(MAX_MSG_SIZE * sizeof(char));
    title = malloc(MAX_FIELD_SIZE * sizeof(char));
    if (!msg || !title) {
        perror("malloc");
        exit(0);
    }

    // Ask client for id.
    send_msg(sock_fd, "id: ", client_addr, client_addr_len);
    if (rcv_msg_polling(sock_fd, msg, client_addr, &client_addr_len) == -1) {
        send_msg(sock_fd, "\n--x Server poll timed out! Operation has been canceled. \n\nEnter next operation:\n", client_addr, client_addr_len);
        free(msg);
        free(title);
        return;
    }
    
    int id = atoi(msg);
    if (ids[id] == 0) {
        sprintf(msg, "\n--x Sorry, the movie with id = %d does not exist in the catalog.\n\nEnter next operation:\n", id);
        send_msg(sock_fd, msg, client_addr, client_addr_len);
        return;
    }
    // Delete title file for movie <id>.
    sprintf(filename, "titles/title_%d", id);
    // Store title to use later.
    read_file(filename, title);
    remove(filename);
    // Delete synopsis file for movie <id>.
    sprintf(filename, "synopses/synopsis_%d", id);
    remove(filename);
    // Delete genre file for movie <id>.
    sprintf(filename, "genres/genre_%d", id);
    remove(filename);
    // Delete room file for movie <id>.
    sprintf(filename, "rooms/room_%d", id);
    remove(filename);
    
    // Mark id as free.
    ids[id] = 0;

    sprintf(msg, "\n--> Movie with id = %d ('%s') has been deleted.\n\nEnter next operation:\n", id, title);
    send_msg(sock_fd, msg, client_addr, client_addr_len);

    free(msg);
    free(title);
}

// Send msg with list of titles and rooms to client.
void list_titles_and_rooms(int sock_fd, struct sockaddr *client_addr, socklen_t client_addr_len, short int *ids) {
    char *msg;
    char *title;
    char *room;
    char *title_and_room;
    char filename[20];
    int no_movie;
    int id;

    msg = malloc(MAX_MSG_SIZE * sizeof(char));
    title = malloc(MAX_FIELD_SIZE * sizeof(char));
    room = malloc(MAX_FIELD_SIZE * sizeof(char));
    title_and_room = malloc(MAX_FIELD_SIZE * sizeof(char));
    if (!msg || !title || !room || !title_and_room) {
        perror("malloc");
        exit(0);
    }

    // Get all titles and rooms in the catalog.
    msg[0] = '\0';
    no_movie = 1;
    for (id = 0; id < MAX_MOVIE_COUNT; id++) {
        if (ids[id]) {
            sprintf(filename, "titles/title_%d", id);
            read_file(filename, title);
            sprintf(filename, "rooms/room_%d", id);
            read_file(filename, room);
            sprintf(title_and_room, "--> '%s', room %s.\n", title, room);
            strcat(msg, title_and_room);
            no_movie = 0;
        }
    }
    if (no_movie) {
        send_msg(sock_fd, "\n--x The catalog is empty.\n\nEnter next operation:\n", client_addr, client_addr_len);
        return;
    } 
    strcat(msg, "\nEnter next operation:\n");
    send_msg(sock_fd, msg, client_addr, client_addr_len);

    free(msg);
    free(title);
    free(room);
    free(title_and_room);
}

// Ask client for genre and send msg with list of titles for that genre to client.
void list_titles_for_genre(int sock_fd, struct sockaddr *client_addr, socklen_t client_addr_len, short int *ids) {
    char *msg;
    char *req_genre;
    char *genre;
    char *title;
    char *title_pretty;
    char filename[20];
    int no_movie;
    int id;

    msg = malloc(MAX_MSG_SIZE * sizeof(char));
    req_genre = malloc(MAX_FIELD_SIZE * sizeof(char));
    genre = malloc(MAX_FIELD_SIZE * sizeof(char));
    title = malloc(MAX_FIELD_SIZE * sizeof(char));
    title_pretty = malloc(MAX_FIELD_SIZE * sizeof(char));
    if (!msg || !title || !req_genre || !genre || !title_pretty) {
        perror("malloc");
        exit(0);
    }

    // Ask client for genre.
    send_msg(sock_fd, "Genre: ", client_addr, client_addr_len);
    if (rcv_msg_polling(sock_fd, req_genre, client_addr, &client_addr_len) == -1) {
        send_msg(sock_fd, "\n--x Server poll timed out! Operation has been canceled. \n\nEnter next operation:\n", client_addr, client_addr_len);
        free(msg);
        free(title);
        free(req_genre);
        free(genre);
        free(title_pretty);
        return;
    }

    // Read all genres in the catalog and match them with the given genre.
    // When genre with id = <id> gets matched, append title with id = <id> to response msg.
    msg[0] = '\0';
    no_movie = 1;
    for (id = 0; id < MAX_MOVIE_COUNT; id++) {
        if (ids[id]) {
            sprintf(filename, "genres/genre_%d", id);
            read_file(filename, genre);
            if (!strcmp(genre, req_genre)) {
                // Movie with id = <id> has the requested genre.
                sprintf(filename, "titles/title_%d", id);
                read_file(filename, title);
                sprintf(title_pretty, "\n--> '%s'", title);
                strcat(msg, title_pretty);
                no_movie = 0;
            }
        }
    }
    if (no_movie) {
        sprintf(msg, "\n--x The catalog has no movie with the '%s' genre.\n\nEnter next operation:\n", req_genre);
        send_msg(sock_fd, msg, client_addr, client_addr_len);
        return;
    }
    strcat(msg, "\n\nEnter next operation:\n");
    send_msg(sock_fd, msg, client_addr, client_addr_len);

    free(msg);
    free(title);
    free(req_genre);
    free(genre);
    free(title_pretty);
}

// Ask client for id and send msg with movie title for that id.
void get_movie_title(int sock_fd, struct sockaddr *client_addr, socklen_t client_addr_len, short int *ids) {
    char *msg;
    char *title;
    char filename[20];

    msg = malloc(MAX_MSG_SIZE * sizeof(char));
    title = malloc(MAX_FIELD_SIZE * sizeof(char));
    if (!msg || !title) {
        perror("malloc");
        exit(0);
    }

    // Ask client for id.
    send_msg(sock_fd, "id: ", client_addr, client_addr_len);
    if (rcv_msg_polling(sock_fd, msg, client_addr, &client_addr_len) == -1) {
        send_msg(sock_fd, "\n--x Server poll timed out! Operation has been canceled. \n\nEnter next operation:\n", client_addr, client_addr_len);
        free(msg);
        free(title);
        return;
    }

    int id = atoi(msg);
    if (ids[id] == 0) {
        sprintf(msg, "\n--x Sorry, the movie with id = %d does not exist in the catalog.\n\nEnter next operation:\n", id);
        send_msg(sock_fd, msg, client_addr, client_addr_len);
        return;
    }
    // Get title for requested id.
    sprintf(filename, "titles/title_%d", id);
    read_file(filename, title);
    sprintf(msg, "\n--> '%s'.\n\nEnter next operation:\n", title);
    send_msg(sock_fd, msg, client_addr, client_addr_len);    

    free(msg);
    free(title);
}

// Ask client for id and send msg with all movie info for that id.
void get_movie_info(int sock_fd, struct sockaddr *client_addr, socklen_t client_addr_len, short int *ids) {
    char *msg;
    char *info;
    char *info_pretty;
    char filename[20];

    msg = malloc(MAX_MSG_SIZE * sizeof(char));
    info = malloc(MAX_FIELD_SIZE * sizeof(char));
    info_pretty = malloc(MAX_FIELD_SIZE * sizeof(char));
    if (!msg || !info || !info_pretty) {
        perror("malloc");
        exit(0);
    }

    // Aks client for id.
    send_msg(sock_fd, "id: ", client_addr, client_addr_len);
    if (rcv_msg_polling(sock_fd, msg, client_addr, &client_addr_len) == -1) {
        send_msg(sock_fd, "\n--x Server poll timed out! Operation has been canceled. \n\nEnter next operation:\n", client_addr, client_addr_len);
        free(msg);
        free(info);
        free(info_pretty);
        return;
    }

    int id = atoi(msg);
    if (ids[id] == 0) {
        sprintf(msg, "\n--x Sorry, the movie with id = %d does not exist in the catalog.\n\nEnter next operation:\n", id);
        send_msg(sock_fd, msg, client_addr, client_addr_len);
        return;
    }
    // Get all info for requested id.
    msg[0] = '\0';
    sprintf(filename, "titles/title_%d", id);
    read_file(filename, info);
    sprintf(info_pretty, "\n--> Title: '%s'", info);
    strcat(msg, info_pretty);

    sprintf(filename, "synopses/synopsis_%d", id);
    read_file(filename, info);
    sprintf(info_pretty, "\n--> Synopsis: '%s'", info);
    strcat(msg, info_pretty);

    sprintf(filename, "genres/genre_%d", id);
    read_file(filename, info);
    sprintf(info_pretty, "\n--> Genre: '%s'", info);
    strcat(msg, info_pretty);

    sprintf(filename, "rooms/room_%d", id);
    read_file(filename, info);
    sprintf(info_pretty, "\n--> Room: '%s'", info);
    strcat(msg, info_pretty);

    strcat(msg, "\n\nEnter next operation:\n");
    send_msg(sock_fd, msg, client_addr, client_addr_len); 

    free(msg);
    free(info);
    free(info_pretty);
}

// Send msg with all info for all movies to client.
void list_all_info(int sock_fd, struct sockaddr *client_addr, socklen_t client_addr_len, short int *ids) {
    char *msg;
    char *info;
    char *info_pretty;
    char filename[20];
    int no_movie;
    int id;

    msg = malloc(MAX_MSG_SIZE * sizeof(char));
    info = malloc(MAX_FIELD_SIZE * sizeof(char));
    info_pretty = malloc(MAX_FIELD_SIZE * sizeof(char));
    if (!msg || !info || !info_pretty) {
        perror("malloc");
        exit(0);
    }

    // Get all info for all movies.
    msg[0] = '\0';
    no_movie = 1;
    for (id = 0; id < MAX_MOVIE_COUNT; id++) {
        if (ids[id]) {
            // Append all info for movie with id = <id>.
            sprintf(info_pretty, "\n--> id: '%d'", id);
            strcat(msg, info_pretty);

            sprintf(filename, "titles/title_%d", id);
            read_file(filename, info);
            sprintf(info_pretty, "\n--> Title: '%s'", info);
            strcat(msg, info_pretty);

            sprintf(filename, "synopses/synopsis_%d", id);
            read_file(filename, info);
            sprintf(info_pretty, "\n--> Synopsis: '%s'", info);
            strcat(msg, info_pretty);

            sprintf(filename, "genres/genre_%d", id);
            read_file(filename, info);
            sprintf(info_pretty, "\n--> Genre: '%s'", info);
            strcat(msg, info_pretty);

            sprintf(filename, "rooms/room_%d", id);
            read_file(filename, info);
            sprintf(info_pretty, "\n--> Room: '%s'\n", info);
            strcat(msg, info_pretty);

            no_movie = 0;
        }
    }
    if (no_movie) {
        sprintf(msg, "\n--x There are no movies in the catalog!\n\nEnter next operation:\n", id);
        send_msg(sock_fd, msg, client_addr, client_addr_len);
        return;
    }
    strcat(msg, "\n\nEnter next operation:\n");
    send_msg(sock_fd, msg, client_addr, client_addr_len);

    free(msg);
    free(info);
    free(info_pretty);
}

// Send msg with all available operations.
void send_usage_msg(int sock_fd, struct sockaddr *addr, socklen_t addr_len) {
    char *msg = "\nEnter one of the following operations:\n"
                "add\n"
                "delete\n"
                "list titles and rooms\n"
                "list titles for genre\n"
                "get movie title\n"
                "get movie info\n"
                "list all info\n"
                "end\n\n";
    send_msg(sock_fd, msg, addr, addr_len);
}

// Returns next free id, starting at 0.
int next_free_id(short int *ids) {
    int id;
    for (id = 0; id < MAX_MOVIE_COUNT; id++) {
        if (ids[id] == 0) return id;
    }
    // No free ids.
    return -1;
}

// Padds the message so that it has size MAX_MSG_SIZE, which is knowm by client, and sends it.
int send_msg(int sock_fd, char *msg, struct sockaddr *addr, socklen_t addr_len) {
    sendto(sock_fd, msg, strlen(msg), 0, addr, addr_len);
    
    // Return -1 on failure, 0 on success.
    return 0;
}

int rcv_msg(int sock_fd, char *msg, struct sockaddr *addr, socklen_t *addr_len) {
    int numbytes = recvfrom(sock_fd, msg, MAX_MSG_SIZE, 0, addr, addr_len);
    msg[numbytes] = '\0';

    return 0;
}

// Receives msg non-blocking by polling with a timeout. Return 0 on success, -1 on timeout.
int rcv_msg_polling(int sock_fd, char *msg, struct sockaddr *addr, socklen_t *addr_len) {
    struct pollfd pfds[1];

    // Add the socket descriptor to be monitored by poll() call.
    pfds[0].fd = sock_fd;
    pfds[0].events = POLLIN;

    poll(pfds, 1, POLL_TIMEOUT_MS);

    if (pfds[0].revents & POLLIN) {
        // Ready to read.
        int numbytes = recvfrom(sock_fd, msg, MAX_MSG_SIZE, 0, addr, addr_len);
        msg[numbytes] = '\0';

        return 0;
    }

    printf("Poll timed out.\n");
    
    // Signalize that timeot occurred.
    return -1;
}

// Writes given string to given filename.
int write_file(char *filename, char *s) {
    FILE *f = fopen(filename, "w");

    if (f) {
        fprintf(f, s);
        fclose(f);
        return 0;
    }
    return -1;
}

// Reads from given filename and store in buffer.
int read_file(char *filename, char *buffer) {
    long length;
    FILE *f = fopen(filename, "r");

    if (f) {
        fseek(f, 0, SEEK_END);
        length = ftell(f);
        fseek(f, 0, SEEK_SET);
        if (buffer) {
            fread(buffer, 1, length, f);
        }
        fclose(f);
        buffer[length] = '\0';
        return 0;
    }
    return -1;
}

// Returns 1 if given path exists, 0 otherwise.
int path_exists(char *path) {
    return access(path, F_OK) == -1 ? 0 : 1;
}

// Get sockaddr, IPv4 or IPv6.
void *get_in_addr(struct sockaddr *sa) {
	if (sa->sa_family == AF_INET) {
		return &(((struct sockaddr_in*)sa)->sin_addr);
	}
	return &(((struct sockaddr_in6*)sa)->sin6_addr);
}