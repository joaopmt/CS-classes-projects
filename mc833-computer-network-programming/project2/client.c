#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <netdb.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <arpa/inet.h>

#include "utils.h"

// get sockaddr, IPv4 or IPv6:
void *get_in_addr(struct sockaddr *sa) {
	if (sa->sa_family == AF_INET) {
		return &(((struct sockaddr_in*)sa)->sin_addr);
	}

	return &(((struct sockaddr_in6*)sa)->sin6_addr);
}

int main(int argc, char *argv[]){
	int sockfd, numbytes;  
	char buf[MAX_MSG_SIZE];
	struct addrinfo hints, *servinfo, *p;
	int rv;
	char s[INET6_ADDRSTRLEN];

	if (argc != 2) {
	    fprintf(stderr,"usage: client hostname\n");
	    exit(1);
	}

	memset(&hints, 0, sizeof hints);
	hints.ai_family = AF_INET;
	hints.ai_socktype = SOCK_DGRAM;

	if ((rv = getaddrinfo(argv[1], PORT, &hints, &servinfo)) != 0) {
		fprintf(stderr, "getaddrinfo: %s\n", gai_strerror(rv));
		return 1;
	}

	// loop through all the results and connect to the first we can
	for(p = servinfo; p != NULL; p = p->ai_next) {
		if ((sockfd = socket(p->ai_family, p->ai_socktype,
				p->ai_protocol)) == -1) {
			perror("client: socket");
			continue;
		}
		break;
	}

	if (p == NULL) {
		fprintf(stderr, "client: failed to create socket\n");
		return 2;
	}

	freeaddrinfo(servinfo); // all done with this structure

    struct sockaddr server_addr;
    socklen_t server_addr_len = sizeof server_addr;
    send_msg(sockfd, INITIAL_MSG, p->ai_addr, p->ai_addrlen);
    while (1) {
        rcv_msg(sockfd, buf, &server_addr, &server_addr_len);
	    printf("%s", buf);
        scanf("%[^\n]", buf);
        getchar();
        send_msg(sockfd, buf, &server_addr, server_addr_len);
        if (!strcmp(buf, "end")) {
            break;
        }
    }

	close(sockfd);

	return 0;
}

// Send message to given addr using datagrams.
int send_msg(int sock_fd, char *msg, struct sockaddr *addr, socklen_t addr_len) {
    sendto(sock_fd, msg, strlen(msg), 0, addr, addr_len);
    
    // Return -1 on failure, 0 on success.
    return 0;
}

// Receive a message using datagrams.
int rcv_msg(int sock_fd, char *msg, struct sockaddr *addr, socklen_t *addr_len) {
    int numbytes = recvfrom(sock_fd, msg, MAX_MSG_SIZE, 0, addr, addr_len);
    msg[numbytes] = '\0';

    return 0;
}