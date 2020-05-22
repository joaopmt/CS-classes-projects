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

#define PORT "3490" // the port client will be connecting to 

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
	hints.ai_family = AF_UNSPEC;
	hints.ai_socktype = SOCK_STREAM;

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
		if (connect(sockfd, p->ai_addr, p->ai_addrlen) == -1) {
			perror("client: connect");
			close(sockfd);
			continue;
		}
		break;
	}

	if (p == NULL) {
		fprintf(stderr, "client: failed to connect\n");
		return 2;
	}

	inet_ntop(p->ai_family, get_in_addr((struct sockaddr *)p->ai_addr),
			s, sizeof s);
	printf("Client: connecting to %s.\n", s);

	freeaddrinfo(servinfo); // all done with this structure

    while (1) {
        rcv_msg(sockfd, buf);
	    printf("%s", buf);
        scanf("%[^\n]", buf);
        getchar();
        printf("Enviando: '%s'\n", buf);
        send_msg(sockfd, buf);
        if (!strcmp(buf, "end")) {
            break;
        }
    }
	

	close(sockfd);

	return 0;
}


int send_msg(int new_fd, char *msg) {
    char msg_padded[MAX_MSG_SIZE];
    int sent = 0; // How many bytes were sent.
    int left = MAX_MSG_SIZE; // How many are left to send.
    int n;

    int len = strlen(msg);
    if (len > MAX_MSG_SIZE) {
        perror("Warning: sending data with more than MAX_MSG_SIZE bytes.");
    }
    int i;
    for (i = 0; i < len; i++) {
        msg_padded[i] = msg[i];
    }
    msg_padded[i] = '\0';
    while(left > 0) {
        n = send(new_fd, msg_padded + sent, left, 0);
        if (n == -1) { break; }
        sent += n;
        left -= n;
    }
    // Return -1 on failure, 0 on success.
    return n == -1 ? -1 : 0;
}

int rcv_msg(int new_fd, char *msg) {
    char buffer[MAX_MSG_SIZE];
    int count = 0;
    int total = 0;
    int i;

    while ((count = recv(new_fd, &buffer[total], sizeof buffer - count, 0)) > 0) {
        for (i = total; i < total + count; i++) {
            msg[i] = buffer[i - total];
        }
        total += count;
        if (total >= MAX_MSG_SIZE) break;
    }
    if (count == -1) {
        perror("recv");
    }
    msg[total] = '\0';
}