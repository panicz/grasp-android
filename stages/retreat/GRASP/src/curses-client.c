/* gcc grup.c -o grup -lcurses */

#include <ncurses.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#define SEND(fd, data) ({                                       \
            int sent = send(fd, data"\n", sizeof(data), 0);     \
            if (sent < 0) {                                     \
                perror("send "data);                            \
            }                                                   \
            (sent > 0);                                         \
        })

#define RECEIVE(fd, buffer) ({                                  \
            int recvd = recv(fd, buffer, sizeof(buffer), 0);    \
            if (recvd < 0) {                                    \
                perror("recv");                                 \
            }                                                   \
            else {                                              \
                buffer[recvd] = '\0';                           \
            }                                                   \
            (recvd > 0) && buffer[0] != '#';                    \
        })

void finish() {
    nocbreak();
    endwin();
}

/*
 * No dobra, architektura nasza powinna byc jaka?
 *
 * - otwieramy sobie (mile, cieple) gniazdko
 *   serwera napisanego w Kawa
 * - mamy takie komendy:
 *   cursor-next
 *   cursor-back
 *   cursor-position
 *   screen-state
 */

int main(int argc, char *argv[]) {

    atexit(finish);
    
    initscr();
    cbreak();
    keypad(stdscr, true);
    noecho();

    int kawa = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    struct sockaddr_in address = {
        .sin_addr = {
            .s_addr = inet_addr("127.0.0.1")
        },
        .sin_family = AF_INET,
        .sin_port = htons(5432)
    };

    if (connect(kawa, (struct sockaddr *) &address,
                sizeof(address)) < 0) {
        perror("connect");
    }

    
    int c;
    int x, y;
    int w, h;
    while (c = getch()) {
        getyx(stdscr, y, x);
        getmaxyx(stdscr, h, w);
        
        printw("%d x %d", w, h);
        
        switch (c) {
        case KEY_LEFT: {
            SEND(kawa, "cursor-back");
            x = x-1;
            if (x < 0) {
                x = w-1;
            }
            move(y, x);
            break;
        }
        case KEY_UP: {
            SEND(kawa, "cursor-up");
            y = y-1;
            if (y < 0) {
                y = h-1;
            }
            move(y, x);
            break;
        }
        case KEY_RIGHT: {
            SEND(kawa, "cursor-next");
            x = x+1;
            if (x >= w) {
                x = 0;
            }
            move(y, x);
            break;
        }
        case KEY_DOWN: {
            SEND(kawa, "cursor-down");
            y = y+1;
            if (y >= h) {
                y = 0;
            }
            move(y, x);
            break;
        }
        default:
            if (isprint(c)) {
                printw("%c", c);
            }
            break;
        }
        
        refresh();
    }
    
    return 0;
}
