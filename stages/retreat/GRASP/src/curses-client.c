/* gcc grup.c -o grup -lcurses */

#include <ncurses.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#define SEND(fd, data) ({                                       \
            int _sent = send(fd, data"\n", sizeof(data), 0);     \
            if (_sent < 0) {                                     \
                perror("send "data);                            \
            }                                                   \
            (_sent > 0);                                         \
        })

#define SENDF(fd, fmt, ...) ({					\
      char _buffer[4*sizeof(fmt)+4];				\
      snprintf(_buffer, sizeof(_buffer), fmt"\n",		\
	       __VA_ARGS__);					\
      int _sent = send(fd, _buffer, strlen(_buffer), 0);	\
      if (_sent < 0) {						\
	perror("send "fmt);					\
      }								\
      (_sent > 0);						\
    })

#define RECEIVE(fd, buffer) ({                                  \
            int recvd = recv(fd, buffer, sizeof(buffer), 0);    \
            if (recvd < 0) {                                    \
                perror("recv");                                 \
            }                                                   \
            else {                                              \
                buffer[recvd] = '\0';                           \
            }                                                   \
            (recvd > 0);					\
        })

void finish() {
  noraw();
  //nocbreak();
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


#define KEY_CTRL_LEFT 0x222
#define KEY_CTRL_RIGHT 0x231
#define KEY_CTRL_UP 0x237
#define KEY_CTRL_DOWN 0x20e

#define KEY_RETURN 0xa
#define KEY_ESCAPE 0x1b

#define KEY_SHIFT_LEFT 0x189
#define KEY_SHIFT_RIGHT 0x192
#define KEY_SHIFT_UP 0x151
#define KEY_SHIFT_DOWN 0x150

#define KEY_ALT_LEFT 0x220
#define KEY_ALT_RIGHT 0x22f
#define KEY_ALT_UP 0x235
#define KEY_ALT_DOWN 0x20c

#define KEY_CTRL_SHIFT_LEFT 0x223
#define KEY_CTRL_SHIFT_RIGHT 0x232
#define KEY_CTRL_SHIFT_UP 0x238
#define KEY_CTRL_SHIFT_DOWN 0x20f

#define KEY_ALT_SHIFT_LEFT 0x221
#define KEY_ALT_SHIFT_RIGHT 0x230
#define KEY_ALT_SHIFT_UP 0x236
#define KEY_ALT_SHIFT_DOWN 0x20d

#define KEY_CTRL_Z 0x1a
#define KEY_CTRL_X 0x18
#define KEY_CTRL_C 0x03
#define KEY_CTRL_V 0x16
#define KEY_CTRL_A 0x01
#define KEY_CTRL_S 0x13
#define KEY_CTRL_Y 0x19
#define KEY_CTRL_Q 0x11


int main(int argc, char *argv[]) {

    atexit(finish);
    
    initscr();
    //cbreak();
    raw();
    keypad(stdscr, true);
    noecho();
    mousemask(ALL_MOUSE_EVENTS, NULL);

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
    getyx(stdscr, y, x);
    
    while ((c = getch())) {
        getmaxyx(stdscr, h, w);

	char result[w*h];

	move(0, 0);
	clrtoeol();
	mvprintw(0, 0, "%d/0%o/x%x (%c)", c, c, c, isprint(c) ? c : '?');
	
        switch (c) {
        case KEY_LEFT: {
            SEND(kawa, "cursor-back");
	    if (RECEIVE(kawa, result)) {
	      sscanf(result, "(%d %d)", &x, &y);
	    }
            break;
        }
        case KEY_UP: {
            SEND(kawa, "cursor-up");
	    if (RECEIVE(kawa, result)) {
	      sscanf(result, "(%d %d)", &x, &y);
	    }
	    break;
        }
        case KEY_RIGHT: {
            SEND(kawa, "cursor-next");
	    if (RECEIVE(kawa, result)) {
	      sscanf(result, "(%d %d)", &x, &y);
	    }

            break;
        }
        case KEY_DOWN: {
            SEND(kawa, "cursor-down");
	    if (RECEIVE(kawa, result)) {
	      sscanf(result, "(%d %d)", &x, &y);
	    }
            break;
        }

	case KEY_SHIFT_LEFT: {
            SEND(kawa, "select-left");
	    if (RECEIVE(kawa, result)) {
	      sscanf(result, "(%d %d)", &x, &y);
	    }
	  break;
	}

	case KEY_SHIFT_RIGHT: {
            SEND(kawa, "select-right");
	    if (RECEIVE(kawa, result)) {
	      sscanf(result, "(%d %d)", &x, &y);
	    }
	  break;
	}

	case KEY_SHIFT_UP: {
            SEND(kawa, "select-up");
	    if (RECEIVE(kawa, result)) {
	      sscanf(result, "(%d %d)", &x, &y);
	    }
	  break;
	}

	case KEY_SHIFT_DOWN: {
            SEND(kawa, "select-down");
	    if (RECEIVE(kawa, result)) {
	      sscanf(result, "(%d %d)", &x, &y);
	    }
	  break;
	}
	  
	case KEY_MOUSE: {

	  break;
	}
	case '(':
	case '{':
	case '[': {
	  SEND(kawa, "create-box");
	  break;
	}

	case ']':
	case '}':
	case ')': {
	  SEND(kawa, "cursor-leave");
	  break;
	}

	case '\n':
	  SEND(kawa, "insert-character #\\newline");
	  if (RECEIVE(kawa, result)) {
	    sscanf(result, "(%d %d)", &x, &y);
	  }
	  break;

	case ' ':
	  SEND(kawa, "insert-character #\\space");
	  if (RECEIVE(kawa, result)) {
	    sscanf(result, "(%d %d)", &x, &y);
	  }
	  break;

	case KEY_BACKSPACE: {
	  SEND(kawa, "delete-back");
	  if (RECEIVE(kawa, result)) {
	    sscanf(result, "(%d %d)", &x, &y);
	  }
	  break;
	}

	case KEY_DL: {
	  SEND(kawa, "delete-next");
	  if (RECEIVE(kawa, result)) {
	    sscanf(result, "(%d %d)", &x, &y);
	  }	  
	  break;
	}
	  
	case '\4': {
	  exit(0);
	}
	  
        default:
	  if (isgraph(c)) {
	    SENDF(kawa, "insert-character #\\%c", c);
	  }
	  break;
        }

	SEND(kawa, "screen-state");
	if (RECEIVE(kawa, result)) {
	  mvprintw(0, 0, "%s", result);
	}

	move(y+1, x);
        refresh();
    }
    
    return 0;
}
