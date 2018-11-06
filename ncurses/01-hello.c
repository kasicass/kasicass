// http://www.tldp.org/HOWTO/NCURSES-Programming-HOWTO/helloworld.html
#include <curses.h>

//! mode: exe
//! link: ncurses
//! src: hello.c
int main(void)
{
	initscr();                // Start curses mode
	printw("Hello World!");   // Print Hello World
	refresh();                // Print it on the real screen
	getch();                  // Wait for user input
    endwin();                 // End curses mode

	return 0;
}

