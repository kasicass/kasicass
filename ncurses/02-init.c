// http://www.tldp.org/HOWTO/NCURSES-Programming-HOWTO/init.html
#include <curses.h>

//! mode: exe
//! int: obj
//! flag: -Wall
//! link: ncurses
//! src: 02-init.c
int main(void)
{
	int ch;

	initscr();                     // Start curses mode
	raw();                         // Line buffering disabled
	keypad(stdscr, TRUE);          // We get F1, F2 etc..
	noecho();                      // Don't echo() while we do getch

	printw("Type any character to see it in bold\n");
	ch = getch();                  // If raw() hadn't been called
                                   // we have to press enter before it
                                   // gets to the program

	if (ch == KEY_F(1))            // Without keypad enabled this will
	{                              //   not get to us either
		printw("F1 Key pressed");  // Without noecho() some ugly escape
	}                              // characters might have been printed
	else                           // on screen
	{
		printw("The press key is ");
		attron(A_BOLD);
		printw("%c", ch);
		attroff(A_BOLD);
	}

	refresh();                // Print it on the real screen
	getch();                  // Wait for user input
	endwin();                 // End curses mode

	return 0;
}

