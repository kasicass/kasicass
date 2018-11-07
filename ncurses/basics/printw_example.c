// http://www.tldp.org/HOWTO/NCURSES-Programming-HOWTO/printw.html
#include <curses.h>
#include <string.h>

//! mode: exe
//! int: obj
//! flag: -Wall
//! link: ncurses
//! src: printw_example.c
int main(void)
{
	char mesg[] = "Just a string";
	int row, col;

	initscr();
	getmaxyx(stdscr, row, col);
	mvprintw(row/2, (col-strlen(mesg))/2, "%s", mesg);
	mvprintw(row-2,0,"This screen has %d rows and %d columes\n", row, col);

	printw("Try resizeing your window(if possible) and then run this program again");
	refresh();
	getch();
	endwin();

	return 0;
}

