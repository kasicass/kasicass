#include <ncurses.h>
#include <string.h>

//! mode: exe
//! int: obj
//! flag: -Wall
//! link: ncurses
//! src: scanw_example.c
int main()
{
	char mesg[]  = "Enter a string: ";
	char mesg1[] = "Enter a int: ";
	char str[80];
	int row, col;
	int v;

	initscr();

	// getstr()
	getmaxyx(stdscr, row, col);
	mvprintw(row/2, (col-strlen(mesg))/2, "%s", mesg);

	getstr(str);
	mvprintw(LINES-2, 0, "You Entered: %s", str);
	getch();

	// sacnw()
	clear();
	getmaxyx(stdscr, row, col);
	mvprintw(row/2, (col-strlen(mesg))/2, "%s", mesg1);

	scanw("%d", &v);
	mvprintw(LINES-2, 0, "You Entered: %d", v);
	getch();

	endwin();
	return 0;
}

