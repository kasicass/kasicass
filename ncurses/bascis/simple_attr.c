/* http://www.tldp.org/HOWTO/NCURSES-Programming-HOWTO/attrib.html */
#include <ncurses.h>
#include <stdlib.h>

//! mode: exe
//! int: obj
//! flag: -Wall, -Wno-unused-but-set-variable
//! link: ncurses
//! src: simple_attr.c
int main(int argc, char* argv[])
{
	int ch, prev, row, col;
	FILE *fp;
	int y, x;

	if (argc != 2)
	{
		printf("Usage: %s <a c file name>\n", argv[0]);
		exit(1);
	}

	fp = fopen(argv[1], "r");
	if (fp == NULL)
	{
		perror("Can't open input file");
		exit(1);
	}

	prev = EOF;
	initscr();
	getmaxyx(stdscr, row, col);

	while ((ch = fgetc(fp)) != EOF)
	{
		getyx(stdscr, y, x);          // get the current cursor position
		if (y == (row - 1))           // are we at the end of the screen
		{
			printw("<-Press Any Key->");
			getch();
			clear();
			move(0, 0);
		}

		if (prev == '/' && ch == '*') // If it is /* then only switch bold on
		{
			attron(A_BOLD);
			getyx(stdscr, y, x);
			move(y, x - 1);           // back up one space
			printw("%c%c", '/', ch);
		}
		else
		{
			printw("%c", ch);
		}

		refresh();

		if (prev == '*' && ch == '/')
		{
			attroff(A_BOLD);         // bold off
		}

		prev = ch;
	}
	endwin();
	fclose(fp);	
	
	return 0;
}

