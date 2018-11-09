// http://www.tldp.org/HOWTO/NCURSES-Programming-HOWTO/windows.html
#include <ncurses.h>

WINDOW* create_newwin(int height, int width, int starty, int startx);
void destroy_win(WINDOW *local_win);

//! mode: exe
//! int: obj
//! flag: -Wall
//! link: ncurses
//! src: win_border.c
int main(int argc, char* argv[])
{
	WINDOW *my_win;
	int startx, starty, width, height;
	int ch;

	initscr();
	cbreak();              // Line buffering diabled, pass on everthing to me
	keypad(stdscr, TRUE);  // I need that notify F1

	height = 3;
	width = 10;
	starty = (LINES - height) / 2; // calculating for center polacement
	startx = (COLS - width) / 2;   // of the window
	printw("Press F1 to exit");
	refresh();
	my_win = create_newwin(height, width, starty, startx);

	while ((ch = getch()) != KEY_F(1))
	{
		switch (ch)
		{
		case KEY_LEFT:
			destroy_win(my_win);
			my_win = create_newwin(height, width, starty, --startx);
			break;

		case KEY_RIGHT:
			destroy_win(my_win);
			my_win = create_newwin(height, width, starty, ++startx);
			break;

		case KEY_UP:
			destroy_win(my_win);
			my_win = create_newwin(height, width, --starty, startx);
			break;

		case KEY_DOWN:
			destroy_win(my_win);
			my_win = create_newwin(height, width, ++starty, startx);
			break;
		}
	}

	endwin();	
	return 0;	
}

WINDOW* create_newwin(int height, int width, int starty, int startx)
{
	WINDOW* local_win;

	local_win = newwin(height, width, starty, startx);
	box(local_win, 0, 0);
	wrefresh(local_win);
	return local_win;
}

void destroy_win(WINDOW* local_win)
{
	// This won't produce the desired result of erasing
	// the window. It will leave it's four corners and so
	// an ugly remnant of window.
	// box(local_win, ' ', ' ');

	// 1. win: the window on which to operate
	// 2. ls: character to be used for the left side of the window 
	// 3. rs: character to be used for the right side of the window 
	// 4. ts: character to be used for the top side of the window 
	// 5. bs: character to be used for the bottom side of the window 
	// 6. tl: character to be used for the top left corner of the window 
	// 7. tr: character to be used for the top right corner of the window 
	// 8. bl: character to be used for the bottom left corner of the window 
	// 9. br: character to be used for the bottom right corner of the window
	wborder(local_win, ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ');
	
	wrefresh(local_win);
	delwin(local_win);	
}

