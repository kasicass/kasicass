#include "K1RControllerWin32.h"

namespace K1R {

KeyPress KeyPressWin32::GetKey()
{
	switch (m_wParam)
	{
	case 'A': case 'a':		return KEY_A;
	case 'B': case 'b':		return KEY_B;
	case 'C': case 'c':		return KEY_C;
	case 'D': case 'd':		return KEY_D;
	case 'E': case 'e':		return KEY_E;
	case 'F': case 'f':		return KEY_F;
	case 'G': case 'g':		return KEY_G;
	case 'H': case 'h':		return KEY_H;
	case 'I': case 'i':		return KEY_I;
	case 'J': case 'j':		return KEY_J;
	case 'K': case 'k':		return KEY_K;
	case 'L': case 'l':		return KEY_L;
	case 'M': case 'm':		return KEY_M;
	case 'N': case 'n':		return KEY_N;
	case 'O': case 'o':		return KEY_O;
	case 'P': case 'p':		return KEY_P;
	case 'Q': case 'q':		return KEY_Q;
	case 'R': case 'r':		return KEY_R;
	case 'S': case 's':		return KEY_S;
	case 'T': case 't':		return KEY_T;
	case 'U': case 'u':		return KEY_U;
	case 'V': case 'v':		return KEY_V;
	case 'W': case 'w':		return KEY_W;
	case 'X': case 'x':		return KEY_X;
	case 'Y': case 'y':		return KEY_Y;
	case 'Z': case 'z':		return KEY_Z;
	default:				return KEY_NONE;
	}
}

}