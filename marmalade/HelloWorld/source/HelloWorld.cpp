#include "s3e.h"
#include "IwGx.h"

int main()
{
	IwGxInit();
	IwGxSetColClear(0, 0, 0xff, 0xff);

	while (!s3eDeviceCheckQuitRequest() &&
				 !(s3eKeyboardGetState(s3eKeyEsc) & S3E_KEY_STATE_DOWN) &&
				 !(s3eKeyboardGetState(s3eKeyAbsBSK) & S3E_KEY_STATE_DOWN))
	{
		IwGxClear();
		IwGxPrintString(120, 150, "Hello, World!");
		IwGxFlush();
		IwGxSwapBuffers();
		s3eDeviceYield(0);
	}

	IwGxTerminate();
	return 0;
}

