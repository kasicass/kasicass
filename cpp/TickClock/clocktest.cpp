#if defined(USE_CHRONO_CLOCK)
  #define WIN32_LEAN_AND_MEAN
  #include <Windows.h>
  #include <stdio.h>
  #include "chronoclock.hpp"
#else
  #define WIN32_LEAN_AND_MEAN
  #include <Windows.h>
  #include <stdio.h>
  #include "win32clock.hpp"
#endif


int main(void) {
	TickClock clock;
	clock.start();
	Sleep(1000);  // 1 second
	clock.stop();
	printf("%f\n", clock.elapsed());
}
