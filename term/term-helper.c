#include <stdlib.h>
#include <unistd.h>
#include <assert.h>

#ifdef __APPLE__
#include <util.h>
#include <sys/ioctl.h>
#else
#include <pty.h>
#endif

__asm__(".symver forkpty,forkpty@GLIBC_2.2.5");

void run_shell(int rows, int cols, const char *program, char* const argv[],
               const char *term_env, pid_t* pid, int* fd)
{
  struct winsize win = { rows, cols, 0, 0 };
  *pid = forkpty(fd, NULL, NULL, &win);
  assert(*pid >= 0);
  if (*pid == 0) {
    setenv("TERM", term_env, 1);
    assert(execvp(program, argv) >= 0);
  }
}
