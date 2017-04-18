#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>

void silence_stderr()
{
	int fd = open("/dev/null", O_WRONLY);

	if (fd < 0) {
		perror("fatal: open /dev/null");
		exit(EXIT_FAILURE);
	}

	dup2(fd, STDERR_FILENO);
}
