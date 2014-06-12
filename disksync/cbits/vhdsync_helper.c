/*
 * Copyright (c) 2011 Citrix Systems, Inc.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

int fork_close_and_exec(int num_open_fds, int open_fds[], char* argv[])
{
	int i, maxfd;
	int* to_leave_open;
	pid_t pid = fork();
	if (0 == pid) {
		// child closes all fds except those specified
		// before exec'ing the specified process
		maxfd = sysconf(_SC_OPEN_MAX);
		to_leave_open = calloc(maxfd, sizeof(int));
		for (i=0; i < num_open_fds; i++) {
			to_leave_open[open_fds[i]] = 1;
		}
		for (i=0; i < maxfd; i++) {
			if (!to_leave_open[i]) close(i);
		}

		execv(argv[0], argv);
		perror("Unix error: ");
		_exit(127);
	}
	if (pid == -1) {
		perror("Unix error: ");
		_exit(126);
	}
	return (int)pid;
}

