/* -*-C-*-

Copyright (c) 1987 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. */

/* $Header: unixlib.c,v 10.0 88/12/07 13:11:49 las Exp $
   $MIT-Header: unixlib.c,v 9.38 87/04/11 14:44:53 GMT jinx Rel $

   Unix utilities for several user primitive files. 
*/

/* Imports */

#include "config.h"

#ifdef bsd
#include <strings.h>
#else
#include <string.h>
#define index strchr
#endif

#ifdef hpux
#include <fcntl.h>
#endif

#ifdef spectrum
#include <sys/types.h>
#endif

#include <sys/file.h>

#ifndef NULL
#define NULL	0
#endif

/* Exports: */

extern Boolean file_exists();
extern void unix_find_pathname();

/* Check whether a file exists */

Boolean 
unix_file_exists(name)
     char *name;
{
  int desc;

  if ((desc = open(name, O_RDONLY|O_NDELAY, 0)) != -1)
  {
    close(desc);
    return true;
  }
  return false;
}

/* Find the pathname corresponding to program_name. */

void
unix_find_pathname(program_name, target)
     char *program_name, *target;
{
  char *path, *next;
  int length;
  extern char *getenv();

  /* Attempt first in the home directory */

  if ((program_name[0] == '/') ||		/* Absolute path */
      (unix_file_exists(program_name)) ||	/* In current directory */
      ((path = getenv("PATH")) == NULL))
  {
    strcpy(target, program_name);
    return;
  }
  for (next = index(path, ':') ;
       path != NULL;
       path = next+1, next = index(path, ':'))
    {
      length = ((next == NULL) ? strlen(path) : (next-path));
      strncpy(target, path, length);
      target[length] = '/';
      target[length+1] = '\0';
      strcpy(&target[length+1], program_name);
      if (unix_file_exists(target))
	return;
    }
  strcpy(target, program_name);
  return;
}
