/*
                                ********

Copyright 1992 by BBN Systems and Technologies, A division of Bolt,
Beranek and Newman Inc.

Permission to use, copy, modify and distribute this software and its
documentation is hereby granted without fee, provided that the above
copyright notice and this permission appear in all copies and in
supporting documentation, and that the name Bolt, Beranek and Newman
Inc. not be used in advertising or publicity pertaining to distribution
of the software without specific, written prior permission.  In
addition, BBN makes no respresentation about the suitability of this
software for any purposes.  It is provided "AS IS" without express or
implied warranties including (but not limited to) all implied warranties
of merchantability and fitness.  In no event shall BBN be liable for any
special, indirect or consequential damages whatsoever resulting from
loss of use, data or profits, whether in an action of contract,
negligence or other tortuous action, arising out of or in connection
with the use or performance of this software.

                                ********
*/

#include "scheme.h"
#include "cl-fileio.h"

#include <stdio.h>

#include <sys/types.h>
#include <sys/times.h>
#include <sys/file.h>
#include <signal.h>
#include <errno.h>
#include <sys/stat.h>
#include <pwd.h>

/* ************************************
  Common Lisp low-level I/O.
  CL has its own buffering system.
   ************************************ */

/*
  Open.
  Processes Common Lisp mode args.
  Returns a non-negative fd if a-ok,
  else returns -1, and the status arg is set to an os-dependent
  number whose "text" can be obtained by some implementation-dependent
  call, to be done later.
*/

int
OS_cl_file_open(name,direction,if_exists,if_does_not_exist,status,file_exists_p)
char *name;
int direction, if_exists, if_does_not_exist, *status;
Boolean *file_exists_p;
{
  int open_flags = 0;
  int seek_to_eof = 0;
  int fd, lseek_status;
  extern int errno;

  switch (direction)
    {
    case CL_FILE_INPUT:
      {
	open_flags = O_RDONLY;
	if (if_does_not_exist == CL_FILE_CREATE) open_flags |= O_CREAT;
	break;
      }
    case CL_FILE_OUTPUT:
    case CL_FILE_IO:
      {
	if (direction == CL_FILE_IO) open_flags = O_RDWR;
	else open_flags = O_WRONLY;
	if (if_does_not_exist == CL_FILE_CREATE) open_flags |= O_CREAT;
	switch (if_exists)
	  {
	  case CL_FILE_ERROR:
	  case CL_FILE_NIL:
	    {
	      open_flags |= O_CREAT | O_EXCL;
	      break;
	    }
	  case CL_FILE_NEW_VERSION:
	  case CL_FILE_RENAME:
	  case CL_FILE_RENAME_AND_DEL:
	  case CL_FILE_SUPERSEDE:
	    {
	      open_flags |= O_TRUNC;
	      break;
	    }
	  case CL_FILE_APPEND:
	    {
	      seek_to_eof = 1;
	    }
	  }
      }
    }

  if (OS_file_existence_test(name) > 0) *file_exists_p = true;
  else *file_exists_p = false;

  fd = open(name,open_flags,0xffffffff);
  if (fd == -1) 
    {
      *status = errno;
      if (*status==ENOENT) *file_exists_p = false;
    }
  else if (seek_to_eof)
    { 
      lseek_status = lseek(fd,0,2);
      if (lseek_status == -1)
	{
	  *status = errno;
	  return -1;
	}
    }
  return fd;
}

/*
  Close.
  Returns 0 if ok, else -1
  and stuffs status with error code as described above
*/

int
OS_cl_file_close(fd,status)
int fd, *status;
{
  extern int errno;
  
  if (close(fd) == -1)
    {
      *status = errno;
      return -1;
    }
  else return 0;
}

/*
  Read a buffer.
  Return value:
    > 0  --   number of bytes read.
    0    --   eof
    -1   ---  error; status stuffed as above
*/

int
OS_cl_file_read_chars(fd,buf,nchars,status)
int fd, nchars, *status;
char *buf;
{
  int n_read;
  extern int errno;

  n_read = read(fd,buf,nchars);
  *status = errno;
  return n_read;
}

/*
  Write a buffer.
  Returns number bytes written, or -1 if error.
  status stuffed as above.
*/

int
OS_cl_file_write_chars(fd,buf,nchars,status)
int fd, nchars, *status;
char *buf;
{
  int n_written;
  extern int errno;

  n_written = write(fd,buf,nchars);
  *status = errno;
  return n_written;
}

int
OS_cl_file_set_pos(fd,pos,status)
int fd, pos, *status;
{
  extern int errno;
  int r;
  
  if (pos==-1) r = lseek(fd,0,2);
  else r = lseek(fd,pos,0);
  *status = errno;
  return r;
}

int
OS_cl_file_get_pos(fd,status)
int fd, *status;
{
  extern int errno;
  int pos;

  pos = lseek(fd,0,1);
  *status = errno;
  return pos;
}
/*
  Common Lisp requires this support. 
  Given a filename and a pointer to a block of
  type time_t, write the block.

  Returns zero if ok, else error number.
*/

long
OS_file_write_date(filename, time)
char *filename;
time_t *time;
{
  struct stat filestat;
  long res;

  res = stat(filename,&filestat);
  *time = filestat.st_mtime;
  return res;
}

/*
  Common Lisp requires this support. 
  The "author" of a file is its owner, in the
  standard unix context.

  Returns a pointer somewhere into system space;
  copy the name as soon as you get it.

  Return value of zero indicates error.
*/

char *
OS_file_author(filename)
char *filename;
{
  struct stat filestat;
  struct passwd *pw;

  if (stat(filename,&filestat) != 0) return (char *)0;
  pw = getpwuid(filestat.st_uid);
  return pw->pw_name;
}
