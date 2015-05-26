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
#include "transact.h"

/* Requests serviced by the file server */

#define RQ_OPEN		100
#define RQ_CLOSE	101
#define RQ_LSEEK	102
#define RQ_READ		103
#define RQ_WRITE	104
#define RQ_CWD		105
#define RQ_GETCWD	106
#define RQ_FOPEN	107
#define RQ_FCLOSE	108
#define RQ_FREAD	109
#define RQ_FWRITE	110
#define RQ_FSEEK	111
#define RQ_FEOF		112
#define RQ_FILENO	113
#define RQ_PUTC		114
#define RQ_EXIT		115
#define RQ_FERROR	116
#define RQ_CLEARERR	117
#define RQ_FFLUSH	118

extern void
  Start_Direct_IO(),
  Finish_Direct_IO();

static Boolean Do_IO_Directly = false; /* Use direct I/O for this */

void
Start_Direct_IO()
{
  Do_IO_Directly = true;
  return;
}

void
Finish_Direct_IO()
{
  Do_IO_Directly = false;
  return;
}

void
Direct_IO_Setup()
{
  char wdir[256], *bfly_getwd();

  bfly_getwd(wdir, sizeof(wdir));
  chdir(wdir);
} 

/* Utility procedures */

void
server_send_string(str)
char *str;
{ int len;

  len = strlen(str) + 1;
  write(SHARED_DATA->File_Server_Pipe[WRITE_TO_PIPE], &len, sizeof(len));
  write(SHARED_DATA->File_Server_Pipe[WRITE_TO_PIPE], str, len);
  return; }

void
server_read_string(str)
char *str;
{ int len; 

  read(SHARED_DATA->File_Server_Pipe[READ_FROM_PIPE], &len, sizeof(len));
  read(SHARED_DATA->File_Server_Pipe[READ_FROM_PIPE], str, len);
  return; }

void
server_send_answer(val)
int val;
{ write(SHARED_DATA->File_Answer_Pipe[WRITE_TO_PIPE], &val, sizeof(val));
  return; }

void
server_send_number(num)
int num;
{ write(SHARED_DATA->File_Server_Pipe[WRITE_TO_PIPE], &num, sizeof(num));
  return; }

int
server_read_number()
{ int num;

  read(SHARED_DATA->File_Server_Pipe[READ_FROM_PIPE], &num, sizeof(num));
  return (num); }

/* The file server */

extern void run_file_server();

#define BUFFER_SIZE 4096

extern int Debug_Flags[];

void
run_file_server()
{
  int code, request, flags, mode, len, answer, fd, ch, nelemt;
  char buffer[BUFFER_SIZE], mode_buf[32];

  while (!SHARED_DATA->Exit_In_Progress) {
    code = read(SHARED_DATA->File_Server_Pipe[READ_FROM_PIPE],
		&request, sizeof(request));
    if (code < 0) _exit(1);

    if (Debug_Flags[2])
      printf("File server received request %d\n", request);

    switch (request) {
    case RQ_OPEN:
      flags = server_read_number();
      mode = server_read_number();
      server_read_string(buffer);
      if (Debug_Flags[2]) printf("Opening %s\n", buffer);
      server_send_answer(open(buffer, flags, mode));
      break;

    case RQ_FOPEN:
      server_read_string(buffer);
      server_read_string(mode_buf);
      if (Debug_Flags[2]) printf("Opening %s\n", buffer);
      server_send_answer(fopen(buffer, mode_buf));
      break;

    case RQ_CLOSE:
      server_send_answer(close(server_read_number()));
      break;

    case RQ_FCLOSE:
      server_send_answer(fclose(((FILE *) server_read_number())));
      break;

    case RQ_FFLUSH:
      server_send_answer(fflush(((FILE *) server_read_number())));
      break;

    case RQ_LSEEK:
      fd = server_read_number();
      len = server_read_number();
      mode = server_read_number();
      server_send_answer(lseek(fd, len, mode));
      break;

    case RQ_FSEEK:
      fd = server_read_number();
      len = server_read_number();
      mode = server_read_number();
      server_send_answer(fseek(((FILE *) fd), len, mode));
      break;

    case RQ_READ:
      fd = server_read_number();
      len = server_read_number();
      answer = read(fd, buffer, min(sizeof(buffer), len));
      server_send_answer(answer);
      if (answer > 0)
	write(SHARED_DATA->File_Answer_Pipe[WRITE_TO_PIPE], buffer, answer);
      break; 

    case RQ_FREAD:
      fd = server_read_number();
      len = server_read_number();
      answer = fread(buffer, 1, min(sizeof(buffer), len), ((FILE *) fd));
      server_send_answer(answer);
      if (answer > 0)
	write(SHARED_DATA->File_Answer_Pipe[WRITE_TO_PIPE], buffer, answer);
      break; 

    case RQ_WRITE:
      fd = server_read_number();
      len = server_read_number();
      if (Debug_Flags[2])
	printf("RQ_WRITE: fd = %d, len = %d (0x%x)\n", fd, len, len);
      while (len > 0)
	{
	  nelemt = read(SHARED_DATA->File_Server_Pipe[READ_FROM_PIPE], buffer, len);
	  if (nelemt < 0) break;
	  code = write(fd, buffer, nelemt);
	  if (Debug_Flags[2])
	    printf("RQ_WRITE: nelemt = %d (0x%x), code = %d (0x%x)\n", nelemt, nelemt, code, code);
	  server_send_answer(code);
	  if (code < 0) break;
	  len -= nelemt;
	}
      break;

    case RQ_FWRITE:
      fd = server_read_number();
      len = server_read_number();
      while (len > 0)
	{
	  nelemt = read(SHARED_DATA->File_Server_Pipe[READ_FROM_PIPE], buffer, len);
	  if (nelemt < 0) break;
	  code = fwrite(buffer, 1, nelemt, fd);
	  server_send_answer(code);
	  if (code < 0) break;
	  len -= nelemt;
	}
      if (((FILE *) fd) == stdout)
	fflush(stdout);
      break;

    case RQ_FEOF:
      server_send_answer(feof(((FILE *) server_read_number())));
      break;

    case RQ_FERROR:
      server_send_answer(ferror(((FILE *) server_read_number())));
      break;

    case RQ_CLEARERR:
      server_send_answer(clearerr(((FILE *) server_read_number())));
      break;

    case RQ_FILENO:
      server_send_answer(fileno(((FILE *) server_read_number())));
      break;

    case RQ_CWD:
      server_read_string(buffer);
      server_send_answer(chdir(buffer));
      break;

    case RQ_GETCWD:
      answer = getwd(buffer, sizeof(buffer));
      if (answer == NULL)
	server_send_answer(NULL);
      else {
	len = strlen(buffer) + 1;
	server_send_answer(len);
	write(SHARED_DATA->File_Answer_Pipe[WRITE_TO_PIPE], buffer, len); }
      break;

    case RQ_PUTC:
      ch = server_read_number();
      fd = server_read_number();
      server_send_answer(putc(ch, ((FILE *) fd)));
      fflush(((FILE *) fd));
      break;

    case RQ_EXIT:      
      _exit(0);
      /*NOTREACHED*/
    }
  }
  _exit(0);
}

void
grab_file_server(request)
int request;
{
  if (Debug_Flags[2]) printf("grab_file_server %d (lock = %x)\n",
			     request, SHARED_DATA->File_Server_Lock);
  while (atomior(&SHARED_DATA->File_Server_Lock, 0x8000) != 0x0000);
  write(SHARED_DATA->File_Server_Pipe[WRITE_TO_PIPE],
	&request, sizeof(request));
  return; }

int
release_file_server(get)
int get;
{ int code, read_val;

  code = 0;

  if (get) {
    read_val = read(SHARED_DATA->File_Answer_Pipe[READ_FROM_PIPE],
		    &code, sizeof(code)); }

  if (Debug_Flags[2])
    printf("release_file_server(%d) -> %x (read returned %x)\n", get, code, read_val);
  SHARED_DATA->File_Server_Lock = 0;
  return (code); }

int
bfly_open(name, flags, mode)
char *name;
int flags, mode;
{
  if (Do_IO_Directly) {
    Direct_IO_Setup();		/* Make sure our wdir is the same as fileser's. */
    return open(name, flags, mode); }

  grab_file_server(RQ_OPEN);
  server_send_number(flags);
  server_send_number(mode);
  server_send_string(name);
  return (release_file_server(true)); }

FILE *
bfly_fopen(name, mode)
char *name, *mode;
{
  if (Do_IO_Directly)
  {
    Direct_IO_Setup();		/* Make sure our wdir is the same as fileser's. */
    return (fopen(name, mode));
  }

  grab_file_server(RQ_FOPEN);
  server_send_string(name);
  server_send_string(mode);
  return ((FILE *) (release_file_server(true)));
}

int
bfly_close(fd)
int fd;
{
  if (Do_IO_Directly)
    return close(fd);

  grab_file_server(RQ_CLOSE);
  server_send_number(fd);
  return (release_file_server(true)); }

int
bfly_fclose(stream)
     FILE *stream;
{
  if (Do_IO_Directly)
    return fclose(stream);

  grab_file_server(RQ_FCLOSE);
  server_send_number((int) stream);
  return (release_file_server(true));
}

int
bfly_fflush(stream)
     FILE *stream;
{
  if (Do_IO_Directly || (stream == stdout))
    return fflush(stream);

  grab_file_server(RQ_FFLUSH);
  server_send_number((int) stream);
  return (release_file_server(true));
}

int
bfly_lseek(fd, offset, whence)
int fd, offset, whence;
{
  if (Do_IO_Directly)
    return lseek(fd, offset, whence);

  grab_file_server(RQ_LSEEK);
  server_send_number(fd);
  server_send_number(offset);
  server_send_number(whence);
  return (release_file_server(true)); }

int
bfly_fseek(stream, offset, whence)
     FILE *stream;
     int offset, whence;
{
  if (Do_IO_Directly)
    return fseek(stream, offset, whence);

  grab_file_server(RQ_FSEEK);
  server_send_number((int) stream);
  server_send_number(offset);
  server_send_number(whence);
  return (release_file_server(true));
}

int
server_read_common(command, fd, buf, len)
int command, fd, len;
char *buf;
{ int n2xmit, code, nread, read_val;

  if (Debug_Flags[2]) printf("read common(%d, %d, %x, %d)\n",
			     command, fd, buf, len);
  nread = 0;
  while (len > 0)
    {
      n2xmit = min(len, BUFFER_SIZE);
      grab_file_server(command);
      server_send_number(fd);
      server_send_number(n2xmit);
      read_val = read(SHARED_DATA->File_Answer_Pipe[READ_FROM_PIPE], &code, sizeof(code));
      if (read_val < 0) {
	code = read_val;
	release_file_server(false);
	break; }
      if (code == 0) {
	release_file_server(false);
	break; }

      if (Debug_Flags[2]) printf("getting %d bytes (read returned %d (0x%x))\n", code, read_val, read_val);
      while (code > 0)
	{
	  read_val = read(SHARED_DATA->File_Answer_Pipe[READ_FROM_PIPE], buf, code);
	  if (read_val < 0)
	    {
	      code = read_val;
	      len = 0;
	      nread = 0;
	      break;
	    }
	  if (Debug_Flags[2]) printf("read returned %d (0x%x)\n", read_val, read_val);
	  code -= read_val;
	  len -= read_val;
	  nread += read_val;
	  buf += read_val;
	}
      release_file_server(false);
    }

  if (code < 0 && nread == 0)
    nread = code;
  if (Debug_Flags[2])
    printf("returning %d\n", nread);
  return (nread);
}

int
bfly_read(fd, buf, len)
int fd, len;
char *buf;
{
  if (len == 0) return 0;
  if (Do_IO_Directly) return read(fd, buf, len);
  return (server_read_common(RQ_READ, fd, buf, len)); }

int
bfly_fread(buf, m1, m2, stream)
     FILE *stream;
     int m1, m2;
     char *buf;
{

  if (m1 == 0 || m2 == 0)
    return 0;
  if (Do_IO_Directly)
    return fread(buf, m1, m2, stream);
  return (server_read_common(RQ_FREAD, ((int) stream), buf, m1 * m2));
}

int
server_write_common(command, fd, buf, len)
int command, fd, len;
char *buf;
{ int n2xmit, code, nsent, write_val;
  if (Debug_Flags[2])
    printf("server_write_common: fd = %d, len = %d (0x%x)\n", fd, len, len);

  nsent = 0;
  while (len > 0) {
    n2xmit = min(len, BUFFER_SIZE);
    grab_file_server(command);
    server_send_number(fd);
    server_send_number(n2xmit);
    write_val = write(SHARED_DATA->File_Server_Pipe[WRITE_TO_PIPE], buf, n2xmit);
    if (Debug_Flags[2])
      printf("server_write_common: n2xmit = %d (0x%x), write returned %d (0x%x)\n",
	     n2xmit, n2xmit, write_val, write_val);
    code = release_file_server(true);
    if (code <= 0) break;
    len -= code;
    nsent += code;
    buf += code; }

  if (code < 0 && nsent == 0)
    nsent = code;
  return (nsent); }

int
bfly_write(fd, buf, len)
int fd, len;
char *buf;
{
  if (Do_IO_Directly)
    return write(fd, buf, len);

  return (server_write_common(RQ_WRITE, fd, buf, len)); }

int
bfly_fwrite(buf, m1, m2, stream)
     char *buf;
     int m1, m2;
     FILE *stream;
{
  if (stream == stdout) 
    return
      fwrite(buf, m1, m2, stream);
  else
    return (server_write_common(RQ_FWRITE, ((int) stream), buf, m1 * m2));
}

int
bfly_chdir(name)
char *name;
{
  grab_file_server(RQ_CWD);
  server_send_string(name);
  return (release_file_server(true)); }

char *
bfly_getwd(name, maxlen)
char *name;
int maxlen;
{ int len;

  grab_file_server(RQ_GETCWD);
  read(SHARED_DATA->File_Answer_Pipe[READ_FROM_PIPE], &len, sizeof(len));
  if ((len > 0) && (len <= maxlen)) {
    read(SHARED_DATA->File_Answer_Pipe[READ_FROM_PIPE], name, len);
    release_file_server(false);
    return (name); }
  else {
    *name = '\0';
    release_file_server(false);
    return (NULL); } }

int
bfly_fileno(file)
     FILE *file;
{
  if (Do_IO_Directly || (file == stdout) || (file == stdin))
    return fileno(file);

  grab_file_server(RQ_FILENO);
  server_send_number((int) file);
  return (release_file_server(true));
}

int
bfly_feof(file)
     FILE *file;
{
  if (Do_IO_Directly)
    return feof(file);

  grab_file_server(RQ_FEOF);
  server_send_number((int) file);
  return (release_file_server(true));
}

int
bfly_ferror(file)
     FILE *file;
{
  if (Do_IO_Directly || (file == stdout))
    return ferror(file);

  grab_file_server(RQ_FERROR);
  server_send_number((int) file);
  return (release_file_server(true));
}

int
bfly_clearerr(file)
     FILE *file;
{
  if (Do_IO_Directly || (file == stdout))
    return clearerr(file);

  grab_file_server(RQ_CLEARERR);
  server_send_number((int) file);
  return (release_file_server(true));
}

int
bfly_putc(ch, fd)
int ch;
FILE *fd;
{
  if (Do_IO_Directly || fd == stdout)
    return putc(ch, fd);

  grab_file_server(RQ_PUTC);
  server_send_number(ch);
  server_send_number((long) fd);
  return (release_file_server(true)); }

int
bfly_putchar(ch)
int ch;
{ 
  if (Do_IO_Directly)
    return putchar(ch);

  grab_file_server(RQ_PUTC);
  server_send_number(ch);
  server_send_number(stdout);
  return (release_file_server(true)); }

extern void kill_file_server();

void
kill_file_server()
{
  int request;

  request = RQ_EXIT;
  write(SHARED_DATA->File_Server_Pipe[WRITE_TO_PIPE],
	&request, sizeof(request));
  return;
}

