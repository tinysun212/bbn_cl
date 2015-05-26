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

/* -*-C-*- */

#include "transact.h"
#include "interrupt.h"
#include <stdio.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <errno.h>
extern int errno;

/* Utilities */

int
server_fail(saved_errno, name, message)
     int saved_errno;
     char *name, *message;
{
  extern int sys_nerr;
  extern char *sys_errlist[];
  
  fprintf(stderr, "BLUI %sput failure: %s", name, message);
  if (saved_errno >= sys_nerr)
  {
    fprintf(stderr, "; errno = %d.\n", saved_errno);
  }
  else
  {
    fprintf(stderr,
	    ".\nerrno = %d (%s).\n",
	    saved_errno,
	    sys_errlist[saved_errno]);
  }
  return (-1);
}

int
setup_socket(name, port, host)
     char *name;
     int port;
     struct hostent *host;
{
  struct sockaddr_in addr;
  int saved_errno;
  int s1, s2;

  addr.sin_family = host->h_addrtype;
  bcopy(host->h_addr, ((caddr_t) &addr.sin_addr), host->h_length);
  addr.sin_port = port;

  s1 = socket(PF_INET, SOCK_STREAM, 0);
  if (s1 == -1)
  {
    return server_fail(errno, name, "socket failed");
  }
  if (connect(s1, ((caddr_t) &addr), sizeof(addr)) != 0)
  {
    saved_errno = errno;
    close(s1);
    return server_fail(errno, name, "connect failed");
  }
  return (s1);
}

/* The server itself */

static int
  input_server_pid,
  output_server_pid;

void
kill_server()
{
  if (getpid() == output_server_pid)
    kill(input_server_pid, SIGTERM);
  else
    kill(output_server_pid, SIGTERM);
  exit(1);
}

void
run_server(bi, bo)
     int bi, bo;
{
  int saved_errno;
  int the_pipe[2];
  void
    run_input_server(),
    run_output_server();
  
  if (pipe(&the_pipe[0]) != 0)
  {
    saved_errno = errno;
    close(bi);
    close(bo);
    server_fail(saved_errno, "out", "pipe failed");
    exit(1);
  }

  signal(SIGUSR2, SIG_IGN); /* Until ready */
  input_server_pid = getpid();
  output_server_pid = fork();
  switch(output_server_pid)
  {
    case 0:
    {
      /* Child process: Output server. */
      output_server_pid = getpid();
      close(bi);
      close(the_pipe[READ_FROM_PIPE]);
      if (fcntl(the_pipe[WRITE_TO_PIPE], F_SETFL, FNDELAY) == -1)
      {
	saved_errno = errno;
	close(bo);
	close(the_pipe[WRITE_TO_PIPE]);
	server_fail(saved_errno, "out", "fnctl failed");
	exit(1);
      }
      run_output_server(bo, the_pipe[WRITE_TO_PIPE]);
      exit(1);
    }

    default:
    {
      /* Parent process: Input server. */
      signal(SIGCHLD, kill_server);
      close(bo);
      close(the_pipe[WRITE_TO_PIPE]);
      if (fcntl(the_pipe[READ_FROM_PIPE], F_SETFL, FNDELAY) == -1)
      {
	saved_errno = errno;
	close(bi);
	close(the_pipe[READ_FROM_PIPE]);
	server_fail(saved_errno, "in", "fnctl failed");
	exit(1);
      }
      run_input_server(bi, the_pipe[READ_FROM_PIPE]);
      kill_server();
    }
  }
  /*NOTREACHED*/
}

/* Output server.  It uses the local output server code. */

static int
  input_server_communication_pipe;

void
run_output_server(bf_requests, is_pipe)
     int bf_requests, is_pipe;
{
  extern void
    run_local_output_server(),
    flush_current_input_request();
  short int always_false;

  always_false = 0;
  input_server_communication_pipe = is_pipe;
  signal(SIGINT, SIG_IGN);
  signal(SIGQUIT, SIG_IGN);
  signal(SIGUSR2, flush_current_input_request);
  run_local_output_server(bf_requests, &always_false);
  exit(1);
}

extern void
  set_input_stream();

void
set_input_stream(value)
     int value;
{
  int mask, result;

  mask = sigsetmask(-1);
  result = write(input_server_communication_pipe, &value, sizeof(value));
  if (result != sizeof(value))
  {
    if ((result == -1) && (errno == EWOULDBLOCK))
    {
      fprintf(stderr, "output server: Setting the input stream would block.\n");
      exit(1);
    }
    else
    {
      fprintf(stderr, "output server: Could not set input stream.\n");
      exit(1);
    }
  }
  kill(input_server_pid, SIGUSR2);
  sigsetmask(mask);
  return;
}

/* Input server.  It uses the local input server code. */

static int
  output_server_communication_pipe,
  butterfly_input_pipe;

void
run_input_server(bf_requests, outser_requests)
     int bf_requests, outser_requests;
{
  extern void run_local_input_server();

  output_server_communication_pipe = outser_requests;
  butterfly_input_pipe = bf_requests;
  run_local_input_server();
  exit(1);
}

extern int
  get_input_stream();

extern void
  send_input_data(),
  flush_input_request();

int
get_input_stream()
{
  int stream, result;

  result = read(output_server_communication_pipe, &stream, sizeof(stream));
  if (result != sizeof(stream))
  {
    if ((result == -1) && (errno == EWOULDBLOCK))
    {
      fprintf(stderr, "input server: Getting the input stream would block.\n");
      exit(1);
    }
    else
    {
      fprintf(stderr, "input server: Could not get input stream.\n");
      exit(1);
    }
  }
  return (stream);
}

void
flush_input_request()
{
  kill(output_server_pid, SIGUSR2);
  return;
}

void
send_input_data(request, data, size)
     int request;
     char *data;
     int size;
{
  char buffer[12];

  switch(request)
  {
    case SIG_CHAR_INT:
    {
      struct character_interrupt_data *ndata;
      
      ndata = ((struct character_interrupt_data *) data);
      buffer[0] = PROC_INTRUPT;
      buffer[1] = '\0';
      buffer[2] = '\0';
      buffer[3] = ((char) (ndata->kind));
      if (write(butterfly_input_pipe, &buffer[0], 4) != 4)
	kill_server();
      break;
    }

    case SIG_INPUT_INT:
    {
      struct input_interrupt_data *ndata;

      ndata = ((struct input_interrupt_data *) data);
      buffer[0] = BUFFER_AVAIL;
      buffer[1] = (((ndata->stream) >> 8) & 0xff);
      buffer[2] = ((ndata->stream) & 0xff);
      buffer[3] = (((ndata->length) >> 8) & 0xff);
      buffer[4] = ((ndata->length) & 0xff);
      if (write(butterfly_input_pipe, &buffer[0], 5) != 5)
	kill_server();
      if (write(butterfly_input_pipe, ndata->data, ndata->length) !=
	  ndata->length)
	kill_server();
      break;
    }

    default:
      fprintf(stderr, "Unknown request type %d\n", request);
  }
  return;
}

/* Interactive interrupts: dispatch
   Trivial version of the procedure in unix.c
 */

extern int
  Ask_Me();

int
Ask_Me(reader, intchar)
     int (*reader)(), *intchar;
{
  int who_cares;
  int command;

  putchar('\n');
  printf("Interrupt character: ");
  fflush(stdout);
  command = (*reader)(&who_cares);
  if ((command >= ((int) 'a')) && (command <= ((int) 'z')))
  {
    command = ((int) 'A') + (command - ((int) 'a'));
  }
  *intchar = command;
  return (INTERACTIVE_INTERRUPT);
}

#define PORT_NUMBER	3600
#define HOSTNAME	"buckeye.bbn.com"

void
main(argc, argv, envp)
     int argc;
     char **argv;
     char **envp;
{
  extern int atoi();
  char *hostname;
  int port_number, bo, bi;
  struct hostent *host;

  hostname = HOSTNAME;
  port_number = PORT_NUMBER;
  switch(argc)
  {
    default:
      fprintf(stderr, "usage: %s [host] [port]\n", argv[0]);
      exit(1);
      /*NOTREACHED*/

    case 3:
      port_number = atoi(argv[2]);
      /* fall through */

    case 2:
      hostname = argv[1];
      /* fall through */

    case 1:
      break;
  }

#ifdef DEBUG
  fprintf(stderr, "%s: Attempting to connect:\n", argv[0]);
  fprintf(stderr, "  hostname = \"%s\"\n", hostname);
  fprintf(stderr, "  port = %d\n", port_number);
  fprintf(stderr, "\n");
#endif /* DEBUG */

  host = gethostbyname(hostname);
  if (host == ((struct hostent *) NULL))
  {
    fprintf(stderr, "%s: Unknown host \"%s\".\n", argv[0], hostname);
    exit(1);
  }
  if ((bo = setup_socket("out", (port_number + 1), host)) == -1)
    exit(1);
  bi = setup_socket("in", port_number, host);
  if (bi == -1)
  {
    close(bo);
    exit(1);
  }
  run_server(bi, bo);
  exit(1);
}
