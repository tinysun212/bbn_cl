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

#include "transact.h"
#include <stdio.h>
#include <signal.h>
#include <fcntl.h>

extern int sigsetmask();

#define boolean		int
#define false		0
#define true		1
#define FALSE		0
#define TRUE		1

/* Random Utilities */

boolean
begins_with(name, what)
     char *name, *what;
{
  while (*what != '\0')
  {
    if (*name == '\0')
      return (false);
    if (*name != *what)
      return (false);
    name ++;
    what ++;
  }
  return (true);
}

boolean
special_stream_name(name)
     char *name;
{
  return
    begins_with(name, "METER:") ||
      begins_with(name, "LISPM:");
}

/* Input stream manipulation */

#define INPUT_REQUEST_STACK
#define MAX_STREAM_STACK 256

extern void set_input_stream();

static int
  input_stream_pointer,
  input_stream_queue[MAX_STREAM_STACK];

void
initialize_input_stack()
{
  input_stream_pointer = 0;

  set_input_stream(-1);
  return;
}

void
add_input_request(incoming_stream, buffered)
     short int incoming_stream;
     boolean buffered;
{
  register int ptr;
  int stream, mask;

  mask = sigsetmask(-1);
  stream = ((int) incoming_stream);

  if (!buffered)
    stream |= 0x80000000;

#ifdef INPUT_REQUEST_STACK

  for (ptr = ((input_stream_pointer >= MAX_STREAM_STACK) ?
	      (MAX_STREAM_STACK - 1) :
	      input_stream_pointer);
       ptr > 0;
       ptr -= 1)
  {
    input_stream_queue[ptr] = input_stream_queue[ptr - 1];
  }
  input_stream_queue[0] = stream;
  input_stream_pointer += 1;
  if (input_stream_pointer > MAX_STREAM_STACK)
  {
    /* Ignore old pending requests. */

    input_stream_pointer = MAX_STREAM_STACK;
  }
  if ((input_stream_pointer == 1) ||
      (input_stream_queue[1] != stream))
  {
    set_input_stream(stream);
  }

#else /* not INPUT_REQUEST_STACK, a queue */

  if (input_stream_pointer < MAX_STREAM_STACK)
  {
    /* Otherwise ignore the request. */

    input_stream_queue[input_stream_pointer++] = stream;
    if (input_stream_pointer == 1)
    {
      set_input_stream(stream);
    }
  }

#endif /* INPUT_REQUEST_STACK */

  sigsetmask(mask);
  return;
}

void
remove_input_request(stream)
     short int stream;
{
  register int from, to;
  int old_stream, mask;

  mask = sigsetmask(-1);
  if (stream == ((short int) (-1)))
  {
    /* Flush everything, and tell the input server. */

    input_stream_pointer = 0;
    set_input_stream(-1);
  }
  else if (input_stream_pointer != 0)
  {
    /* If it is 0, we can't give an error because the input
       queue/stack may have overflowed.
     */

    old_stream = input_stream_queue[0];

    for (to = 0, from = 0; from < input_stream_pointer; from++)
    {
      if (((short int) ((input_stream_queue[from]) & 0x7fffffff)) != stream)
	input_stream_queue[to++] = input_stream_queue[from];
    }
    input_stream_pointer = to;

    if ((to == 0) || (input_stream_queue[0] != old_stream))
    {
      set_input_stream((to == 0) ?
		       (-1) :
		       input_stream_queue[0]);
    }
  }
  sigsetmask(mask);
  return;
}

extern void flush_current_input_request();

void
flush_current_input_request()
{
  if (input_stream_pointer != 0)
    remove_input_request(input_stream_queue[0]);
  return;
}

/* Local output server.

   This procedure manages output to the controlling terminal when we
   are not using BLUI.
 */

extern void run_local_output_server();
extern int Debug_Flags[];

int last_output_stream;
int noisy_mode = false;

void
run_local_output_server(request_pipe, exit_loc)
     int request_pipe;
     short int *exit_loc;
{
  extern int errno;
  char kind, string[2048];
  int code, temp;
  short int length, stream;

  last_output_stream = (-1);

  if (Debug_Flags[21])
    printf("Output server read to listen\n");

  while (!(*exit_loc)) {
    code = read(request_pipe, &kind, 1);
    if (code <= 0)
    {
      if (code != 0)
	fprintf(stderr, "output server: Broken request pipe - %d\n", code);
      goto exit_output_server;
    }

Recover_From_Error:
    code = read(request_pipe, &stream, 2);
    if (code <= 0) {
      fprintf(stderr, "output server: Broken request pipe - %d\n", code);
      goto exit_output_server;
    }

    if (Debug_Flags[21])
      printf("(output server): Request kind=%d stream=%d\n", kind, stream);

    switch (kind) {
    case REQ_INPUT:
      add_input_request(stream, false);
      break;

    case REQ_BUF_INPUT:
      add_input_request(stream, true);
      break;

    case FLUSH_REQ:
      remove_input_request(stream);	  
      break;

    case OUTPUT_CHAR:
      code = read(request_pipe, string, 1);
      if (code <= 0)
	break;
      string[1] = '\0';

      if (noisy_mode &&
	  stream != last_output_stream)
	printf("\nOutput from stream %d:\n", stream);

      printf("%s", string);
      fflush(stdout);

      last_output_stream = stream;
      break;

    case OUTPUT_STRING:
      code = read(request_pipe, &length, 2);
      if (code <= 0 || length <= 0 || length > 255)
	break;
      code = read(request_pipe, string, length);
      if (code <= 0)
	break;      
      string[length] = '\0';

      if (noisy_mode &&
	  stream != last_output_stream)
	printf("\nOutput from stream %d:\n", stream);
      
      printf("%s", string);
      fflush(stdout);

      last_output_stream = stream;
      break;

    case CREATE_STREAM:
      code = read(request_pipe, &length, 2);
      if (code <= 0 || length <= 0 || length > 255)
	break;
      code = read(request_pipe, string, length);
      if (code <= 0)
	break;
      string[length] = '\0';
      
      add_meter_stream(string, stream);

      if (noisy_mode && !special_stream_name(string))
	printf("Opening stream %d: %s\n", stream, string);
      last_output_stream = stream;
      break;

    case CLOSE_STREAM:
      remove_input_request(stream);
      printf("Closing stream %d\n", stream);
      break;

    case METER_INFO:
      length = stream;
      string[0] = METER_INFO;
      string[1] = length >> 8;
      string[2] = length;
      read(request_pipe, &string[3], length);
      write_meter_data(string, length + 3);
      break;

    case WHOLINE_INFO:
    case DEF_NAME:
      length = stream;
      code = read(request_pipe, string, length);
      break;

    case REQ_EXIT:
      goto exit_output_server;

    case TURN_ON_MSGS:
      noisy_mode = stream;
      break;

    case START_METERING:
      close_meter_file();
      code = read(request_pipe, &length, 2);
      if (code <= 0 || length <= 0 || length > 255)
	break;
      code = read(request_pipe, string, length);
      if (code <= 0)
	break;      
      string[length] = '\0';
      open_meter_file(string);
      break;

    case END_METERING:
      close_meter_file();
      break;

    default:
      while ((*exit_loc) &&
	     (kind <= 0 || kind > MAX_OUT_TRANSACTION))
	read(request_pipe, &kind, 1);
      goto Recover_From_Error; } }

exit_output_server:
  exit(0);
}

#define METER_BUFLEN 256

int the_meter_file = 0, meter_posn;
char meter_buffer[METER_BUFLEN];

add_meter_stream(name, number)
char *name;
int number;
{ char header[5];
  int len;

  if (the_meter_file < 0)
    return;

  if (name[0] != 'M' || name[1] != 'E' || name[2] != 'T' ||
      name[3] != 'E' || name[4] != 'R' || name[5] != ':')
    return;

  name = &name[6];
  len = strlen(name);

  header[0] = CREATE_STREAM;
  header[1] = number >> 8;
  header[2] = number;
  header[3] = len >> 8;
  header[4] = len;
  if (the_meter_file > 0)
    {
      write(the_meter_file, header, sizeof(header));
      write(the_meter_file, name, len);
    }
}

open_meter_file(name)
char *name;
{
  the_meter_file = open(name, O_WRONLY + O_CREAT + O_TRUNC, 0666);
  if (the_meter_file <= 0)
    return;

  meter_posn = 0;
}

write_meter_data(data, datalen)
char *data;
int datalen;
{
  if (the_meter_file > 0)
    while (datalen > 0)
      {
	if (meter_posn >= METER_BUFLEN)
	  {
	    write(the_meter_file, meter_buffer, meter_posn);
	    meter_posn = 0;
	  }
	meter_buffer[meter_posn++] = *data++;
	datalen--;
      }
}

close_meter_file()
{
  if (the_meter_file > 0)
    {
      if (meter_posn > 0)
	write(the_meter_file, meter_buffer, meter_posn);
      close(the_meter_file);
      the_meter_file = (-1);
    }
}
