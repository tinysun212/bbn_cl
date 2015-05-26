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
#include <signal.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <errno.h>
extern int errno;

extern int Debug_Flags[];


/*
  The servers are:

  - The input server which reads from the keyboard and sends character
  level interrupts for either input text or interrupt characters.
  It has to handle things like ^G and ^B.

  - The file server waits for requests over a pipe and opens and closes
  files, does seeks, writes the specified data or returns resulting data.
*/


int file_server_pid = -2;
int input_server_pid = -2;

void
start_the_servers()
{
  extern void
    init_subprocess(),
    run_local_input_server(),
    run_file_server();

  /* Input Server */

  if ((input_server_pid = fork()) == 0)
    {
      init_subprocess(true);
      run_local_input_server();
      exit(0);
    }
  printf("Input server is process %d\n", input_server_pid);

  /* File server */

  pipe(SHARED_DATA->File_Server_Pipe);
  pipe(SHARED_DATA->File_Answer_Pipe);
  SHARED_DATA->File_Server_Lock = 0;

  if ((file_server_pid = fork()) == 0)
  {
    init_subprocess(true);
    run_file_server();
    exit(0);
  }
  printf("File server is process %d\n", file_server_pid);
  return;
}

void
kick_input_server()
{
  kill(input_server_pid, SIGUSR2);
}



extern void kill_the_servers();

void
kill_the_servers()
{
  extern void kill_file_server();

  /* The kill procedures give the servers a chance to flush
     their request queues.
   */
  if (file_server_pid >= 0)
    kill_file_server();
  if (input_server_pid >= 0)
    kill(input_server_pid, SIGTERM);
  sleep(1);
  if (file_server_pid >= 0)
    kill(file_server_pid, SIGTERM);
  return;
}

/* Input to processors.

   This procedures cycles through the processors sending succesive
   "packets", to succesive processors.

   Since signal handlers use this procedure as well, there could be
   a problem with the shared variable processor.

   This is currently not a problem because input is currently only sent
   when the signals are disabled (either by sigblock, or by sigvec).
   
   If this situation is ever changed, this procedure would have to do
   its own signal blocking.
 */

extern void
  send_input_data();

void
send_input_data(request, data, size)
     int request;
     char *data;
     int size;			/* in chars */
{
  static int processor = 0;

  Send_Signal_Info(processor++, request, ((int *) data), size);
  if (processor >= SHARED_DATA->N_Interpreters)
    processor = 0;
  return;
} 

void
wait_for_ext_msg_ack()
{
  if (Debug_Flags[10])
    printf("wait_for_ext_msg_ack: wait\n");
  while (SHARED_DATA->Ext_Msg_Ack == 0)
    {}
  if (Debug_Flags[10])
    printf("wait_for_ext_msg_ack: ack\n");
  return;
}

void
ack_ext_msg()
{
  if (Debug_Flags[10])
    printf("ack_ext_msg\n");
  SHARED_DATA->Ext_Msg_Ack = 1;
  return;
}

void
clear_ext_msg_ack()
{
  if (Debug_Flags[10])
    printf("clear_ext_msg_ack\n");
  SHARED_DATA->Ext_Msg_Ack = 0;
  return;
}

