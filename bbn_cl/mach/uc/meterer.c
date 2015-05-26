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

/*
  The metering demon sends metering info packets to the Lisp machine.
  It could be modified to draw an X11 window to display the same data.
*/

#include "scheme.h"
#include "zones.h"
#include "transact.h"
#include <sys/signal.h>
#include <sys/time.h>

#define WHOLINE_VERSION 2
#define PACKET_HEAD_SIZE 16

extern long Local_Chunk_Size;
extern int Debug_Flags[];

meter_alarm_handler()
{ }

meter_server()
{
  char packet[1500];
  int packsize, restsize, queuesize, chunksize, pn, range;
  long now, percent, interval;
  struct itimerval new_val, old_val;

  signal(SIGALRM, meter_alarm_handler);

  while (true)
    {
      interval = SHARED_DATA->Wholine_Rate; /* In milliseconds */
      new_val.it_value.tv_sec = interval / 1000;
      new_val.it_value.tv_usec = (interval % 1000) * 1000;
      new_val.it_interval.tv_sec = 0;
      new_val.it_interval.tv_usec = 0;
      setitimer(ITIMER_REAL, &new_val, &old_val);
      sigpause(0);

      packsize = 4 * N_Interps + PACKET_HEAD_SIZE;
      restsize = packsize - 3;

      packet[0] = WHOLINE_INFO;
      packet[1] = restsize >> 8;
      packet[2] = restsize;
      packet[3] = WHOLINE_VERSION;
      packet[4] = N_Interps;
      packet[5] = N_Interps;

      queuesize = SHARED_DATA->Work_Queue.queue_count;
      packet[6] = queuesize >> 8;
      packet[7] = queuesize;

      chunksize = Local_Chunk_Size;
      packet[8] = chunksize >> 24;
      packet[9] = chunksize >> 16;
      packet[10] = chunksize >> 8;
      packet[11] = chunksize;

      now = System_Clock();
      packet[12] = now >> 24;
      packet[13] = now >> 16;
      packet[14] = now >> 8;
      packet[15] = now;

      for (pn = 0; pn < N_Interps; pn++)
	{
	  packet[pn + PACKET_HEAD_SIZE] = SHARED_DATA->Processor_Zone[pn];
	  packet[pn + PACKET_HEAD_SIZE + N_Interps] = 0;
	  packet[pn + PACKET_HEAD_SIZE + N_Interps + N_Interps] = 0;
	  range = (SHARED_DATA->Memory_Table[pn].Free_Top -
		   SHARED_DATA->Memory_Table[pn].Free_Bottom);
	  percent = (100 * range) / Local_Chunk_Size;
	  if (percent < 0) percent = 0;
	  if (percent > 100) percent = 100;
	  percent = 100 - percent;
	  packet[pn + PACKET_HEAD_SIZE + N_Interps + N_Interps + N_Interps] = percent;
	  if (Debug_Flags[23])
	    printf("pn=%d FT=0x%x FB=0x%x r=0x%x (%d/%d) per=%d\n",
		   pn, SHARED_DATA->Memory_Table[pn].Free_Top,
		   SHARED_DATA->Memory_Table[pn].Free_Bottom,
		   range, range, Local_Chunk_Size, percent);
	}

      while(atomior(&SHARED_DATA->Lisp_Output_Lock, 0x8000) != 0);
      write(SHARED_DATA->Lisp_Output_Pipe[WRITE_TO_PIPE],
	    packet, packsize);
      SHARED_DATA->Lisp_Output_Lock = 0;
    }
}
