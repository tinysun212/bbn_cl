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
#include "interrupt.h"
#include <setjmp.h>
#include <stdio.h>
#include <sgtty.h>
#include <errno.h>
#include <signal.h>

#define boolean		int
#define FALSE		0
#define TRUE		1

extern void
  send_input_data();

static boolean
  in_input_wait_p = FALSE;

static jmp_buf
  input_wait_state;

extern int Debug_Flags[];


/* Utilities */

/* Immediate IO for the interactive handler.

   This procedure assumes that the signals are blocked
   during its execution, so the read will not be interrupted.
*/

int
butterfly_tyi_within_interrupt(interrupted)
     boolean *interrupted;
{
  int nchars, old_flags;
  struct sgttyb tty_block;
  char buffer[1];

  ioctl(fileno(stdin), TIOCGETP, &tty_block);
  old_flags = tty_block.sg_flags;
  tty_block.sg_flags |= RAW;
  ioctl(fileno(stdin), TIOCSETP, &tty_block);
  nchars = read(fileno(stdin), &buffer[0], 1);
  tty_block.sg_flags = old_flags;
  ioctl(fileno(stdin), TIOCSETP, &tty_block);
  *interrupted = (nchars != 1);
  return ((int) buffer[0]);
}

/* 
   Controlling terminal state

   NOTES on interrupt stuff:
   The interrupt characters follow a different convention from
   that used normally:
    - SIGINT is the breakpoint or interactive handler (^B),
    - SIGQUIT is the abort handler (^G).
   The effect as viewed from Scheme should be the same.
 */ 

static boolean tty_bashed_p = FALSE;
static struct tchars the_chars;
static struct sgttyb TTY_Block;

void
reset_tty_input()
{
  int value;

  if (tty_bashed_p)
  {
    ioctl(fileno(stdin), TIOCSETC, &the_chars);
    tty_bashed_p = FALSE;
  }
  return;
}

void
initialize_tty_input()
{
  if (!tty_bashed_p)
  {
    struct tchars new_chars;

    ioctl(fileno(stdin), TIOCGETC, &the_chars);

    bcopy(((char *) &the_chars),
	  ((char *) &new_chars),
	  sizeof(the_chars));
    new_chars.t_intrc = '\003';	/* ^C */
    new_chars.t_quitc = '\007';	/* ^G */

    tty_bashed_p = TRUE;
    ioctl(fileno(stdin), TIOCSETC, &new_chars);
  }
  return;
}

/* Signal handlers */

int
exit_input_server()
{
  void reset_tty_input();

  reset_tty_input();
  _exit(0);
}

int
butterfly_suspend_handler()
{
  int sigsetmask();
  void initialize_tty_input();
  void reset_tty_input();
  struct sigvec siginfo, saved_siginfo;
  int saved_mask;

  saved_mask = sigsetmask(-1);
  reset_tty_input();
  siginfo.sv_handler = SIG_DFL;
  siginfo.sv_mask = 0;
  siginfo.sv_flags = 0;
  sigvec(SIGTSTP, &siginfo, &saved_siginfo);
  ((void) sigsetmask((~sigmask(SIGTSTP)) &
		     (~sigmask(SIGTERM))));

  if (Debug_Flags[2])
    printf ("Butterfly_suspend_handler: SIGSTP %d -> %d\n", getpid(), getpid());

  kill(getpid(), SIGTSTP);

  /* After SIGCONT */
  ((void) sigsetmask(-1));
  sigvec(SIGTSTP, &saved_siginfo, &siginfo);
  initialize_tty_input();
  ((void) sigsetmask(saved_mask));
  if (!in_input_wait_p)
    return;
  longjmp(input_wait_state, 1);
  /*NOTREACHED*/
}


/* This always causes a ^G interrupt. */

int
butterfly_abort_handler()
{
  static struct character_interrupt_data cid;

  cid.kind = ((int) 'G');
  send_input_data(SIG_CHAR_INT, &cid, sizeof(cid));
  if (!in_input_wait_p)
    return;
  longjmp(input_wait_state, 1);
  /*NOTREACHED*/
}

/* This always causes a ^B interrupt.
   From the new REPL we can always go up, etc.
 */

int
butterfly_breakpoint_handler()
{
  static struct character_interrupt_data cid;

  cid.kind = ((int) 'B');
  send_input_data(SIG_CHAR_INT, &cid, sizeof(cid));
  if (!in_input_wait_p)
    return;
  longjmp(input_wait_state, 1);
  /*NOTREACHED*/
}

/* Interactive handler.

   It calls Ask_Me to dispatch on the input, but does the
   interrupting or dismissing itself.
 */

int
butterfly_interactive_handler(sig, code, scp)
     int sig, code;
     struct sigcontext *scp;
{
  static struct character_interrupt_data cid;
  int val;
  
  suspend_the_lisp_tasks();
  val = Ask_Me(butterfly_tyi_within_interrupt, &cid.kind);
  resume_the_lisp_tasks();
  
  switch(val)
  {
    case INTERACTIVE_INTERRUPT:
      send_input_data(SIG_CHAR_INT, &cid, sizeof(cid));
      break;

    case INTERACTIVE_DISMISS:
      /* Restart the read if in input wait, otherwise it
	 does the right thing too.
       */
      break;

    case INTERACTIVE_RECURSIVE:
      /* This should never happen, the signals are supposedly
	 blocked because of the mask used in the sigvec call.
       */
      break;

    case INTERACTIVE_SUSPEND:
    {
      int sigsetmask();
      boolean saved;
      int mask;

      saved = in_input_wait_p;
      in_input_wait_p = FALSE;
      mask = sigsetmask(-1);
      ((void) sigsetmask(mask & (~sigmask(SIGTSTP))));
      killpg(0, SIGTSTP);

      /* This assumes that the signal is delivered to this
	 process before the following call is made.
       */

      ((void) sigsetmask(mask));
      in_input_wait_p = saved;
      break;
    }

    case INTERACTIVE_EXIT:
      /* This should do it. */
      ((void) exit_input_server());
      /*NOTREACHED*/

    default:
      /* Should this print an error message? */
      break;
  }
  if (in_input_wait_p)
  {
    longjmp(input_wait_state, 1);
    /*NOTREACHED*/
  }
  else
  {
#ifdef butterfly
    if (scp != (struct sigcontext *) NULL) sigreturn(scp);
    /*NOTREACHED*/
#else /* not butterfly */
    return;
#endif
  }
}

int
dummy_handler()
{
  return 0;
}


/* Local input server.

   This procedure manages input and interrupts from the controlling
   terminal.
 */

extern void run_local_input_server();

#define INPUT_BUFLEN		1024

#define my_signal(signame, handler, mask, flags)			\
do									\
{									\
  siginfo.sv_handler = (handler);					\
  siginfo.sv_mask = (mask);						\
  siginfo.sv_flags = (flags);						\
  sigvec((signame), &siginfo, ((struct sigvec *) NULL));		\
} while(0)


void
setup_signal_handlers()
{
  struct sigvec siginfo;

  my_signal(SIGINT, butterfly_interactive_handler, -1, SV_INTERRUPT);
  my_signal(SIGQUIT, butterfly_abort_handler, -1, SV_INTERRUPT);
  my_signal(SIGTSTP, butterfly_suspend_handler, -1, SV_INTERRUPT);

  my_signal(SIGTERM, exit_input_server, -1, SV_INTERRUPT);
  my_signal(SIGUSR1, exit_input_server, -1, SV_INTERRUPT);
  my_signal(SIGUSR2, exit_input_server, -1, SV_INTERRUPT);
}

void
run_local_input_server()
{
  extern int errno;
  struct input_interrupt_data iid;
  char buffer[INPUT_BUFLEN];
  struct sigvec siginfo;
  int nchars;

  /* **************** */

  initialize_tty_input();

  while (TRUE)
    {
      setup_signal_handlers();
      sigpause(0);
    }

  /* **************** */

  /* 
    We wait here for a SIGUSR2 from kick_input_server,
    since we only want inser to start reading when bfio
    is started.
  */

  my_signal(SIGUSR2, dummy_handler, -1, SV_INTERRUPT);

  sigpause(0);

  setup_signal_handlers();

  initialize_tty_input();

  while(TRUE)
  {
    /* Wait for input from tty */

    nchars = -1;
    in_input_wait_p = TRUE;
    if (setjmp(input_wait_state) == 0)
      {
	nchars = read(fileno(stdin), &buffer[0], INPUT_BUFLEN);
	if (Debug_Flags[10])
	  if (nchars >= 0)
	    printf("Inser buffered read: %d %.*s", nchars, nchars, buffer);
      }

    in_input_wait_p = FALSE;

    if (nchars < 0)
    {
      /* Repeat the request if still needed */
      continue;
    }
    iid.length = nchars;
    bcopy(&buffer[0], &iid.data[0], nchars);

    /* Send the data. */

    clear_ext_msg_ack(); /* Be sure no garbage left behind */

    send_input_data(SIG_INPUT_INT,
		    &iid,
		    ((sizeof(iid) - sizeof(iid.data)) + nchars));

    wait_for_ext_msg_ack();
    clear_ext_msg_ack(); /* Clear out for next time in case of interrupt */
  }
}

