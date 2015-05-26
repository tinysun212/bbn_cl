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

/* -*-C-*-

Copyright (c) 1988 Massachusetts Institute of Technology

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

/* 
 * Contains operating system (Unix) dependent procedures. 
 */

#include <sys/types.h>
#include <sys/times.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <signal.h>
#include <errno.h>
extern int errno;
#include "zones.h"

#define SYSTEM_NAME "unix"

#if defined(bsd)
#define HAS_SIGVECTOR
#define sigvector sigvec
#define HAS_DIR
#include <sys/dir.h>
#ifdef butterfly
#include <sys/dirent.h>
#endif
#define HAS_TIMES
#include <sys/timeb.h>
#define HAS_GETTIMEOFDAY
#include <sys/time.h>
#include <sgtty.h>
#ifdef vax
#define SYSTEM_VARIANT "bsd (vax)"
#endif
#ifdef celerity
#define SYSTEM_VARIANT "bsd (Celerity)"
#endif    
#ifdef sun
#define SYSTEM_VARIANT "bsd (sun)"
#include <sys/vadvise.h>
#include <pwd.h>
#endif
#ifdef pyr
#define SYSTEM_VARIANT "bsd (Pyramid)"
#endif
#ifdef butterfly
#define SYSTEM_VARIANT "mach (butterfly)"
#endif
#ifdef alliant
#define SYSTEM_VARIANT "bsd (Alliant)"
#endif
#ifndef SYSTEM_VARIANT
#define SYSTEM_VARIANT "bsd (unknown)"
#endif

#else
#if defined(nu)
#define HAS_TIMES
#include <sys/timeb.h>
#define HAS_GETTIMEOFDAY
#include <time.h>
#include <sgtty.h>
#define SYSTEM_VARIANT "nu (lose)"

#else /* hpux, ATT */
#define HAS_TIMES
#include <time.h>
#include <termio.h>
#include <fcntl.h>
#ifdef system3
#include <mknod.h>
#ifdef hpux
#define HAS_SIGVECTOR
#define HAS_DIR
#include <ndir.h>
/* The hp9000s500 system3 version of ndir defines void! */
#ifdef void
#undef void
#endif
#define SYSTEM_VARIANT "hpux (III)"
#else
#define SYSTEM_VARIANT "AT&T (III)"
#endif
#else	/* Not system 3 below here */
#ifndef spectrum
#include <sys/mknod.h>
#else
#include <sys/sysmacros.h>
#endif
#ifdef hpux
/* Get definition of HZ. It is 50 for Bobcats, and 100 for Indigo's. */
/* There is a bug in the version of param.h for the hp9000s500 that we have. */
#ifndef hp9000s500
#include <sys/param.h>
#endif
/* As of HP-UX version 6.0 we must include <sys/param.h> before <ndir.h>.
   This is because some joker decided to conditionally define DEV_BSIZE
   in <ndir.h>. */
#define HAS_DIR
#include <ndir.h>
#define HAS_GETTIMEOFDAY
#define SYSTEM_VARIANT "hpux (V)"
#else
#define SYSTEM_VARIANT "ATT (V)"
#endif
#endif
#endif
#endif

#ifndef butterfly

#define GUARANTEE_WD()

#else

#define GUARANTEE_WD()							\
{									\
  extern char *bfly_getwd();						\
  char wd_buffer[(FILE_NAME_LENGTH + 2)];				\
									\
  chdir(bfly_getwd(wd_buffer, sizeof(wd_buffer)));			\
}

#endif

#ifdef butterfly

#include "transact.h"

#define open bfly_open
#define close bfly_close
#define lseek bfly_lseek
#define read bfly_read
#define write bfly_write
#define fopen bfly_fopen
#define fclose bfly_fclose
#define fread bfly_fread
#define fwrite bfly_fwrite
#define fseek bfly_fseek
#define getwd bfly_getwd
#define chdir bfly_chdir

#ifdef feof
#undef feof
#endif
#define feof bfly_feof

#ifdef fileno
#undef fileno
#endif
#define fileno bfly_fileno

#ifdef putc
#undef putc
#endif
#define putc bfly_putc

#ifdef putchar
#undef putchar
#endif
#define putchar bfly_putchar

#ifdef ferror
#undef ferror
#endif
#define ferror bfly_ferror

#ifdef clearerr
#undef clearerr
#endif
#define clearerr bfly_clearerr

#ifdef fflush
#undef fflush
#endif
#define fflush bfly_fflush

/* IMPORTANT:
   The following system calls are NOT global (handled by the file server).

   stat
   umask
   mkdir
   
   Any calls to them must be preceeded by GUARANTEE_WD().
 */

extern void Section_Fault();	/* for SIGSEGV */

#define initial_real_time SHARED_DATA->Initial_Real_Time
#define initial_real_millitm SHARED_DATA->Initial_Real_Millitm
#define initial_process_time SHARED_DATA->Initial_Process_Time
#define initial_system_process_time SHARED_DATA->Initial_System_Process_Time

#define SIGNAL_BLOCKED_ON_DELIVERY
#endif

extern Boolean Recover_Automatically;
extern int Debug_Flags[];

extern char *getenv ();
extern char *malloc ();
extern char *tgetstr ();
extern char *tgoto ();

extern long lseek ();

extern void OS_tty_write_char ();

/* Fixnum multiplication */

#ifdef vax

#define Mul_handled

/* Note that "register" is used here (not "fast") since the
   assembly code requires knowledge of the location of
   the variables and they therefore must be in registers.
   This is a kludge.  It depends on what register variables 
   get assigned to what registers.  It should be entirely 
   coded in assembly language.  -- JINX
*/

Pointer
Mul(Arg1, Arg2)
     Pointer Arg1, Arg2;
{
  register long A, B, C;

  Sign_Extend(Arg1, A);
  Sign_Extend(Arg2, B);
  asm("	emul	r11,r10,$0,r10");  /* A is in 11, B in 10 */
  C = A;
  A = B;	/* What is all this shuffling? -- JINX */
  B = C;
  /* B should have high order result, A low order */
  if (((B == 0)  && (A & (-1 << 23)) == 0) ||
      ((B == -1) && (A & (-1 << 23)) == (-1 << 23)))
  {
    return (MAKE_SIGNED_FIXNUM(A));
  }
  else
  {
    return (NIL);
  }
}

#endif

/* 68k family code.  Uses hp9000s200 conventions for the new compiler. */

#if defined(hp9000s200) && !defined(old_cc) && !defined(__GNU__)
#define Mul_handled

/* The following constants are hard coded in the assembly language
 * code below.  The code assumes that d0 and d1 are scratch registers 
 * for the compiler. 
 */

#if (NIL != 0) || (TC_FIXNUM != 0x1A)
#include "Error: types changed.  Change assembly language appropriately"
#endif

#ifdef MC68020

static long Fixnum_Range[2] = {SMALLEST_FIXNUM , BIGGEST_FIXNUM};

	asm("	text");
	asm("	global _Mul");
	asm("_Mul:");
	asm("	bfexts	4(%sp){&8:&24},%d0");
	asm("	bfexts	8(%sp){&8:&24},%d1");
	asm("	muls.l	%d1,%d0");
	asm("	bvs.b	result_is_nil");
	asm("	cmp2.l	%d0,_Fixnum_Range");
	asm("	bcs.b	result_is_nil");
	asm("	moveq	&0x1A,%d1");
	asm("	bfins	%d1,%d0{&0:&8}");
	asm("	rts");
	asm("result_is_nil:");
	asm("	clr.l	%d0");
	asm("	rts");
	asm("	data");

#else	/* not MC68020, but 68k family */

	/* 20(sp) = arg0; 24(sp) = arg1 because of movem */

	asm("	text");
	asm("	global _Mul");
	asm("_Mul:");
	asm("	movem.l	%d2-%d5,-(%sp)");
	asm("	clr.b	%d5");
	asm("	tst.b	21(%sp)");
	asm("	slt	20(%sp)");
	asm("	bge.b	coerce_1");
	asm("	moveq	&1,%d5");
	asm("	neg.l	20(%sp)");

	asm("coerce_1:");
	asm("	tst.b	25(%sp)");
	asm("	slt	24(%sp)");
	asm("	bge.b	after_coerce");
	asm("	eori.b	&1,%d5");
	asm("	neg.l	24(%sp)");
	asm("after_coerce:");
	asm("	move.l	20(%sp),%d0");
	asm("	move.l	24(%sp),%d1");
	asm("	move.w	%d0,%d2");
	asm("	mulu	%d1,%d2");
	asm("	move.w	%d1,%d4");
	asm("	swap	%d1");
	asm("	move.w	%d1,%d3");
	asm("	mulu	%d0,%d3");
	asm("	swap	%d0");
	asm("	mulu	%d0,%d4");
	asm("	add.l	%d4,%d3");
	asm("	bcs.b	result_is_nil");
	asm("	mulu	%d0,%d1");
	asm("	bne.b	result_is_nil");
	asm("	swap	%d2");
	asm("	add.w	%d3,%d2");
	asm("	bcs.b	result_is_nil");
	asm("	swap	%d3");
	asm("	tst.w	%d3");
	asm("	bne.b	result_is_nil");
	asm("	cmpi.w	%d2,&0x7F");
	asm("	bgt.b	result_is_nil");
	asm("	swap	%d2");
	asm("	tst.b	%d5");
	asm("	beq.b	sign_is_right");
	asm("	neg.l	%d2");
	asm("sign_is_right:");
	asm("	move.l	%d2,-(%sp)");
	asm("	move.b	&0x1A,(%sp)");
	asm("	move.l	(%sp)+,%d0");
	asm("	movem.l	(%sp)+,%d2-%d5");
	asm("	rts");
	asm("result_is_nil:");
	asm("	clr.l	%d0");
	asm("	movem.l	(%sp)+,%d2-%d5");
	asm("	rts");
	asm("	data");

#endif	/* not MC68020 */
#endif  /* hp9000s200 */

#ifndef Mul_handled
/* table case */
#include "mul.c"
#endif

#if defined(TCFLSH)	/* hpux, ATT */
#define TIOCFLUSH	TCFLSH
#endif

#ifndef FREAD
#define FREAD		0
#endif

/* OpSys dependent I/O operations. */

/* First flush the buffered input,
   then tell the OS to flush the pending input.
*/

#define flush_input_buffer()						\
{									\
  int flags;								\
									\
  flags = FREAD;							\
  (stdin)->_cnt = 0;							\
  ioctl( fileno( stdin), TIOCFLUSH, &flags);				\
}

/* This assumes ASCII */

void
print_character_name(the_char, long_p)
     unsigned char the_char;
     Boolean long_p;
{
  if (the_char <= 040)
  {
    printf("`^%c'", (the_char + 0100));
    if (long_p)
    {
      printf(" (control-%c)", (the_char + 0100));
    }
  }
  else
  {
    printf("`%c'", the_char);
  }
  return;
}

/* File I/O Primitives */

static int
my_stat (path, buf)
     char *path;
     struct stat *buf;
{
  fast int result;

  while (1)
  {
    errno = 0;
    result = (stat (path, buf));
    if (result == 0)
      break;
    if (errno != EINTR)
      break;
  }
  return (result);
}

static FILE *
my_fopen (file_name, type)
     char *file_name;
     char *type;
{
  fast FILE *result;
  extern FILE *fopen ();

  while (1)
    {
      errno = 0;
      result = (fopen (file_name, type));
      if (result != NULL)
	break;
      if (errno != EINTR)
	break;
    }
  return (result);
}

static int
my_fread (ptr, size, nitems, stream)
     char *ptr;
     int size;
     int nitems;
     fast FILE *stream;
{
  fast int n_left;
  fast int n_read;
  extern int fread ();

  n_left = nitems;
  while (n_left > 0)
    {
      errno = 0;
      n_read = (fread (ptr, size, n_left, stream));
      if (n_read > 0)
	{
	  n_left -= n_read;
	  ptr += (n_read * size);
	}
      if (feof (stream))
	break;
      if (ferror (stream))
	{
	  if (errno != EINTR)
	    break;
	  clearerr (stream);
	}
    }
  return (nitems - n_left);
}

static int
my_fwrite (ptr, size, nitems, stream)
     char *ptr;
     int size;
     int nitems;
     fast FILE *stream;
{
  fast int n_left;
  fast int n_written;
  extern int fwrite ();

  n_left = nitems;
  while (n_left > 0)
    {
      errno = 0;
      n_written = (fwrite (ptr, size, n_left, stream));
      if (n_written > 0)
	{
	  n_left -= n_written;
	  ptr += (n_written * size);
	}
      if (ferror (stream))
	{
	  if (errno != EINTR)
	    break;
	  clearerr (stream);
	}
    }
  return (nitems - n_left);
}

static int
my_fflush (stream)
     fast FILE *stream;
{
  fast int result;
  extern int fflush ();

  while (1)
    {
      clearerr (stream);
      errno = 0;
      result = (fflush (stream));
      if (result == 0)
	break;
      if ((ferror (stream)) && (errno != EINTR))
	break;
    }
  return (result);
}

static int
my_fclose (stream)
     fast FILE *stream;
{
  fast int result_fflush;
  fast int result_fclose;
  extern int fclose ();

  result_fflush = (my_fflush (stream));
  result_fclose = (fclose (stream));
  return ((result_fflush == 0) ? result_fclose : result_fflush);
}

/* Binary file I/O Operations */

extern long Load_Data(), Write_Data();
extern Boolean Open_Dump_File(), Close_Dump_File();
extern int Open_Dump_File_Fd(), Close_Dump_File_Fd();

static int File_Des;

void set_file_des(fd)
int fd;
{
  File_Des = fd;
}

long get_file_des()
{
  return File_Des;
}

Boolean
Open_Dump_File( Name, flag)
     Pointer Name;
     char *flag;
{
  int status;

  File_Des =
    OS_open_dump_file_fd(Scheme_String_To_C_String(Name), flag, &status);
  return (File_Des != -1);
}

int
OS_open_dump_file_fd(name, write_flag, status)
     char *name;
     char *write_flag;
     int *status;
{
  int fd,open_mode;

  errno = 0;
  if (*write_flag == 'w')
  {
    open_mode = O_WRONLY + O_TRUNC + O_CREAT;
    GUARANTEE_WD();
    unlink(name);
  }
  else
    open_mode = O_RDONLY;
  fd = open(name,open_mode,0666);
  *status = errno;
  return (fd);
}

Boolean
Close_Dump_File()
{
  int status;

  return (OS_close_dump_file_fd(File_Des,&status) != -1);
}

int
OS_close_dump_file_fd(Fd,status)
int Fd;
int *status;
{
  int close_val;

  errno = 0;
  close_val = close(Fd);
  *status = errno;
  return(close_val);
}

long
Load_Data(Count, To_Where)
     long Count;
     char *To_Where;
{
  long nbytes_or_status;

  errno = 0;
  nbytes_or_status = read(File_Des,To_Where,sizeof(Pointer)*Count);
  if (nbytes_or_status == -1)
    perror("Load_Data: ");
  if (nbytes_or_status > 0)
    nbytes_or_status = (nbytes_or_status / sizeof(Pointer));
#if false
  printf("load_data: nbytes_or_status: %d, count: %d, bytes: %d\n",
	 nbytes_or_status,Count,sizeof(Pointer)*Count);
#endif
  return nbytes_or_status;
}

long
Write_Data( Count, From_Where)
     long Count;
     char *From_Where;
{
  long nbytes_or_status;

  nbytes_or_status = write(File_Des,From_Where,sizeof(Pointer)*Count);
  if (nbytes_or_status > 0)
    nbytes_or_status = (nbytes_or_status / sizeof(Pointer));
#if false
  printf("write_data: nbytes_or_status: %d, count: %d, bytes: %d\n",
	 nbytes_or_status,Count,sizeof(Pointer)*Count);
#endif
  return nbytes_or_status;
}

void
unread_dump_file_header()
{
  lseek(File_Des,-(sizeof(Pointer)*FASL_HEADER_LENGTH),1);
}

Boolean 
OS_eof_p(fd)
     int fd;
{
  long status;
  char c;

  status = read(fd,&c,1);
  if (status == -1)
    return false;
  if (status == 0)
    return true;
  lseek(fd,-1,1);
  return false;
}

long
OS_dump_file_seek(fd,object_no,status)
     int fd;
     int object_no;
     int *status;
{
  long i;

  lseek(fd,0,0);  /* seek to beginning of file */
  set_file_des(fd);
  for (i=0; i != object_no; i++)
  {
    if (OS_eof_p(fd))
      return 0;
    Read_Header();
    lseek(fd,Data_Length_In_Bytes(),1);
  }
  return 1;
}

/*
  Inquire position.
*/

long 
OS_dump_file_pos(fd, status)
     int fd;
     int *status;
{
  long pos;

  errno = 0;
  pos = lseek(fd,0,1);
  *status = errno;
  return pos;   /* will be -1 if error */
}


/* File I/O Operations */

FILE *
OS_file_open( name, output_p)
     char *name;
     Boolean output_p;
{
  struct stat file_stat;

  if (output_p)
  {
    GUARANTEE_WD();
    /* Delete existing file before opening new one.
       This prevents writing through hard links.
       Do not signal error if the unlink fails,
       because the fopen might still work. */
    if ((my_stat (name, (& file_stat))) == 0)
      unlink (name);
    return (my_fopen (name, "w"));
  }
  else
    return (my_fopen (name, "r"));
}

Boolean
OS_file_close( stream)
     FILE *stream;
{
  return ((my_fclose (stream)) == 0);
}

Boolean
OS_file_eof_p( stream)
     FILE *stream;
{
  return (feof( stream));
}

long
OS_file_length_fd(file_descriptor)
     long file_descriptor;
{
  long current_position, result;
  extern long lseek();

  current_position = lseek( file_descriptor, 0, 1);
  result = lseek( file_descriptor, 0, 2);
  if (current_position != lseek( file_descriptor, current_position, 0))
    error_external_return ();
  return (result);
}

long
OS_file_length(stream)
FILE *stream;
{
  return OS_file_length_fd(fileno(stream));
}


/* Three-valued; Lisp may need this smartness someday:
     -1   Does not exist.
      0   Don't know.
      1   Exists. */

extern int file_exists();

int
file_exists (name)
     char *name;
{
  struct stat file_stat;
  int status;

  errno = 0;
  if ((my_stat (name, (& file_stat))) == 0)
    return (1);
  status = errno;
  if ((status == EACCES) || (status == EIO))
    return (0);
  return (-1);
}

int
OS_file_existence_test (name)
     char *name;
{
  GUARANTEE_WD();
  return (file_exists (name));
}

long
OS_file_read_chars( stream, buffer, nchars)
     FILE *stream;
     char *buffer;
     long nchars;
{
  return (my_fread( buffer, 1, nchars, stream));
}

Boolean
OS_file_write_chars (stream, buffer, nchars)
     FILE *stream;
     char *buffer;
     long nchars;
{
  return ((my_fwrite (buffer, 1, nchars, stream)) == nchars);
}

/* file_type_letter

   file_type_letter accepts a file status block and returns a
   character code describing the type of the file.  'd' is returned
   for directories, 'b' for block special files, 'c' for character
   special files, 'm' for multiplexor files, 'l' for symbolic link,
   's' for socket, 'p' for fifo, '-' for any other file type.

   Taken from the GNU Emacs source code. */

char
file_type_letter (s)
   struct stat *s;
{
  switch ((s -> st_mode) & S_IFMT)
    {
    case S_IFDIR:
      return ('d');
#ifdef S_IFLNK
    case S_IFLNK:
      return ('l');
#endif
#ifdef S_IFCHR
    case S_IFCHR:
      return ('c');
#endif
#ifdef S_IFBLK
    case S_IFBLK:
      return ('b');
#endif
#ifdef S_IFMPC
/* These do not seem to exist */
    case S_IFMPC:
    case S_IFMPB:
      return ('m');
#endif
#ifdef S_IFSOCK
    case S_IFSOCK:
      return ('s');
#endif
#ifdef S_IFIFO
    case S_IFIFO:
      return ('p');
#endif
#ifdef S_IFNWK /* hp-ux hack */
    case S_IFNWK:
      return ('n');
#endif
    default:
      return ('-');
    }
}

/* Working Directory */

#ifdef system3

static char *
getcwd (buffer, length)
     char *buffer;
     int length;
{
  FILE *the_pipe;
  char *finder;

  /* Allocate the buffer if needed. */
  if (buffer == NULL)
    {
      buffer = (malloc (length));
      if (buffer == NULL)
	return (NULL);
    }

  /* Invoke `pwd' and fill the buffer with its output. */
  the_pipe = (popen ("pwd", "r"));
  if (the_pipe == NULL)
    return (NULL);
  fgets (buffer, length, the_pipe);
  pclose (the_pipe);

  /* Remove extraneous newline. */
  finder = buffer;
  while (true)
    {
      if ((*finder) == '\n')
	{
	  (*finder) = '\0';
	  break;
	}
      else if ((*finder++) == '\0')
	break;
    }
  return (buffer);
}

#else

#if defined(sun) || defined(bsd)
/* This will work for the cases we care about. */
#define getcwd getwd
#endif
extern char *getcwd ();

#endif

static Pointer
home_directory_pathname ()
{
  char *path;

  path = (getenv ("HOME"));
  return ((path == NULL) ? NIL : (C_String_To_Scheme_String (path)));
}

Pointer
OS_working_dir_pathname ()
{
  char path[(FILE_NAME_LENGTH + 2)];

  return
    (((getcwd (path, (FILE_NAME_LENGTH + 2))) == NULL)
     ? (home_directory_pathname ())
     : (C_String_To_Scheme_String (path)));
}

Boolean
OS_set_working_dir_pathname (name)
     char *name;
{
  return ((chdir (name)) == 0);
}

/* File System Operations */

Boolean
OS_file_remove (name)
     char *name;
{
  GUARANTEE_WD();
  return ((unlink (name)) == 0);
}

Boolean
OS_file_link_physical (old_name, new_name)
     char *old_name, *new_name;
{
  GUARANTEE_WD();
  return ((link (old_name, new_name)) == 0);
}

Boolean
OS_file_link_symbolic (old_name, new_name)
     char *old_name, *new_name;
{
#ifdef S_IFLNK
  GUARANTEE_WD();
  return ((symlink (old_name, new_name)) == 0);
#else
  return (false);
#endif
}

/* Moves the file from OLD-NAME to NEW-NAME.  Simply reduces to a
   hard-link then a delete file; should be fixed for cross-structure
   rename. */

Boolean
OS_file_rename (old_name, new_name)
     char *old_name, *new_name;
{
  GUARANTEE_WD();
  return
    (((link (old_name, new_name)) != 0)
     ? false
     : ((unlink (old_name)) == 0));
}

#define file_copy_finish(result)					\
{									\
  if (! (OS_file_close (source_file)))					\
    error_external_return ();						\
  if (! (OS_file_close (destination_file)))				\
    error_external_return ();						\
  return (result);							\
}

#define file_copy_1(nchars)						\
{									\
  if ((OS_file_read_chars (source_file, buffer, (nchars))) < (nchars))	\
    {									\
      file_copy_finish (false);						\
    }									\
  if (! (OS_file_write_chars (destination_file, buffer, (nchars))))	\
    {									\
      file_copy_finish (false);						\
    }									\
}

/* An arbitrary length -- could use `malloc' and compute

   file_copy_buffer_length = (((ulimit (3, 0)) - (sbrk (0))) / 2);

   but that is hairy and might not be easy to port to various unices.
   This should be adequate and hopefully will perform better than
   single-character buffering. */

#define file_copy_buffer_length 8192

Boolean
OS_file_copy (source_name, destination_name)
     char *source_name, *destination_name;
{
  FILE *source_file, *destination_file;
  long source_length, buffer_length;
  char buffer[file_copy_buffer_length];

  source_file = (OS_file_open (source_name, false));
  if (source_file == NULL)
    return (false);
  destination_file = (OS_file_open (destination_name, true));
  if (destination_file == NULL)
    {
      if (! (OS_file_close (source_file)))
	error_external_return ();
      return (false);
    }
  source_length = (OS_file_length (source_file));
  buffer_length =
    ((source_length < file_copy_buffer_length)
     ? source_length
     : file_copy_buffer_length);
  while (source_length > buffer_length)
    {
      file_copy_1 (buffer_length);
      source_length -= buffer_length;
    }
  file_copy_1 (source_length);
  file_copy_finish (true);
}

Boolean
OS_directory_make (name)
     char *name;
{
  int old_umask;
  Boolean result;

  GUARANTEE_WD();
  old_umask = (umask (0));
#ifdef bsd
  result = ((mkdir (name, 511)) == 0);
#else
  result = ((mknod (name, 0040666, ((dev_t) 0))) == 0);
#endif
  umask (old_umask);
  return (result);
}

#ifndef HAS_DIR

Pointer
OS_directory_open (name)
     char *name;
{
  return (NIL);
}

Pointer
OS_directory_read ()
{
  error_external_return ();
}

#else /* has directory library */

static DIR *directory_pointer = NULL;
#ifdef butterfly
static struct dirent *directory_entry = NULL;
#else
static struct direct *directory_entry = NULL;
#endif

#define read_directory_entry()						\
{									\
  directory_entry = (readdir (directory_pointer));			\
  if (directory_entry == NULL)						\
    {									\
      closedir (directory_pointer);					\
      directory_pointer = NULL;						\
      return (NIL);							\
    }									\
  return (C_String_To_Scheme_String (directory_entry -> d_name));	\
}

Pointer
OS_directory_open (name)
     char *name;
{
  if (directory_pointer != NULL)
    error_external_return ();
  GUARANTEE_WD();
  directory_pointer = (opendir (name));
  if (directory_pointer == NULL)
    return (NIL);
  read_directory_entry ();
}

Pointer
OS_directory_read ()
{
  if (directory_pointer == NULL)
    error_external_return ();
  read_directory_entry ();
}
#endif

/* Terminal hacking. */

static char stdin_file_type;
static Boolean
  stdin_is_a_kbd,
  stdout_is_a_crt,
  Under_Emacs,
  interactive_p;

#define STDIN_IS_A_FILE() (stdin_file_type == '-')

forward int TYI_Immediate ();
forward int TYI_Buffered ();
forward void OS_Re_Init ();
forward void OS_Quit ();
forward long OS_process_clock ();

int
OS_tty_tyi (Immediate, Interrupted)
     Boolean Immediate, *Interrupted;
{
  fast int C;

  if (stdin_is_a_kbd)
  {
    C =
      (Immediate
       ? (TYI_Immediate (Interrupted))
       : (TYI_Buffered (Interrupted)));
  }
  else if (! Under_Emacs)
  {
    (*Interrupted) = false;
    C = getchar();
    if (Debug_Flags[1])
      printf ("Tyi process %d char %c\n", getpid(), C);
    if (C == EOF)
    {
      Microcode_Termination (TERM_EOF);
    }
/*
    if (STDIN_IS_A_FILE ())
    {
      OS_tty_write_char (C);
    }
*/
  }
  else
  {
    C = (TYI_Buffered (Interrupted));
  }
  return (C);
}

#define TTY_READ_CHAR_BODY(immediate)					\
  int chr;								\
									\
  while (true)								\
    {									\
      Boolean Interrupted;						\
									\
      chr = (OS_tty_tyi ((immediate), (& Interrupted)));		\
      if (Interrupted)							\
	{								\
	  if (INTERRUPT_PENDING_P (INT_Mask))				\
	    {								\
	      Primitive_Interrupt ();					\
	    }								\
	}								\
      else								\
	{								\
	  return ((char) chr);						\
	}								\
    }

char
OS_tty_read_char ()
{ TTY_READ_CHAR_BODY (false) }

char
OS_tty_read_char_immediate ()
{ TTY_READ_CHAR_BODY (true) }

void
OS_tty_write_char (chr)
     char chr;
{
  char buffer [1];

  (buffer [0]) = chr;
  (void) my_fwrite (buffer, 1, 1, stdout);
  return;
}

Boolean
OS_tty_write_chars (string, string_length)
     char *string;
     long string_length;
{
  return ((my_fwrite (string, 1, string_length, stdout)) == string_length);
}

void
OS_Flush_Output_Buffer()
{
  my_fflush (stdout);
}

void
OS_Flush_Input_Buffer()
{
  flush_input_buffer();
}

char *CM, *BC, *UP, *CL, *CE, *term;
int LI, CO;

static Boolean Can_Do_Cursor;	/* Initialized below. */

Boolean
OS_Clear_Screen()
{
  if (Can_Do_Cursor)
    tputs (CL, LI, OS_tty_write_char);
  else
    OS_tty_write_char ('\f');
}

Boolean
OS_tty_move_cursor( x, y)
     long x, y;
{
  if (Can_Do_Cursor)
    tputs (tgoto (CM, x, y), 1, OS_tty_write_char);
  return (Can_Do_Cursor);
}

/* Maybe sometime this should be upgraded to use termcap too. */

void
OS_tty_beep()
{
  OS_tty_write_char (BELL);
  return;
}

void
OS_tty_newline()
{
  OS_tty_write_char ('\n');
  return;
}

/* Not currently implemented. */

Boolean
OS_tty_get_cursor (x, y)
     long *x, *y;
{
  *x = 0;
  *y = 0;
  return (false);
}

long
NColumns ()
{
  return (Can_Do_Cursor ? CO : 79);
}

long
NLines ()
{
  return (Can_Do_Cursor ? LI : 24);
}

Boolean
OS_Clear_To_End_Of_Line ()
{
  if (Can_Do_Cursor)
    tputs (CE, 1, OS_tty_write_char);
  return (Can_Do_Cursor);
}

/* GNU Emacs interface hackery */

#define emacs_message(mess)						\
{									\
  printf (mess);							\
  my_fflush (stdout);							\
}

#define SCHEME_ENTER_INPUT_WAIT		"\033s"
#define SCHEME_EXIT_INPUT_WAIT		"\033f"

Boolean
OS_Under_Emacs ()
{
  return (Under_Emacs);
}

/* Process Clock */

#ifdef HAS_TIMES

#ifdef hpux

#ifndef HZ
#define HZ 60
#endif

#else /* not hpux */

#define HZ 60

#endif /* hpux */

#ifndef butterfly
static long initial_process_time;
static long initial_system_process_time;
#endif

static long idle_time; /* updated by the amount of time a processor has to wait for work */
static long start_idle_time; 

static void
initialize_process_clock ()
{
  struct tms time_buffer;

#ifdef butterfly
  initial_process_time = 0;
  initial_system_process_time = 0;
#else
  times (& time_buffer);
  initial_process_time = (((time_buffer . tms_utime) * 1000) / HZ);
  initial_system_process_time = (((time_buffer . tms_stime) * 1000) / HZ);
#endif
  idle_time = 0;
  start_idle_time = 0;
  return;
}

long
OS_process_clock ()
{
  struct tms time_buffer;

  times (& time_buffer);
  return ((((time_buffer . tms_utime) * 1000) / HZ) - initial_process_time);
}

long
OS_system_process_clock ()
{
  struct tms time_buffer;

  times (& time_buffer);
  return ((((time_buffer . tms_stime) * 1000) / HZ) - initial_system_process_time);
}

void
OS_start_idle_time()
{
  start_idle_time = OS_process_clock();
}

void
OS_end_idle_time()
{
  idle_time += OS_process_clock() - start_idle_time;
}

long
OS_idle_time()
{
  return idle_time;
}


#else /* not HAS_TIMES */

/* Note: These cannot cause errors because the garbage collector wrapper */
   and other system utilities use them.  */

static void
initialize_process_clock ()
{
  return;
}

long
OS_process_clock ()
{
  return (0);
}

#endif /* HAS_TIMES */

/* Real Time Clock */

#ifdef HAS_GETTIMEOFDAY

#ifndef butterfly
static struct timeval initial_real_time;
#endif

static void
initialize_real_time_clock ()
{
  struct timezone time_zone;
  struct timeval initial_time;

  gettimeofday ((& initial_time), (& time_zone));
#ifdef butterfly
  SHARED_DATA->Initial_Real_Time = initial_time.tv_sec;
  SHARED_DATA->Initial_Real_Millitm = initial_time.tv_usec;
#else
  initial_real_time.tv_sec = initial_time.tv_sec;
  initial_real_time.tv_usec = initial_time.tv_usec;
#endif
  return;
}

long
OS_real_time_clock ()
{
  struct timeval current_real_time;
  struct timezone time_zone;

  gettimeofday ((& current_real_time), (& time_zone));
  return
#ifdef butterfly
    ((((current_real_time . tv_sec) - SHARED_DATA->Initial_Real_Time) * 1000) +
     (((current_real_time . tv_usec) - SHARED_DATA->Initial_Real_Millitm) / 1000));
#else
    ((((current_real_time . tv_sec) - (initial_real_time . tv_sec)) * 1000) +
     (((current_real_time . tv_usec) - (initial_real_time . tv_usec)) / 1000));
#endif
}

#else /* not HAS_GETTIMEOFDAY */
#ifdef HAS_TIMES

#ifndef butterfly
static long initial_real_time;
#endif

static void
initialize_real_time_clock ()
{
  struct tms time_buffer;

  initial_real_time = (times (& time_buffer));
  return;
}

long
OS_real_time_clock ()
{
  struct tms time_buffer;

  return ((times (& time_buffer)) - initial_real_time);
}

#else /* not HAS_TIMES */

static void
initialize_real_time_clock ()
{
  return;
}

long
OS_real_time_clock ()
{
  return (0);
}

#endif /* HAS_TIMES */
#endif /* HAS_GETTIMEOFDAY */

long
System_Clock ()
{
  struct tms buff;

  times (&buff);
  return ((100 * (buff.tms_utime - initial_process_time)) / 60);
}

#define OS_time_mark(delta) ((OS_real_time_clock ()) + (delta))

/* Time and dates. */

#if defined(bsd) || defined(hpux) || defined(nu)

extern struct tm *(localtime());

#define Date_Part(C_Name, Which)	\
int					\
C_Name()				\
{					\
  struct tm *Time;			\
  long The_Time;			\
					\
  time(&The_Time);			\
  Time = localtime(&The_Time);		\
  return (Time->Which);			\
}

#else

#define Date_Part(C_Name, ignore)	\
int					\
C_Name()				\
{					\
  return -1;				\
}

#endif

Date_Part(OS_Current_Year, tm_year);
Date_Part(OS_Current_Month, tm_mon + 1);
Date_Part(OS_Current_Day, tm_mday);
Date_Part(OS_Current_Hour, tm_hour);
Date_Part(OS_Current_Minute, tm_min);
Date_Part(OS_Current_Second, tm_sec);

/* Timers (for timer interrupts) */

#if defined(ITIMER_VIRTUAL)
void
Clear_Int_Timer()
{
  struct itimerval New_Value, Old_Value;

  New_Value.it_value.tv_sec = 0;
  New_Value.it_value.tv_usec = 0;

  /* The following two are not necessary according to the
     documentation, but there seems to be a bug in BSD, at least on
     Suns.
   */

  New_Value.it_interval.tv_sec = 0;
  New_Value.it_interval.tv_usec = 0;
  setitimer(ITIMER_REAL, &New_Value, &Old_Value);
  setitimer(ITIMER_VIRTUAL, &New_Value, &Old_Value);
  CLEAR_INTERRUPT(INT_Timer);
  return;
}

void
Set_Int_Timer(Days, Centi_Seconds)
     long Days, Centi_Seconds;
{
  struct itimerval New_Value, Old_Value;
  long Which_Timer = ITIMER_VIRTUAL;

  Clear_Int_Timer();
  if (Centi_Seconds < 0)
  {
    Centi_Seconds = -Centi_Seconds;
    Which_Timer = ITIMER_REAL;
  }
  New_Value.it_value.tv_sec =
    (Days * 24 * 60 * 60 * 60) + (Centi_Seconds / 100);
  New_Value.it_value.tv_usec = (Centi_Seconds % 100) * 10000;
  New_Value.it_interval.tv_sec = 0;	/* Turn off after it rings */
  New_Value.it_interval.tv_usec = 0;
  setitimer(Which_Timer, &New_Value, &Old_Value);
  CLEAR_INTERRUPT(INT_Timer);
  return;
}

#else
void
Clear_Int_Timer()
{
  Primitive_Error( ERR_UNIMPLEMENTED_PRIMITIVE);
}

void
Set_Int_Timer(days, centi_seconds)
     long days, centi_seconds;
{
  Primitive_Error( ERR_UNIMPLEMENTED_PRIMITIVE);
}
#endif

/* Keyboard I/O and interrupts */

#if defined(CBREAK)	/* bsd */

#define Immediate_Declarations()					\
struct sgttyb TTY_Block

#define Immediate_Prolog(only_polling)					\
{									\
  gtty(fileno(stdin), &TTY_Block);					\
  TTY_Block.sg_flags |= CBREAK;						\
  stty(fileno(stdin), &TTY_Block);					\
}

#define Immediate_Epilog()						\
{									\
  TTY_Block.sg_flags &= ~CBREAK;					\
  stty(fileno(stdin), &TTY_Block);					\
}

#else
#if defined(TCFLSH)	/* hpux, ATT */

#ifndef VINTR
#define VINTR	  	0
#define VQUIT	  	1
#define VEOF		4
#define VMIN		4
#define VTIME		5
#endif

#define Immediate_Declarations()					\
struct termio The_Chars;						\
char Old_VMIN, Old_VTIME;						\
unsigned short lflag

#define Immediate_Prolog(only_polling)					\
{									\
  ioctl(fileno(stdin), TCGETA, &The_Chars);				\
  lflag = The_Chars.c_lflag;						\
  The_Chars.c_lflag &= ~(ICANON | ECHO);				\
  Old_VMIN = The_Chars.c_cc[VMIN];					\
  Old_VTIME = The_Chars.c_cc[VTIME];					\
  if (only_polling)							\
  {									\
    The_Chars.c_cc[VMIN] = (char) 0;					\
    The_Chars.c_cc[VTIME] = (char) 0;					\
  }									\
  else									\
  {									\
    The_Chars.c_cc[VMIN] = (char) 1; /* Min # of chars. */		\
    The_Chars.c_cc[VTIME] = (char) 1; /* Timeout in 1/10 sec. */	\
  }									\
  ioctl(fileno(stdin), TCSETA, &The_Chars);				\
}

#define Immediate_Epilog()						\
{									\
  The_Chars.c_cc[VMIN] = Old_VMIN;					\
  The_Chars.c_cc[VTIME] = Old_VTIME;					\
  The_Chars.c_lflag = lflag;						\
  ioctl(fileno(stdin), TCSETA, &The_Chars);				\
}

#else	/* ??? */
/* No immediate IO */

#define Immediate_Declarations()
#define Immediate_Prolog(only_polling)
#define Immediate_Epilog()

#endif
#endif

/* These are pretty trivial */

#define Buffered_Declarations()
#define Buffered_Prolog()
#define Buffered_Epilog()

/* Keyboard Interrupts */

/* Scheme interrupt key codes */

#define CONTROL_B	'B'
#define CONTROL_C	'C'
#define CONTROL_G	'G'
#define CONTROL_U	'U'
#define CONTROL_X	'X'

#define DISABLE_EOF	-1

/* Unix interrupt key codes */

#define CONTROL_BIT	0100

#define C_B		(CONTROL_B - CONTROL_BIT)
#define C_C		(CONTROL_C - CONTROL_BIT)
#define C_G		(CONTROL_G - CONTROL_BIT)
#define C_U		(CONTROL_U - CONTROL_BIT)
#define C_X		(CONTROL_X - CONTROL_BIT)

#define DEFAULT_SIGINT_CHAR	C_G
#define DEFAULT_SIGQUIT_CHAR	C_C

/*
  sigint_char is the character on which Unix will send a SIGINT signal.
  sigquit_char is the character on which Unix will send a SIGQUIT signal.
  Int_Char is the scheme interrupt key code to be processed next.
 */

static char
  sigint_char = DEFAULT_SIGINT_CHAR,
  sigquit_char = DEFAULT_SIGQUIT_CHAR;

#ifdef butterfly

/* Int_State is a kludge to avoid a pending interrupt queue,
   which is the right thing.
 */

char Int_Char;
char Int_String[256];
int Int_Stream;
int Int_State;

#else /* not butterfly */

static char Int_Char;

#endif /* butterfly */


#define SET_INT_CHAR(character)						\
{									\
  if (Int_Char == '\0')							\
  {									\
    Int_Char = ((char) character);					\
    REQUEST_INTERRUPT(INT_Character);					\
  }									\
}

int
OS_Get_Next_Interrupt_Character()
{
  int result;

  if (Int_Char == '\0')
  {
    return (-1);
  }
  result = ((int) Int_Char);
  Int_Char = '\0';
#ifdef butterfly
  Int_State &= (~INT_STATE_CHAR);
#endif
  return (result);
}

/* OS_Clean_Interrupt_Channel is used to clear the input buffer when a
   character interrupt is received.  On most systems this is not
   currently used, but the Emacs interface under hp-ux needs some
   assistance.  Normally this is used in conjunction with some kind of
   distinguished marker in the input stream which indicates where each
   interrupt occurred.

   The `mode' argument allows the following values: */

/* This mode indicates that the input buffer should be flushed up to
   and including the most recent interrupt marker. */
#define UNTIL_MOST_RECENT_INTERRUPT_CHARACTER	0

/* This mode indicates that all interrupts which match
   `interrupt_char' should be removed from the input buffer.  Any
   other interrupts should be left alone. */
#define MULTIPLE_COPIES_ONLY			1

Boolean
OS_Clean_Interrupt_Channel (mode, interrupt_char)
     int mode, interrupt_char;
{

#if defined(hpux)
  if (Under_Emacs && (mode == UNTIL_MOST_RECENT_INTERRUPT_CHARACTER))
    while ((OS_tty_read_char_immediate ()) != '\0')
      ;
#endif

#if defined(butterfly)
  if (mode == UNTIL_MOST_RECENT_INTERRUPT_CHARACTER)
  {
    static char buffer[3];

    /* Flush typeahead */

    Int_State &= (~INT_STATE_INPUT);
    if (Int_State == 0)
      IntCode &= ~INT_Character;

#if false

    buffer[0] = FLUSH_REQ;
    buffer[1] = 0xff;
    buffer[2] = 0xff;
    write(SHARED_DATA->Lisp_Output_Pipe[WRITE_TO_PIPE],
	  buffer, 3);
#endif /* false */
  }
#endif /* butterfly */
  return (true);
}

/* Keyboard Interrupts and I/O synchronization */

#define FIRST_TIME	0
#define INTERRUPT	1
#define REDO		2

/* Why is this only looking at certain interrupts? -- CPH */

#define Interrupt_available						\
(INTERRUPT_PENDING_P(INT_Character | INT_Timer | INT_Suspend))

typedef struct { Boolean in_input_wait;
		 jmp_buf storage;
	       } reader_context;

static reader_context real_read_env, *saved_read_env;
  
#define Keyboard_Input_Procedure(Name, decl, prolog, epilog)		\
int									\
Name(Interrupted)							\
Boolean *Interrupted;							\
{									\
  int Which_Way;							\
  int C;								\
  decl;									\
									\
  Which_Way = setjmp(saved_read_env->storage);				\
  while(true)								\
  { switch (Which_Way)							\
    { case FIRST_TIME:							\
      		 prolog;						\
      case REDO:							\
		 saved_read_env->in_input_wait = true;			\
		 if (!Interrupt_available)				\
		 {							\
		   C = getchar();					\
		   saved_read_env->in_input_wait = false;		\
		   epilog;						\
		   if (C == EOF)					\
		   {							\
		     Microcode_Termination(TERM_EOF);			\
		   }							\
		   *Interrupted = false;				\
		   return C;						\
		 }							\
									\
      case INTERRUPT:							\
		 saved_read_env->in_input_wait = false;			\
		 epilog;						\
		 *Interrupted = true;					\
      		 return EOF;						\
      default:	 continue;						\
    }									\
  }									\
}

Keyboard_Input_Procedure(TYI_Immediate,
			 Immediate_Declarations(),
			 Immediate_Prolog(false),
			 Immediate_Epilog())

Keyboard_Input_Procedure(TYI_Buffered,
			 Buffered_Declarations(),
			 Buffered_Prolog(),
			 Buffered_Epilog())


/* Keyboard test will need to be considerably haired up to make it portable.
   See the file `src/keyboard.c' in GNU Emacs for the details.
   Who knows what magic VMS will require to perform this. */

#if defined(FIONREAD) && !defined(hp9000s200)

Boolean
are_there_characters_ready()
{
  long temp;

  if (((stdin)->_cnt) > 0)
    return true;
  if (ioctl( fileno( stdin), FIONREAD, &temp) < 0)
    return false;
  return (temp > 0);
}

#else
#if defined(TCFLSH) && !defined(hp9000s500)

Boolean
are_there_characters_ready()
{
  int result;

  result = (getchar ());
  if (result < 0)
    return (false);
  ungetc (result, stdin);
  return (true);
}

#else
/* Unknown... no characters ready. */

#define are_there_characters_ready() false
#endif
#endif

Boolean
OS_read_char_ready_p (delay)
     long delay;
{
  long limit;
  Boolean result;
  Immediate_Declarations ();

  Immediate_Prolog (true);
  limit = (OS_time_mark (delay));
  while (true)
    {
      if (stdin_is_a_kbd && (are_there_characters_ready ()))
	{
	  result = true;
	  break;
	}
      if ((OS_time_mark (0)) >= limit)
	{
	  result = false;
	  break;
	}
    }
  Immediate_Epilog ();
  return (result);
}

/* Interrupt Handlers. Utility definitions. */

typedef int (*signal_handler)();

#define save_read_context(extra)				\
reader_context next_buffer, *old_env;				\
extra;								\
next_buffer.in_input_wait = false;				\
old_env = saved_read_env;					\
saved_read_env = &next_buffer

#define restore_read_context(extra, action)			\
if (old_env->in_input_wait)					\
{								\
  old_env->in_input_wait = false;				\
  saved_read_env = old_env;					\
  extra;							\
  longjmp(saved_read_env->storage, action);			\
  /*NOTREACHED*/						\
}								\
extra;								\
saved_read_env = old_env;					\
return

/* Sigvector can be used to make signal do the right thing. */

#ifdef HAS_SIGVECTOR

#define my_signal(name, handler) install_signal_handler(name, handler)

signal_handler
install_signal_handler(signal_name, routine)
     int signal_name;
     signal_handler routine;
{
  struct sigvec new_vector;
  struct sigvec old_vector;

  new_vector.sv_handler = routine;
  new_vector.sv_mask    = (long) 0x0;
  new_vector.sv_onstack = 0;
  
  sigvector (signal_name, &new_vector, &old_vector);

  return (old_vector.sv_handler);
}

#else not HAS_SIGVECTOR

#define my_signal(name, handler) signal(name, handler)

#endif HAS_SIGVECTOR

#ifndef SIGNAL_BLOCKED_ON_DELIVERY

/* Vanilla Unix not only does not block the signal, but also
   installs handlers for only one shot.  Thus we must install
   a handler as soon as the signal is received, and make sure
   that our handler is in place before we exit.  As it is,
   there is a window between delivery and the signal call below
   in which the process will die if the signal is received again.
 */

#define disable_interrupt(signal_name)				\
my_signal(signal_name, SIG_IGN)

#define enable_interrupt(signal_name, routine)			\
my_signal(signal_name, routine)

#else /* SIGNAL_BLOCKED_ON_DELIVERY */

#define disable_interrupt(signal_name)				\
false

#define enable_interrupt(signal_name, routine)			\
false

#endif /* SIGNAL_BLOCKED_ON_DELIVERY */

#define interrupt_start(signal_name)				\
save_read_context(disable_interrupt(signal_name))

#define interrupt_end(signal_name, routine, action)		\
restore_read_context(enable_interrupt(signal_name, routine), action)

/* Abort, Termination, and Timer interrupt handlers. */

int
Control_G(sig)
     int sig;
{
  interrupt_start(sig);

  OS_tty_beep();
  OS_Flush_Output_Buffer();
  SET_INT_CHAR(CONTROL_G);
  interrupt_end(sig, Control_G, INTERRUPT);
}

/* Kill Scheme after undoing terminal garbage */
/* OS_Quit is called by Microcode_Termination */

extern char *assassin_signal;

char *assassin_signal = ((char *) NULL);

int
Kill_Me(sig)
     int sig;
{
  char *find_signal_name();

  assassin_signal = find_signal_name(sig);

  if (Debug_Flags[2])
    printf ("Kill_Me process %d, assassin signal %s\n", getpid(), assassin_signal);

  Microcode_Termination(TERM_SIGNAL);
  /*NOTREACHED*/
}

int
Timer_Interrupt(sig)
     int sig;
{
  interrupt_start(sig);
  REQUEST_INTERRUPT(INT_Timer);
  interrupt_end(sig, Timer_Interrupt, INTERRUPT);
}

/* Temporary suspension interrupt handler. */

int Scheme_Process_Id;

#ifdef SIGTSTP

/* Assumes there is sigsetmask */

#define NO_SIGNALS_ALLOWED -1
#define TSTP_MASK ~(1 << (SIGTSTP - 1))
#define BOGUS_SIGNAL 0

/* sig should only be SIGTSTP or BOGUS_SIGNAL in the following */

int
Suspend_Me(sig)
     int sig;
{
  int saved_mask = sigsetmask(NO_SIGNALS_ALLOWED);
  interrupt_start(SIGTSTP);

  OS_Quit(false);
  sigsetmask(saved_mask & TSTP_MASK);
  kill(Scheme_Process_Id, SIGTSTP);
  sigsetmask(NO_SIGNALS_ALLOWED);
  OS_Re_Init();
  sigsetmask(saved_mask);
  interrupt_end(SIGTSTP, Suspend_Me, REDO);
}

Boolean
Restartable_Exit()
{
  Suspend_Me(BOGUS_SIGNAL);
  return true;
}

#else

Boolean
Restartable_Exit()
{ 
#if false
  fprintf(stderr, "\Restartable_Exit: unimplemented.");
#endif
  return false;
}

#endif

/* Interactive interrupt handler: Utility procedure. */

#define C_STRING_LENGTH 256

void
Examine_Memory()
{
  Pointer *Where;
  char input_string[10];
  int free;
  Boolean interrupted;

  interrupted = false;
  printf("Enter location to examine (0x prefix for hex) : ");
  OS_Flush_Output_Buffer();

  /* Considerably haired up to go through standard (safe) interface.
     Taken from debug.c */
  
  if (interrupted)
    return;
  for (free = 0; free < C_STRING_LENGTH; free++)
  {
    input_string[free] = OS_tty_tyi(false, &interrupted);
    if (interrupted)
      return;
    if (input_string[free] == '\n')
    {
      input_string[free] = '\0';
      break;
    }
  }

  /* Check to see if address is in Hex (0x prefix). */
  if ((input_string[0] == '0') && (input_string[1] == 'x')) 
  {
    sscanf(input_string + 2, "%x", &Where);
  }
  else
  {
    sscanf(input_string, "%d", &Where);
  }
  Print_Expression(*Where, "Contents");
  OS_tty_newline();
  return;
}

/* Interactive interrupts: dispatch */

int
Ask_Me(reader, intchar)
     int (*reader)(), *intchar;
{
  char command;
  Boolean Interrupted;

  if (!Under_Emacs)
  {
    OS_tty_beep();
    OS_tty_newline();
  }
  while (true)
  {
    if (!Under_Emacs)
    {
      printf("Interrupt character (? for help): ");
      OS_Flush_Output_Buffer();
    }
    command = (*reader)(&Interrupted);
    if (Interrupted)
    {
      return (INTERACTIVE_RECURSIVE);
    }
    switch (command)
    {
      /* Debugging options. */

      case 'D':
      case 'd':
	if (!Under_Emacs)
	{
	  OS_tty_newline();
	  Handle_Debug_Flags();
	}
	return (INTERACTIVE_DISMISS);

      case 'E':
      case 'e':
	if (!Under_Emacs)
	{
	  OS_tty_newline();
	  Examine_Memory();
	}
	return (INTERACTIVE_DISMISS);

      case 'T':
      case 't':
	if (!Under_Emacs)
	{
	  OS_tty_newline(); 
	  Back_Trace();
	}
	return (INTERACTIVE_DISMISS);

      /* Interrupts, exitting, and suspension */

      case C_B:
      case 'B':
      case 'b':
        *intchar = CONTROL_B;
	return (INTERACTIVE_INTERRUPT);
	
      case C_G:
      case 'G':
      case 'g':
        *intchar = CONTROL_G;
	return (INTERACTIVE_INTERRUPT);

      case C_U:
      case 'U':
      case 'u':
	*intchar = CONTROL_U;
	return (INTERACTIVE_INTERRUPT);

      case C_X:
      case 'X':
      case 'x':
	*intchar = CONTROL_X;
	return (INTERACTIVE_INTERRUPT);
      
      case 'Q':
      case 'q':
	if (!Under_Emacs)
	{
	  forward Boolean confirm();
	  
	  if (!confirm("\nDo you really want to exit lisp? [Y or N]"))
	  {
	    return (INTERACTIVE_DISMISS);
	  }
	  else
	  {
	    return (INTERACTIVE_EXIT);
	  }
	}
	else
	{
	  return (INTERACTIVE_EXIT);
	}

      case 'Z':
      case 'z':
	if (!Under_Emacs)
	{
	  OS_tty_newline();
	}
	return (INTERACTIVE_SUSPEND);

      /* Help and miscellaneous. */

      case '\f':
	if (!Under_Emacs)
	{
	  OS_Clear_Screen();
	}
	return (INTERACTIVE_DISMISS);

#ifndef butterfly
      case 'R':
      case 'r':
      {
	forward Boolean confirm();
	forward void recover_from_trap();

	if (!confirm("\nDo you really want to reset? [Y or N] "))
	{
	  return (INTERACTIVE_DISMISS);
	}
	recover_from_trap(true, -1, 0, NULL, NULL);
	/*NOTREACHED*/
      }
#endif

      case 'H':
      case 'h':
	if (!Under_Emacs)
	{
	  printf("\n\n");

	  printf("The interrupt character is ");
	  print_character_name(sigint_char, true);
	  printf(".\n");
	  printf("  When typed, scheme will abort the running program,\n");
	  printf("  and the top level read-eval-print loop will resume.\n");

	  printf("The quit character is ");
	  print_character_name(sigquit_char, true);
	  printf(".\n");
	  printf("  When typed, various interrupt options are offered.\n");
	  printf("  Type ");
	  print_character_name(sigquit_char, false);
	  printf(" followed by `?' for a list of options.\n\n");
	}
	return (INTERACTIVE_DISMISS);

      case 'I':
      case 'i':
	if (!Under_Emacs)
	{
	  printf("Ignored.  Resuming Scheme.\n");
	} 
	return (INTERACTIVE_DISMISS);

      default:
	if (!Under_Emacs)
	{
	  OS_tty_newline();
	  printf("B: Enter a breakpoint loop.\n");
	  printf("D: Debugging: change interpreter flags.\n");
	  printf("E: Examine memory location.\n");
	  printf("X: Abort to current REP loop.\n");
	  printf("G: Goto to top level read-eval-print (REP) loop.\n");
	  printf("H: Print simple information on interrupts.\n");
	  printf("I: Ignore interrupt request.\n");
	  printf("U: Up to previous (lower numbered) REP loop.\n");
	  printf("Q: Quit instantly, killing Butterfly Scheme/Common Lisp.\n");
	  printf("T: Stack trace.\n");
#ifndef butterfly
	  printf("R: Hard reset.  Very dangerous.\n");
#endif
	  printf("Z: Quit instantly, suspending Scheme.\n");
	  printf("^L: Clear the screen.\n");
	  printf("\n");
	}
	break;
	/* loop */
    }
  }
}

/* Interactive interrupt handler */

/* Utility */

int
tyi_within_interrupt(interrupted)
     Boolean *interrupted;
{
  flush_input_buffer();
  return (OS_tty_tyi(true, interrupted));
}

/* The signal handler itself.
   It calls Ask_Me to dispatch on the input, but does the
   interrupting or dismissing itself.
 */

int
interactive_handler(sig)
     int sig;
{
  int intchar, code;
  long saved_intcode;
  interrupt_start(sig);
  
  while (true)
  {
    saved_intcode = FETCH_INTERRUPT_CODE();
    code = Ask_Me(tyi_within_interrupt, &intchar);
    switch(code)
    {
      case INTERACTIVE_INTERRUPT:
        SET_INT_CHAR(intchar);
	interrupt_end(sig, interactive_handler, INTERRUPT);
	/*NOTREACHED*/

      case INTERACTIVE_DISMISS:
	interrupt_end(sig, interactive_handler, REDO);
	/*NOTREACHED*/

      case INTERACTIVE_RECURSIVE:
	if (FETCH_INTERRUPT_CODE() == (saved_intcode | INT_Timer))
	  break;
	else
	  interrupt_end(sig, interactive_handler, INTERRUPT);
	
      case INTERACTIVE_SUSPEND:
	Restartable_Exit();
	interrupt_end(sig, interactive_handler, REDO);

      case INTERACTIVE_EXIT:
	Microcode_Termination(TERM_HALT);
    }
  }
}

static Boolean
confirm (string)
     char *string;
{
  char answer;
  forward char trap_read_option ();

  while (true)
  {
    printf ("%s", string);
    answer = (trap_read_option ((Boolean *) NULL));
    if ((answer == 'y') || (answer == 'Y'))
      return(true);
    if ((answer == 'n') || (answer == 'N'))
      return(false);
  }
}

static char
trap_read_option (Interrupted_ptr)
     Boolean *Interrupted_ptr;
{
  char option;
  Boolean Interrupted;

  OS_Flush_Output_Buffer ();
  if (Under_Emacs)
  {
    emacs_message (SCHEME_ENTER_INPUT_WAIT);
  }
  flush_input_buffer();
  option = (OS_tty_tyi (true, (& Interrupted)));
  if (Under_Emacs)
  {
    emacs_message (SCHEME_EXIT_INPUT_WAIT);
  }
  if ((! Interrupted) && (! Under_Emacs) && (option != '\n'))
  {
    OS_tty_write_char (option);
  }
  if (Interrupted_ptr != ((Boolean *) NULL))
  {
    (*Interrupted_ptr) = Interrupted;
  }
  return (option);
}

/* Trap handlers. */

/* Can it happen while we are not inside a primitive?
   Otherwise we can find out in what primitive it happened in from
   the expression register.
 */

int
Floating_Trap(sig, code)
     int sig, code;
{
  disable_interrupt(sig);

#if false
  fprintf(stderr, "\nFloating trap: code = %d\n", code);
  Primitive_Error(ERR_FLOATING_OVERFLOW);
#endif
  enable_interrupt(sig, Floating_Trap);
  return;
}

void
recover_from_trap(master, sig, code, scp)
     int sig, master, code;
     struct sigcontext *scp;
{
  IntEnb = INT_Mask;
  if (Free < MemTop)
    Free = MemTop;
  History = Make_Dummy_History();

#ifdef butterfly
  {
    int option, looping, i;
    Boolean interrupted;
    Pointer *stack;

    looping = !Recover_Automatically;

    while (looping && master)
    {
      printf("\nHardware trap process=%d sig=%x code=%d scp=%x pc=%x\n",
	     SHARED_DATA->Task_Id[Who_Am_I],
	     sig, code, scp, scp->sc_pc);
      printf("Options are: Q quit, D dump, L loop, R recover\n");
      option = trap_read_option(&interrupted);
      if (option >= 'a' && option <= 'z') option -= ' ';

      switch (option) {

      case 'Q':
	exit(-1);

      case 'R':
	looping = false;
	break;

      case 'D':
	printf("\n**** C Trace, Processor = %d, Process = %d ****\n\n",
	       Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I]);
	stack_trace();
	printf("\n**** Stack Trace, Processor = %d, Process = %d ****\n\n",
	       Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I]);
	Back_Trace(stdout);
	fflush(stdout);
	break;

      case 'L':
	printf("\nProcess %d looping for dbx\n", SHARED_DATA->Task_Id[Who_Am_I]);
	while (true) Standard_Delay();
	break;
      }
    }

    printf("\nRecovering from hardware trap\n");
    SHARED_DATA->Work_Queue.queue_count = 0;
    SHARED_DATA->Work_Queue.queue_lock = 0;
    SHARED_DATA->Hardware_Trap_Count = 0;

    if (master) {
      if (Get_Fixed_Obj_Slot(Future_Vector) != NIL) {
	Pointer Running;
	Running = Fast_Vector_Ref(Get_Fixed_Obj_Slot(Future_Vector),
				  Who_Am_I + 1);
	if (Type_Code(Running) == TC_FUTURE) {
	  SHARED_DATA->Hardware_Trap_Task_Code =
	    Fast_Vector_Ref(Running, FUTURE_ORIG_CODE);
	  if (Type_Code(Fast_Vector_Ref(Running, FUTURE_PRIVATE)) ==
	      TC_CHARACTER_STRING)
	    SHARED_DATA->Hardware_Trap_Task_Name =
	      Fast_Vector_Ref(Running, FUTURE_PRIVATE);
	  else
	    SHARED_DATA->Hardware_Trap_Task_Name =
	      Fast_Vector_Ref
		(Fast_Vector_Ref
		 (Fast_Vector_Ref(Running, FUTURE_PRIVATE), 1), 1);
	  SHARED_DATA->Hardware_Trap_Task_Number =
	    Fast_Vector_Ref(Running, FUTURE_METERING);
	  SHARED_DATA->Hardware_Trap_Task_Spawn =
	    Fast_Vector_Ref(Running, FUTURE_SPAWNID); }
	else {
	  SHARED_DATA->Hardware_Trap_Task_Code = NIL;
	  SHARED_DATA->Hardware_Trap_Task_Name = NIL;
	  SHARED_DATA->Hardware_Trap_Task_Number = NIL;
	  SHARED_DATA->Hardware_Trap_Task_Spawn = NIL; } }
      SHARED_DATA->Hardware_Trap_PC
	= Make_Non_Pointer(TC_FIXNUM, scp->sc_pc);
      SHARED_DATA->Hardware_Trap_Irritant
	= Make_Non_Pointer(TC_FIXNUM, 0);	/* ???? */
      SHARED_DATA->Hardware_Trap_Code
	= Make_Non_Pointer(TC_FIXNUM, (sig << 16) + code);
      
      SHARED_DATA->Memory_Lock = 0;
      for (i = 0; i < N_Interps; i++)
	SHARED_DATA->Memory_Table[i].Free_Bottom =
	  SHARED_DATA->Memory_Table[i].Free_Top;

      stack = Stack_Pointer;
      for (i = 0; i < HSTACK_SIZE; i++)
	if (stack >= Absolute_Stack_Base &&
	    stack <= Highest_Allocated_Address)
	  SHARED_DATA->Hardware_Trap_Stack[i] = *stack++;
	else
	  SHARED_DATA->Hardware_Trap_Stack[i] = (-1); } }
#endif
    
  Initialize_Stack();

  if (master) {
    IntCode = INT_GC;
   Will_Push(CONTINUATION_SIZE + (STACK_ENV_EXTRA_SLOTS + 2));
    Store_Return(RC_END_OF_COMPUTATION);
    Store_Expression(NIL);
    Save_Cont();
    Push(Make_Non_Pointer(TC_FIXNUM, sig));
    Push(Get_Fixed_Obj_Slot(Trap_Handler));
    Push(STACK_FRAME_HEADER + 1);
   Pushed(); }
  else {
    IntCode = 0;
   Will_Push(CONTINUATION_SIZE + (STACK_ENV_EXTRA_SLOTS + 1));
    Store_Return(RC_END_OF_COMPUTATION);
    Store_Expression(NIL);
    Save_Cont();
    Push(Get_Fixed_Obj_Slot(Default_Rescheduler));
    Push(STACK_FRAME_HEADER);
   Pushed(); }

  longjmp(*Back_To_Eval, PRIM_APPLY);
  /* The following comment is by courtesy of LINT, your friendly sponsor. */
  /*NOTREACHED*/ }

extern void spin_for_debugger();

void
spin_for_debugger()
{
#ifdef butterfly
  if (Debug_Flags[18])
  {
    printf("\nProcess %d (on %d) looping for dbx\n",
	   SHARED_DATA->Task_Id[Who_Am_I], Who_Am_I);
    while (true)
      Standard_Delay();
  }
  /*NOTREACHED*/
#else /* not butterfly */
  return;
#endif /* butterfly */
}

void
really_spin_for_debugger (msg)
char *msg;
{
#ifdef butterfly
  SHARED_DATA->debug_spin_flag = 1;
  printf("\n(%s) process %d (on %d) looping for dbx\n",
	 msg, SHARED_DATA->Task_Id[Who_Am_I], Who_Am_I);
  while (SHARED_DATA->debug_spin_flag != 0)
    Standard_Delay();
  /*NOTREACHED*/
#else /* not butterfly */
  return;
#endif /* butterfly */
}

static int
suspension_trap (sig, code)
     int sig;
     int code;
{
  interrupt_start (sig);
  REQUEST_INTERRUPT (INT_Suspend);
  interrupt_end (sig, suspension_trap, INTERRUPT);
  /*NOTREACHED*/
}

int
Hardware_Trap(sig, code, scp)
     int sig, code;
     struct sigcontext *scp;
{
  void Trap_Common();

  Trap_Common("The hardware", sig, code, scp, Hardware_Trap);
  /*NOTREACHED*/
}


#ifdef butterfly

int
Special_Handler(sig, code, scp)
     int sig, code;
     struct sigcontext *scp;
{ 
  long action;

  interrupt_start(sig);

  action = REDO;
  if (Debug_Flags[10])
  {
    printf("Signal type %d arrived on %d pc = %x\n",
	   SHARED_DATA->Signal_Meaning, Who_Am_I, scp->sc_pc);
    fflush(stdout);
  }

  switch (SHARED_DATA->Signal_Meaning)
  {
    case SIG_GLOBAL_INT:
    {
      static struct global_interrupt_data gid;

      Get_Signal_Data(&gid, sizeof(gid));
      REQUEST_INTERRUPT(gid.code);
      action = INTERRUPT;
      break;
    }

    case SIG_CHAR_INT:
    {
      static struct character_interrupt_data cid;

      Get_Signal_Data(&cid, sizeof(cid));
      Int_State |= INT_STATE_CHAR;
      SET_INT_CHAR(cid.kind);
      action = INTERRUPT;
      break;
    }

    case SIG_INPUT_INT:
    {
      static struct input_interrupt_data iid;
      int i;

      Get_Signal_Data(&iid, sizeof(iid));
      for (i = 0; i < iid.length; i++)
	Int_String[i] = iid.data[i];
      Int_String[i] = '\0';
      Int_State |= INT_STATE_INPUT;
      REQUEST_INTERRUPT(INT_Character);
      action = INTERRUPT;
      break;
    }

    case SIG_NEW_CODE:
    {
      extern void get_new_code();
      static struct new_code_interrupt_data ncid;

      Get_Signal_Data(&ncid, sizeof(ncid));
      get_new_code(&ncid);
      atomadd(&SHARED_DATA->GC_Propagate_Lock, -1);
      action = REDO;
      break;
    }

    case SIG_DISK_RESTORE:
    {
      /* Note: this MUST be static. */

      static struct disk_restore_interrupt_data droid;

      Get_Signal_Data(&droid, sizeof(droid));
      if (droid.state)
      {
	/* Start of disk restore. */

	while (droid.state)
	{
	  sigpause((~sigmask(SIGTERM)) &
		   (~sigmask(SIGTSTP)) &
		   (~sigmask(SIGUSR1)));
	}

	/* End of disk restore. */

	if (droid.succeeded)
	{
	  extern void slave_after_band_load();

	  real_read_env.in_input_wait = false;
	  saved_read_env = &real_read_env;
	  sigsetmask(0);
	  slave_after_band_load(((Pointer) droid.utils));
	  /*NOTREACHED*/
	}
      }
      action = REDO;
      break;
    }

    case SIG_RELOCATE:
    {
      extern void relocate_area();
      static struct relocate_interrupt_data rid;

      Get_Signal_Data(&rid, sizeof(rid));
      relocate_area(&rid);
      atomadd(&SHARED_DATA->GC_Propagate_Lock, -1);
      action = REDO;
      break;
    }

    case SIG_FILE_CODE:
    {
      extern void get_file_code();
      static struct file_code_data fcd;

      Get_Signal_Data(&fcd, sizeof(fcd));
      get_file_code(&fcd);
      atomadd(&SHARED_DATA->GC_Propagate_Lock, -1);
      action = REDO;
      break;
    }

    default:
    {
      static struct unknown_interrupt_data foo;

      Get_Signal_Data(&foo, SHARED_DATA->Signal_Data_Length);
      fprintf(stderr, "(on %d/%d) Ignoring unknown signal type %d\n",
	      Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I], 
	      SHARED_DATA->Signal_Meaning);
      action = REDO;
      break;
    }
  }

  if (Debug_Flags[10])
  {
    printf("Returning from interrupt handler IntCode = %x IntEnb=%x pc=%x\n",
	   IntCode, IntEnb, scp->sc_pc);
    fflush(stdout);
  }

  interrupt_end(sig, Special_Handler, action);

#if false
  sigreturn(scp);
#else /* not false */
  return;
#endif /* false */
}

#endif butterfly

int
Software_Trap(sig, code)
     int sig, code;
{
  void Trap_Common();

  Trap_Common("System software", sig, code, Software_Trap);
  /*NOTREACHED*/
}

void
dump_core ()
{
  signal(SIGQUIT, SIG_DFL);
  kill(Scheme_Process_Id, SIGQUIT);
  /*NOTREACHED*/
}

void
Trap_Common(message, sig, code, scp, handler)
     char *message;
     int sig, code;
     struct sigcontext *scp;
     signal_handler handler;
{
  char command, *name;
  Boolean Interrupted;
  char *find_signal_name();

  disable_interrupt(sig);

#ifdef butterfly
  {
    int old_count;
    
    Set_Time_Zone(Zone_Trapping);
    old_count = atomadd(&SHARED_DATA->Hardware_Trap_Count, 1);
    sleep(1);
    recover_from_trap(old_count == 0, sig, code, scp);
  }
#else

  name = find_signal_name(sig);
  printf("\n%s has trapped! signal = %d (%s); code = %d",
	 message, sig, name, code);
  printf("\nThe system may not be able to recover.");

  if (Recover_Automatically)
  {
    printf("\nAttempting to recover...");
    recover_from_trap(true, sig, code, scp);
  }

  command = '\0';
try_again:
  do
  {
    Interrupted = false;
    if (command != '\n')
    {
      printf("\nChoose an action [Y = proceed; N = kill Scheme; D = dump core] ");
    }
    command = (trap_read_option (&Interrupted));
  } while (Interrupted ||
	   ((command != 'N') && (command != 'n') &&
	    (command != 'Y') && (command != 'y') &&
	    (command != 'D') && (command != 'd')));
  printf("\n");
  enable_interrupt(sig, handler);
  if ((command == 'D') || (command == 'd'))
  {
    if (!confirm("Do you really want a core dump? [Y or N] "))
    {
      goto try_again;
    }
    OS_Quit(false);
    dump_core();
  }
  if ((command == 'N') || (command == 'n'))
  {
    if (!confirm("Do you really want to kill Scheme? [Y or N] "))
    {
      goto try_again;
    }
    printf("\n");
    Microcode_Termination(TERM_TRAP);
    /*NOTREACHED*/
  }
  recover_from_trap(true, sig, code, scp);
#endif
  /*NOTREACHED*/
}

/* Signal handler descriptors */

typedef struct
{ int the_signal;
  char *the_name;
  Boolean do_it_always;
  signal_handler handler;
} signal_state;

/* Disable keyboard interrupt handlers */

#ifndef butterfly
#define ABORT_HANDLER		Control_G
#define INTERACTIVE_HANDLER	interactive_handler
#define SUSPEND_HANDLER		Suspend_Me
#else
#define ABORT_HANDLER		(SIG_IGN)
#define INTERACTIVE_HANDLER	(SIG_IGN)
#define SUSPEND_HANDLER		(SIG_DFL)
#endif

/* The only signals which are always assumed to be there are
   SIGINT and SIGQUIT.
 */

static signal_state scheme_signal_handlers[] =
{
  { SIGINT,	"SIGINT",	true,	((signal_handler) ABORT_HANDLER)},
  { SIGQUIT,	"SIGQUIT",	false,	((signal_handler) INTERACTIVE_HANDLER)},
#ifdef SIGTERM
  { SIGTERM,	"SIGTERM",	true,	((signal_handler) Kill_Me)},
#endif
#ifdef SIGHUP
  { SIGHUP,	"SIGHUP",	false,	((signal_handler) Kill_Me)},  /* was suspension_trap */
#endif
#ifdef SIGTSTP
  { SIGTSTP,	"SIGTSTP",	false,	((signal_handler) SUSPEND_HANDLER)},
#endif
#ifdef SIGALRM
  { SIGALRM,	"SIGALRM",	true,	((signal_handler) Timer_Interrupt)},
#endif
#ifdef SIGVTALRM
  { SIGVTALRM,	"SIGVTALRM",	true,	((signal_handler) Timer_Interrupt)},
#endif
#ifdef SIGFPE
  { SIGFPE,	"SIGFPE",	true,	((signal_handler) Floating_Trap)},
#endif
#ifdef SIGILL
  { SIGILL,	"SIGILL",	false,	((signal_handler) Hardware_Trap)},
#endif
#ifdef SIGBUS
  { SIGBUS,	"SIGBUS",	false,	((signal_handler) Hardware_Trap)},
#endif
#ifdef SIGSEGV
#ifdef butterfly
  { SIGSEGV,	"SIGSEGV",	true,	((signal_handler) Section_Fault)},
#else
  { SIGSEGV,	"SIGSEGV",	false,	((signal_handler) Hardware_Trap)},
#endif
#endif

#ifdef SIGTRAP
  { SIGTRAP,	"SIGTRAP",	false,	((signal_handler) Hardware_Trap)},
#endif
#ifdef SIGPIPE
  { SIGPIPE,	"SIGPIPE",	false,	((signal_handler) Kill_Me)},
#endif
#ifdef SIGPWR
  { SIGPWR,	"SIGPWR",	false,	((signal_handler) Kill_Me)},
#endif
#ifdef SIGIOT
  { SIGIOT,	"SIGIOT",	false,	((signal_handler) Software_Trap)},
#endif
#ifdef SIGEMT
  { SIGEMT,	"SIGEMT",	false,	((signal_handler) Software_Trap)},
#endif
#ifdef SIGSYS
  { SIGSYS,	"SIGSYS",	false,	((signal_handler) Software_Trap)},
#endif
#ifdef SIGUSR1
#ifdef butterfly
  { SIGUSR1,	"SIGUSR1",	true,	((signal_handler) Special_Handler)},
#else
  { SIGUSR1,	"SIGUSR1",	true,	((signal_handler) Software_Trap)},
#endif
#endif
#ifdef SIGUSR2
  { SIGUSR2,	"SIGUSR2",	false,	((signal_handler) Software_Trap)},
#endif
#ifdef SIGPROF
  { SIGPROF,	"SIGPROF",	false,	((signal_handler) Software_Trap)},
#endif
};

/* Missing HPUX signals:
   SIGKILL, SIGCLD, SIGIO, SIGWINDOW
*/

/* Missing bsd signals:
   SIGKILL, SIGURG, SIGSTOP, SIGCONT, SIGCHLD,
   SIGTTIN, SIGTTOU, SIGIO, SIGXCPU, SIGXFSZ
*/

#define NHANDLERS sizeof(scheme_signal_handlers)/sizeof(signal_state)

static signal_state outside_signal_handlers[NHANDLERS];

void
hack_signals(source, dest, do_all, save_state)
     signal_state *source, *dest;
     Boolean do_all, save_state;
{
  signal_handler old_handler;
  fast int i;
  fast signal_state *from, *to;

  /* The following is total paranoia in case a signal which undoes all
     comes in while we are not yet done setting them all up.
   */
  if (save_state)
    for (i = 0, from = source, to = dest;
	 i < NHANDLERS;
	 i++, *from++, *to++)
    {
      to->the_signal = from->the_signal;
      to->the_name = from->the_name;
      to->do_it_always = from->do_it_always;
      to->handler = ((signal_handler) SIG_DFL);
    }
  for (i = 0, from = source, to = dest;
       i < NHANDLERS;
       i++, *from++, *to++)
  {
    if (from->do_it_always || do_all)
    {
      old_handler =
	((signal_handler) my_signal(from->the_signal, from->handler));
      if (old_handler == ((signal_handler) -1))
	old_handler = ((signal_handler) SIG_DFL);
    }
    else
      old_handler = ((signal_handler) SIG_DFL);
    if (save_state)
      to->handler = old_handler;
  }
  if (Debug_Flags[15]) 
    {
      signal(SIGSEGV, SIG_DFL);
      printf("Process %d SIGSEGV should dump core\n", getpid());
    }
  return;
}

char *
find_signal_name(sig)
     int sig;
{
  static char buf[128];
  fast int i;

  for (i = 0; i < NHANDLERS; i++)
  {
    if (scheme_signal_handlers[i].the_signal == sig)
      return (scheme_signal_handlers[i].the_name);
  }

  sprintf(&buf[0], "unknown signal #d%d", sig);
  return (&buf[0]);
}

#define TERMCAP_BUFFER_SIZE 1024

#if defined(HPUX) && !defined(system3)
#define Break_Terminal_Connection()	setpgrp()
#else
#define Break_Terminal_Connection()
#endif

void
OS_Init(ignore)
     Boolean ignore;
{
  char termcaps[TERMCAP_BUFFER_SIZE];
  static char tcb[TERMCAP_BUFFER_SIZE];
  char *tcp = &tcb[0];

  OS_Name = SYSTEM_NAME;
  OS_Variant = SYSTEM_VARIANT;

#ifdef butterfly
  if (Who_Am_I == 0)
#endif
    printf("Butterfly Scheme/Common Lisp, %s [%s] version\n",
	   OS_Name, OS_Variant);

  initialize_process_clock();
  initialize_real_time_clock();

  real_read_env.in_input_wait = false;
  saved_read_env = &real_read_env;

  /* Find process information */

  Under_Emacs =
    Parse_Option("-emacs", Saved_argc, Saved_argv, true) != NOT_THERE;

  Scheme_Process_Id = getpid();

  {
    struct stat s;

    stdin_file_type =
      (((fstat ((fileno (stdin)), (& s))) != 0)
       ? '?'
       : (file_type_letter (& s)));
  }
  stdin_is_a_kbd = (isatty (fileno (stdin)));
  stdout_is_a_crt = (isatty (fileno (stdout)));

  if (Debug_Flags[1])
    printf("s_i_a_k = %d s_i_a_c = %d\n", stdin_is_a_kbd, stdout_is_a_crt);

  /* The ultimate in C style -- by Jinx */

  if (Under_Emacs					||
      (!stdout_is_a_crt)				||
      ((term = getenv("TERM")) == NULL) 		||
      (tgetent(termcaps, term) <= 0)			||
      ((CM = tgetstr("cm", &tcp)) == NULL))
  {
    Can_Do_Cursor = false;
  }
  else /* Find terminal information */
  {
    LI = tgetnum("li");
    CO = tgetnum("co");
    UP = tgetstr("up", &tcp);
    CL = tgetstr("cl", &tcp);
    CE = tgetstr("ce", &tcp);
    BC = tgetflag("bs") ? "\b" : tgetstr("bc", &tcp);
    Can_Do_Cursor = true;
  }

  Int_Char = '\0';

#ifdef butterfly

  Int_State = 0;

#else /* not butterfly */

  interactive_p = (Under_Emacs || (stdin_is_a_kbd && stdout_is_a_crt));

  if (interactive_p && !Under_Emacs)
  {
    printf("Type ");
    print_character_name(sigquit_char, true);
    printf(" followed by `H' to obtain information about interrupts.\n");
  }

  if (!interactive_p)
  {
    Break_Terminal_Connection();
  }
#endif /* butterfly */

  if (Debug_Flags[1]) printf("Can Do Cursor =%d\n", Can_Do_Cursor);
  OS_Flush_Output_Buffer();

  /* Swap in Scheme IO */
  OS_Re_Init();
}

/* Terminal parameter hacking. */

static char Orig_Interrupt, Orig_Quit, Orig_EOF;

#if defined(bsd) || defined(nu)

static long Orig_flags;

#define hack_crt(old, new)						\
{									\
  int crt_pgrp;								\
									\
  ioctl(fileno(stdout), TIOCGPGRP, &crt_pgrp);				\
  if (getpgrp(Scheme_Process_Id) == crt_pgrp)				\
  {									\
    struct sgttyb sg;							\
									\
    gtty(fileno(stdout), &sg);						\
    Orig_flags = old;							\
    sg.sg_flags = new;							\
    ioctl(fileno(stdout), TIOCSETN, &sg);				\
  }									\
}

#ifdef butterfly
#define hack_kbd(Ni, Oi, Nq, Oq, Ne, Oe)				\
{}

#else
#define hack_kbd(Ni, Oi, Nq, Oq, Ne, Oe)				\
{									\
  int crt_pgrp;								\
									\
  ioctl(fileno(stdin), TIOCGPGRP, &crt_pgrp);				\
  if (getpgrp(Scheme_Process_Id) == crt_pgrp)				\
    basic_hack_kbd(Ni, Oi, Nq, Oq, Ne, Oe);				\
}
#endif

#else
#if defined(TCSETA) /* hpux, ATT */

/* Make hpux/system V look like bsd so hack_kbd works */

#define tchars termio
#define TIOCGETC TCGETA
#define TIOCSETC TCSETA
#define t_intrc c_cc[VINTR]
#define t_quitc c_cc[VQUIT]
#define t_eofc  c_cc[VEOF]
#define hack_crt(old, new)

#define hack_kbd(Ni, Oi, Nq, Oq, Ne, Oe)				\
  basic_hack_kbd(Ni, Oi, Nq, Oq, Ne, Oe)

#else /* ??? */

#define hack_crt(old, new)
#define hack_kbd(Ni, Oi, Nq, Oq, Ne, Oe)

#endif
#endif

/* This swaps interrupt characters */

#define basic_hack_kbd(Nintr, Ointr, Nquit, Oquit, NEOF, OEOF)		\
{									\
  struct tchars Terminal_Chars;						\
									\
  ioctl((fileno (stdin)), TIOCGETC, (& Terminal_Chars));		\
  Orig_Interrupt = Ointr;						\
  Orig_Quit = Oquit;							\
  Orig_EOF = OEOF;							\
  (Terminal_Chars . t_intrc) = Nintr;					\
  (Terminal_Chars . t_quitc) = Nquit;					\
  (Terminal_Chars . t_eofc)  = NEOF;					\
  ioctl((fileno (stdin)), TIOCSETC, (& Terminal_Chars));		\
}

/* These procedures swap the terminal and signals from outside to scheme,
   and viceversa. */

void
OS_Re_Init ()
{
  if (stdin_is_a_kbd)
  {
    hack_kbd (sigint_char, Terminal_Chars.t_intrc,
	      sigquit_char, Terminal_Chars.t_quitc,
	      DISABLE_EOF, (Terminal_Chars . t_eofc));
  }
  if (Debug_Flags[1]) printf("1/2 way through re init\n");
  hack_signals (scheme_signal_handlers,
		outside_signal_handlers,
		(stdin_is_a_kbd || Under_Emacs),
		true);

  if (stdout_is_a_crt)
  {
    hack_crt ((sg . sg_flags), ((sg . sg_flags) & (~ XTABS)));
  }
#ifdef sun
  vadvise (VA_ANOM);		/* Anomolous paging, don't try to guess. */
#endif  
  if (Debug_Flags[1]) printf("done with re init\n");
  return;
}

void
OS_Quit (abnormal_p)
     Boolean abnormal_p;
{
  OS_Flush_Output_Buffer ();

  if (stdout_is_a_crt)
  {
    hack_crt (Orig_flags, Orig_flags);
  }
  
  hack_signals (outside_signal_handlers,
		scheme_signal_handlers,
		(stdin_is_a_kbd || Under_Emacs),
		false);
	       
  if (stdin_is_a_kbd)
  {
    hack_kbd (Orig_Interrupt, Orig_Interrupt,
	      Orig_Quit, Orig_Quit,
	      Orig_EOF, Orig_EOF);
  }
  if (abnormal_p && interactive_p)
  {
    printf("\nScheme has terminated abnormally!\n");
    if (confirm("Would you like a core dump? [Y or N] ") &&
	confirm("\nDo you really want a core dump? [Y or N] "))
    {
      printf("\n\n");
      dump_core();
    }
    printf("\n\n");
  }
  return;
}

/**** BBN/Common Lisp Support Follows ****/

#include "cl-fileio.h"

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
	open_flags = O_RDWR;
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

  if (OS_file_existence_test(name) > 0)
    *file_exists_p = true;
  else
    *file_exists_p = false;

  errno = 0;
  fd = open(name,open_flags,0666);
  if (fd == -1) 
    {
      *status = errno;
      if (*status==ENOENT) *file_exists_p = false;
    }
  else if (seek_to_eof)
    { 
      errno = 0;
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
  errno = 0;
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

  errno = 0;
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

  errno = 0;
  n_written = write(fd,buf,nchars);
  *status = errno;
  return n_written;
}

int
OS_cl_file_set_pos(fd,pos,status)
int fd, pos, *status;
{
  int r;
  
  errno = 0;
  if (pos==-1)
    r = lseek(fd,0,2);
  else
    r = lseek(fd,pos,0);
  *status = errno;
  return r;
}

int
OS_cl_file_get_pos(fd,status)
int fd, *status;
{
  int pos;

  errno = 0;
  pos = lseek(fd,0,1);
  *status = errno;
  return pos;
}

Boolean
OS_cl_listen_fd(fd,status)
     int fd, *status;
{
  long temp;
  GUARANTEE_WD();
  errno = 0;
  if (ioctl(fd, FIONREAD, &temp) == -1)
  {
    *status = errno;
    return false;
  }
  else
  {
    *status = 0;
    return (temp > 0);
  }
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
  struct passwd *pw, *getpwuid();

  GUARANTEE_WD();
  if (stat(filename,&filestat) != 0)
    return ((char *) NULL);
  pw = getpwuid(filestat.st_uid);
#ifdef butterfly
  return ((char *) NULL);
#else
  return (pw->pw_name);
#endif
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

  GUARANTEE_WD();
  res = stat(filename,&filestat);
  *time = unix_to_universal_time(filestat.st_mtime);
  return (res);
}
