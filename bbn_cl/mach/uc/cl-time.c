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

/* this file assumes UNIX - makes use of localtime(3), gettimeofday(2)
   and times(2) */

#include <sys/types.h>
#include <sys/times.h>
#include <sys/time.h>
#include <sys/dir.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <sys/resource.h>
#include "scheme.h"
#include "primitive.h"

Pointer C_Unsigned_Integer_To_Scheme_Integer();
unsigned int Scheme_Integer_To_C_Unsigned_Integer();

#define private	static		/* for jpayne - I like it better this way */

#define RESOURCE_USAGE_VECTOR_SIZE 15

/* Below, we don't get rss info because it is not differential data.
   We can add new calls for these if desired */ 

Define_Primitive(prim_getrusage, -1, "GET-RESOURCE-USAGE-VECTOR")
{
  struct rusage r;
  Pointer v;
  long *p;
  long idle_time;

  Primitive_Variable_Args();

  if (Number_Of_Args > 1)
    Primitive_Error(ERR_WRONG_NUMBER_OF_ARGUMENTS);
  if (Number_Of_Args == 1)
    {
      v = Primitive_Variable_Arg(1);
      if ((Type_Code(v) != TC_VECTOR_32B) ||
	  (Vector_Length(v) != RESOURCE_USAGE_VECTOR_SIZE))
	Primitive_Error(ERR_ARG_1_WRONG_TYPE);
    }
  else
    v = make_bare_vector_32b(RESOURCE_USAGE_VECTOR_SIZE);
  getrusage(RUSAGE_SELF, &r);
  idle_time = OS_idle_time();
  p = (long *)&(User_Vector_Ref(v, 0));
#ifdef butterfly
  *p++ = Who_Am_I;
#else
  *p++ = 0;
#endif
  *p++ = OS_real_time_clock();
  *p++ = OS_process_clock() - idle_time;
  *p++ = OS_system_process_clock();
  *p++ = idle_time;
  *p++ = r.ru_minflt;
  *p++ = r.ru_majflt;
  *p++ = r.ru_nswap;
  *p++ = r.ru_inblock;
  *p++ = r.ru_oublock;
  *p++ = r.ru_msgsnd;
  *p++ = r.ru_msgrcv;
  *p++ = r.ru_nsignals;
  *p++ = r.ru_nvcsw;
  *p++ = r.ru_nivcsw;
  return v;
}

Define_Primitive(prim_get_internal_run_time, 0, "GET-INTERNAL-RUN-TIME")
{
  struct tms t;
  Pointer r;

  times(&t);
  r = c_int_to_scheme_int(t.tms_utime+t.tms_stime);
  return r;
}

Define_Primitive(prim_get_internal_time_units_per_second, 0, "GET-INTERNAL-TIME-UNITS-PER-SECOND")
{
  return (Make_Unsigned_Fixnum(60));
}

/* first calculate the number of seconds since the beginning of this
   century up to 1970 - note there were 17 leap years between 1900 and
   1970

   this is just seconds/day times days-since-1900 (including 17
   more days because of leap years) + T, the time since 1970 */

#define UNIX_TO_UNIVERSAL_TIME	2208988800

time_t
unix_to_universal_time(t)
time_t	t;
{
	return UNIX_TO_UNIVERSAL_TIME + t;
}

static
universal_to_unix_time(t)
time_t	t;
{
	return t - UNIX_TO_UNIVERSAL_TIME;
}

Define_Primitive(prim_get_universal_time, 0, "GET-UNIVERSAL-TIME")
{
	return c_int_to_scheme_int(unix_to_universal_time(time(0)));
}

Define_Primitive(prim_get_time_zone, 0, "GET-TIME-ZONE")
{
	struct timeval	tv;
	struct timezone	tz;

	(void) gettimeofday(&tv, &tz);
	return Make_Unsigned_Fixnum(tz.tz_minuteswest / 60);
}

/* returns a vector with the 9 elements as specified in the commonlisp
   manual */
#define TIME_ELEMENTS	9

Define_Primitive(prim_unix_ctime, 1, "UNIX-CTIME")
{
	long	length, 
		i;
	time_t	t;
	struct tm	*tp;
	struct timeval	tv;
	struct timezone	tz;
	Primitive_1_Args();

	if (Type_Code(Arg1) != TC_FIXNUM && Type_Code(Arg1) != TC_BIG_FIXNUM)
		Arg_1_Type(TC_FIXNUM);		/* do the error */
	if ((i = Scheme_Integer_To_C_Unsigned_Integer(Arg1, &t)) != PRIM_DONE)
		Primitive_Error(i);

	gettimeofday(&tv, &tz);
	t = universal_to_unix_time(t);
	tp = localtime(&t);
	length = TIME_ELEMENTS;
	Primitive_GC_If_Needed(length + 1);
	*Free++ = Make_Non_Pointer(TC_MANIFEST_VECTOR, length);

	/* now fill in the vector */
	*Free++ = Make_Non_Pointer(TC_FIXNUM, tp->tm_sec);
	*Free++ = Make_Non_Pointer(TC_FIXNUM, tp->tm_min);
	*Free++ = Make_Non_Pointer(TC_FIXNUM, tp->tm_hour);
	*Free++ = Make_Non_Pointer(TC_FIXNUM, tp->tm_mday);
	*Free++ = Make_Non_Pointer(TC_FIXNUM, 1 + tp->tm_mon);
	*Free++ = Make_Non_Pointer(TC_FIXNUM, 1900 + tp->tm_year);
	*Free++ = Make_Non_Pointer(TC_FIXNUM, tp->tm_wday);
	*Free++ = tp->tm_isdst ? TRUTH : NIL;
	*Free++ = Make_Non_Pointer(TC_FIXNUM, tz.tz_minuteswest / 60);

	return Make_Pointer(TC_VECTOR, Free - (length + 1));
}

#define SPD	86400

Define_Primitive(prim_unix_inverse_ctime, 2, "UNIX-INVERSE-CTIME")
{
	struct timeval	tv;
	struct timezone	tz;
	struct tm	tm;
	long	now;
	Pointer	*op;
	int	year,
		mon,
		days,
		time_zone;
	time_t	seconds;
	struct tm	*tp;
	Primitive_2_Args();
	Touch_In_Primitive (Arg1, Arg1);
	Touch_In_Primitive (Arg2, Arg2);

	op = Nth_Vector_Loc(Arg1, 1);
	tm.tm_sec = Get_Integer(*op++);
	tm.tm_min = Get_Integer(*op++);
	tm.tm_hour = Get_Integer(*op++);
	tm.tm_mday = Get_Integer(*op++);
	tm.tm_mon = Get_Integer(*op++);
	tm.tm_year = Get_Integer(*op++);
	time_zone = Get_Integer(*op++);

	(void) time(&now);
	/* now make sure tm_year is right */
	if (tm.tm_year < 100) {
		int	boc, 	/* beginning of century */
			lower,
			upper;
		time_t	now;

		tp = gmtime(&now);

		tp->tm_year += 1900;
		boc = (tp->tm_year - (tp->tm_year % 100));
		lower = boc + tm.tm_year;
		upper = lower + 100;		/* following century */
		if (tp->tm_year - lower < upper - tp->tm_year)
			tm.tm_year = lower;
		else
			tm.tm_year = upper;
	}
	if (tm.tm_year < 1900)
		Primitive_Error(ERR_ARG_2_BAD_RANGE);
	days = 0;	/* days since 1970 */
	if (tm.tm_year >= 1970) {
		seconds = unix_to_universal_time(0);
		year = 1970;
	} else {
		seconds = 0;
		year = 1900;
	}
	for (; year < tm.tm_year; year++)
		days += year_days(year);
	for (mon = 1; mon < tm.tm_mon; mon++)
		days += month_days(year, mon);
	days += tm.tm_mday - 1;
	seconds += (days * SPD) + (tm.tm_hour * 3600) + (tm.tm_min * 60) + tm.tm_sec;

	seconds += (time_zone * 3600);
	/* if second argument is false then TIME-ZONE was not supplied,
	   so we got the default and we want to adjust for dst */
	if (Arg2 == NIL) {
		struct tm	*tp;

		tp = localtime(&now);
		if (tp->tm_isdst)
			seconds -= 3600;
	}

	return C_Unsigned_Integer_To_Scheme_Integer(seconds);
}

private
year_days(year)
register int	year;
{
	if ((year % 4) == 0 && (!((year % 100) == 0) || (year % 400) == 0))
		return 366;
	return 365;
}

/* number of days in MON in year YEAR - Jan is month 1 */
private
month_days(year, mon)
register int	year, mon;
{
	static int	mons[] = {-1, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

	if (mon == 2 && year_days(year) == 366)
		return 29;	/* feb. on leap years */
	return mons[mon];
}

/*
  	Versions of the C-integer <-> Scheme-integer conversion programs
	which deal with unsigned integers follow:
*/

#include "bignum.h"

Pointer C_Unsigned_Integer_To_Scheme_Integer(C)
unsigned long C;
{ fast bigdigit *Answer, *SCAN, *size;
  long Length;
  if (Fixnum_Fits(C))
    return Make_Non_Pointer(TC_FIXNUM, C);
  Length = Align(C_INTEGER_LENGTH_AS_BIGNUM);
  Primitive_GC_If_Needed(Length);
  Answer = BIGNUM(Free); 
  Prepare_Header(Answer, 0, POSITIVE);
  size   = &LEN(Answer);
  for (SCAN = Bignum_Bottom(Answer); C != 0; *size += 1)
  { *SCAN++ = Rem_Radix(C);
    C    = Div_Radix(C);
  }
  *((Pointer *) Answer) = Make_Header(Align(*size));
  Free  += Length;
  Debug_Test(Free-Length);
  return Make_Pointer(TC_BIG_FIXNUM, Free-Length);
}

/* jpayne - this expects positive numbers and doesn't care if it does the
   wrong thing if the numbers are negative (sorry 'bout this) */

unsigned int Scheme_Integer_To_C_Unsigned_Integer(Arg1, C)
Pointer Arg1;
long *C;
{ int type = Type_Code(Arg1);
  fast bigdigit *SCAN, *ARG1;
  fast long Answer, i;
  long Length;

  if (type == TC_FIXNUM)
    { Sign_Extend(Arg1, *C);
      return PRIM_DONE;
    }
  if (type != TC_BIG_FIXNUM) return ERR_ARG_1_WRONG_TYPE;
  ARG1 = BIGNUM(Get_Pointer(Arg1));
  Length = LEN(ARG1);
  if (Length==0) Answer = 0;
  else if (Length > C_INTEGER_LENGTH_AS_BIGNUM)
    return ERR_ARG_1_BAD_RANGE;
  else if (Length <= C_INTEGER_LENGTH_AS_BIGNUM)
    for (SCAN=Bignum_Top(ARG1), i=0, Answer=0; i< Length; i++)
      Answer = Mul_Radix(Answer) + *SCAN--;
  if NEG_BIGNUM(ARG1) Answer = - Answer;
  *C = Answer;
  return PRIM_DONE;
}

/* Some unix utilities used by Common Lisp */

#define HOSTNAMELENGTH 100

Define_Primitive(prim_cl_get_host_name, 0, "CL-GET-HOST-NAME")
{ char name[HOSTNAMELENGTH];
  int length = HOSTNAMELENGTH;

  gethostname(name, length);

  return(C_String_To_Scheme_String(name));
}


#define VECTOR_LOC(vector, index) (Nth_Vector_Loc ((vector), ((index) + 1)))
#define MAXDIRENTRIES 4096

Define_Primitive(prim_cl_scan_directory, 1, "CL-SCAN-DIRECTORY")
{
  DIR *dirp;
  struct direct *dp;

  char *temp_table[MAXDIRENTRIES];
  char here[MAXPATHLEN];
  char *absolute_there[2*MAXPATHLEN];
  char *temp[MAXPATHLEN];
  char *there;

  long n_entries, i;
  Pointer result, *result_scan;
  
  Primitive_1_Arg();

  if (Type_Code(Arg1) != TC_CHARACTER_STRING)
  {
    CL_Error("Argument to CL-SCAN-DIRECTORY is not a string: ~A", 1, Arg1);
  }

  there = Scheme_String_To_C_String(Arg1);

  /* Get the absolute directory name */
  getwd(here);
  if (chdir(there))
  { 
    CL_Error("Couldn't connect to ~A", 1, Arg1);
  }
  getwd(absolute_there);
  chdir(here);

  /* A little formatting */
  strcat(absolute_there, "/");

  /* Open up the directory */
  dirp = opendir(there);
  if (!dirp)
  {
    CL_Error("Not enough UNIX memory to allocate directory entries.", 0);
  }

  /* Grab all the names and store them temporarily */
  for (i= 0, dp = readdir(dirp); dp != NULL; dp = readdir(dirp), i++)
  { temp_table[i] = dp->d_name;
  }
  n_entries = i;
  
  /* Check for enough memory for the vector of strings and for the strings themselves */
  Primitive_GC_If_Needed((2 + n_entries) + (n_entries * ((MAXPATHLEN / 4) + 2)));

  /* Allocate the result vector */
  result = allocate_marked_vector(TC_VECTOR, n_entries, false);

  /* Copy strings from the namelist into the vector, adding the absolute pathname and */
  /* Scheme-ifying them along the way  */
  result_scan = (VECTOR_LOC(result, 0));
  for (i = 0; i < n_entries; i++)
  {
    strcpy(temp, absolute_there);
    (*result_scan++) = C_String_To_Scheme_String(strcat(temp, temp_table[i]));
  }

  /* Close the directory and return */
  closedir(dirp);
  return(result);
}


Define_Primitive(prim_cl_directoryp, 1, "CL-DIRECTORY?")
{
  struct stat buf;
  int result;
  unsigned short mode;

  Primitive_1_Arg();
  
  if (Type_Code(Arg1) != TC_CHARACTER_STRING)
  {
    CL_Error("Argument to CL-FILE-MODE is not a string: ~A", 1, Arg1);
  }
  
  result = stat(Scheme_String_To_C_String(Arg1), &buf);
  
  if (result != 0)
  {
    CL_Error("stat(2) produced an error on ~A", 1, Arg1);
  }
  else 
  {
    mode = buf.st_mode;
    if (mode & S_IFDIR)
    {
      return(TRUTH);
    }
    else
    {
      return(NIL);
    }
  }
}

Define_Primitive(prim_sleep, 1, "CL-SLEEP")
{
  unsigned int seconds;
  Pointer result;
  Primitive_1_Arg();

  result = Scheme_Integer_To_C_Unsigned_Integer(Arg1, &seconds);

  if (result != PRIM_DONE)
  { Primitive_Error(result);
  }
  else
  { sleep(seconds);
    return NIL;
  }
}
