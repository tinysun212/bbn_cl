#                                 ********
#
# Copyright 1992 by BBN Systems and Technologies, A division of Bolt,
# Beranek and Newman Inc.
#
# Permission to use, copy, modify and distribute this software and its
# documentation is hereby granted without fee, provided that the above
# copyright notice and this permission appear in all copies and in
# supporting documentation, and that the name Bolt, Beranek and Newman
# Inc. not be used in advertising or publicity pertaining to distribution
# of the software without specific, written prior permission.  In
# addition, BBN makes no respresentation about the suitability of this
# software for any purposes.  It is provided "AS IS" without express or
# implied warranties including (but not limited to) all implied warranties
# of merchantability and fitness.  In no event shall BBN be liable for any
# special, indirect or consequential damages whatsoever resulting from
# loss of use, data or profits, whether in an action of contract,
# negligence or other tortuous action, arising out of or in connection
# with the use or performance of this software.
# 
#                                 ********
# 
##	Emacs: Use -*-Fundamental-*- mode and do not auto-fill!
#
#	$Header: Makefile.bfly,v 10.2 89/02/14 09:20:40 las Exp $
#	$MIT-Header: Makefile,v 9.65 88/03/16 00:02:00 GMT duck Exp $
#
#
###	MACHINE DEPENDENT PART	###
#
#
#	bfly (mach) version
#
#
#

CC = ccnew
CVT = /usr/blisp/tools/cvt/cvt
ECHO = echo

MACMACH = -Dmach -Dbutterfly -Dunix -Dbsd -DMC68020 -DSITE_INCLUDE_FILE=\"site.h\" \
	-DINHIBIT_FASL_VERSION_CHECK -DPARALLEL_PROCESSOR \
	-DUSE_SINGLE_HEAP -DELOG # -DMETERING

#
# *** NOTE ***
# These are the main lines to change.
# They reflect the machine and operating system type.
#

# The actual machine we are running on.

MAC = $(MACMACH)

MATH_LIB = -lm

# The terminal description library in use.  Usually -ltermcap.

TERM_LIB = -ltermcap

# Extra libraries needed for compatibility.

XTRA_LIB =

# Compiled code interface files.
# The defaults (commented out) are just stubs.

CSRC  = cmp68020.s			# compiler.c
CFILE = cmp68020.o			# compiler.oo
D_CFILE = cmp68020.o			# compiler.do
F_CFILE = cmp68020.o			# compiler.o
CFLAG = -g -Ox -DCMPGCFILE=\"cmp68kgc.h\"	# <nothing>
GC_HEAD_FILES= gccode.h cmp68kgc.h	# gccode.h

.s.o: ; @$(ECHO) "#**   Generating" $@ because of $?
	rm -f $*.o $*.m4
	m4 <$*.s >$*.m4
	@$(ECHO) "# as -m" $*.m4
	$(CVT) -a -o $*.o $*.m4


# Additional primitives required by the system

# These primitive files are for the default version of scheme

SYS_PRIM_SOURCES = unixprim.c # fhooks.c xdebug.c
SYS_PRIM_OBJECTS = unixprim.oo # fhooks.oo xdebug.oo

# These are for dscheme, a debugging version.  You don't want to use it.

D_SYS_PRIM_SOURCES = unixprim.c fhooks.c xdebug.c
D_SYS_PRIM_OBJECTS = unixprim.do fhooks.do xdebug.do

# These are for fscheme, a version with futures, a multiprocessing extension.

F_SYS_PRIM_SOURCES = unixprim.c fhooks.c future.c bf-interco.c xdebug.c \
	butterfly.c bf-io.c bf-prims.c bf-queue.c
F_SYS_PRIM_OBJECTS = unixprim.o fhooks.o future.o bf-interco.o xdebug.o \
	butterfly.o bf-io.o bf-prims.o bf-queue.o
#
#
#
#
###	INSTALLATION DEPENDENT PART	###
#
#

# Source directory and default runtime system image

SPATH = -DSCHEME_SOURCES_PATH=\"/usr/blisp/mach/runtime/\"
BAND = -DDEFAULT_BAND_NAME=\"/usr/blisp/mach/runtime/fscheme.band\"
CBAND = -DDEFAULT_COMPILER_BAND=\"/usr/blisp/mach/zcomp/fscheme-c.band\"

PATHS = $(SPATH) $(BAND) $(CBAND)

# User defined primitives.  Add/"uncomment" files as appropriate.

USER_PRIM_SOURCES = parallel.c ffuture.c cl-arith.c cl-fileio.c \
			cl-symbol.c cl-time.c gvector.c stringmisc.c \
			cl-pkgs.c bool.c values.c cl-generic.c cl-streams.c \
			cl-array.c cl-ivector.c codeman.c \
			servers.c fileser.c inser.c \
			trace.c elog.c # computil.c

USER_PRIM_OBJECTS = parallel.oo
D_USER_PRIM_OBJECTS = parallel.do
F_USER_PRIM_OBJECTS = parallel.o ffuture.o cl-arith.o cl-fileio.o \
			cl-symbol.o cl-time.o gvector.o stringmisc.o \
			cl-pkgs.o bool.o values.o cl-generic.o cl-streams.o \
			cl-array.o cl-ivector.o codeman.o \
			servers.o fileser.o inser.o \
			trace.o elog.o # computil.o

USER_LIB = # extra libraries needed by user primitives

getpagesize.h: pagesize.h
	rm -f getpagesize.h
	cp pagesize.h getpagesize.h

# Extra flags.  The stepper does not work, at present.

EFLAG = # -DCOMPILE_STEPPER

#
#
###	COMMON PART	###
#
#
#

#
# *** Important: ***
# You should not need to change anything below this point.
# All the machine & os/installation dependencies are above.
# 

LIB = $(USER_LIB) $(MATH_LIB) $(TERM_LIB) $(XTRA_LIB)

BASE_PRIM_SOURCES = $(SYS_PRIM_SOURCES) $(USER_PRIM_SOURCES)
BASE_PRIM_OBJECTS = $(SYS_PRIM_OBJECTS) $(USER_PRIM_OBJECTS)

# These primitive files are for the default version of scheme

PRIM_SOURCES = $(BASE_PRIM_SOURCES)
PRIM_OBJECTS = $(BASE_PRIM_OBJECTS) usrdef.o

# These are for dscheme, a debugging version.  You don't want to use it.

D_PRIM_SOURCES = $(D_SYS_PRIM_SOURCES) $(USER_PRIM_SOURCES)
D_PRIM_OBJECTS = $(D_SYS_PRIM_OBJECTS) $(D_USER_PRIM_OBJECTS) dusrdef.o

# These are for fscheme, a version with futures, a multiprocessing extension.

F_PRIM_SOURCES = $(F_SYS_PRIM_SOURCES) $(USER_PRIM_SOURCES)
F_PRIM_OBJECTS = $(F_SYS_PRIM_OBJECTS) $(F_USER_PRIM_OBJECTS) fusrdef.o

# These extra files are for 6001.

6001_SOURCES = $(BASE_PRIM_SOURCES) Sgraph.c
6001_OBJECTS = $(BASE_PRIM_OBJECTS) Sgraph.oo usr6001.oo
6001_LIB = -ltermcap -ldd300h -ldd98700 -ldd98710 -lsb1 -lsb2 $(MATH_LIB)

# These extra files are for 6003.

6003_SOURCES = $(BASE_PRIM_SOURCES) Sgraph.c Sgraph_xt.c fft.c array.c \
	image.c Sgraph_ar.c
6003_OBJECTS = $(BASE_PRIM_OBJECTS) Sgraph.oo Sgraph_xt.oo fft.oo array.oo \
	image.oo Sgraph_ar.oo usr6003.oo
6003_LIB = -ltermcap -ldd300h -ldd98700 -ldd98710 -lddhil -lsb1 -lsb2 $(MATH_LIB)

# These extra files are for KWH's development SCHEME

KWH_SOURCES = $(BASE_PRIM_SOURCES) bitops.c # Xlib.c Xrep.c 
KWH_OBJECTS = $(BASE_PRIM_OBJECTS) bitops.oo usrKWH.oo # Xlib.c Xrep.c 
KWH_LIB = -ltermcap -ldd300h -lddhil -lsb1 -lsb2 -lX $(MATH_LIB)

# Default switches

SWITCHES = $(MAC) $(PATHS) $(EFLAG) $(CFLAG)
PSWITCHES = -DCOMPILE_HISTORY $(SWITCHES)

# Size of the built-in primitive table.

NBUILT = 0x1b0

ASMSRC= $(CSRC) bf-gcloop.s
SRC0= interpret.c hooks.c utils.c
SRC1= fixnum.c hunk.c list.c \
      step.c string.c vector.c sysprim.c daemon.c
SRC2= prim.c random.c primutl.c extern.c
SRC3= lookup.c debug.c intern.c
SRC4= fasdump.c fasload.c
SRC5= bf-memmag.c bf-gc.c bf-purify.c comutl.c
SRC6= bignum.c flonum.c generic.c
SRC7= storage.c
SRC8= character.c fileio.c stringprim.c ttyio.c
SRC9= boot.c
SRC10= syntax.c bitstr.c regex.c rgxprim.c

SRC4bch = bchdmp.c fasload.c
SRC5bch = bchmmg.c bchgcl.c bchpur.c purutl.c comutl.c

SOURCES = $(SRC0) $(SRC1) $(SRC2) $(SRC3) $(SRC4) \
	  $(SRC5) $(SRC6) $(SRC7) $(SRC8) $(SRC9) \
	  $(SRC10)

OBJ0= interpret.oo hooks.oo utils.oo
OBJ1= fixnum.oo hunk.oo list.oo \
      step.oo string.oo vector.oo sysprim.oo daemon.oo
OBJ2= prim.oo random.oo primutl.oo extern.oo
OBJ3= lookup.oo debug.oo intern.oo
OBJ4= fasdump.oo fasload.oo
OBJ5= bf-memmag.oo bf-gc.oo bf-purify.oo purutl.oo comutl.oo
OBJ6= bignum.oo flonum.oo generic.oo
OBJ7= storage.oo
OBJ8= character.oo fileio.oo stringprim.oo ttyio.oo
OBJ9= boot.oo
OBJ10= syntax.oo bitstr.oo regex.oo rgxprim.oo

OBJECTS = $(OBJ0) $(OBJ1) $(OBJ2) $(OBJ3) $(OBJ4) \
	  $(OBJ5) $(OBJ6) $(OBJ7) $(OBJ8) $(OBJ9) \
	  $(OBJ10) os.oo

ASMOBJ = $(CFILE) bf-gcloop.o

OBJ4bch= bchdmp.oo fasload.oo
OBJ5bch= bchmmg.oo bchgcl.oo bchpur.oo purutl.oo comutl.oo

BCHOBJECTS = $(OBJ0) $(OBJ1) $(OBJ2) $(OBJ3) $(OBJ4bch) \
	     $(OBJ5bch) $(OBJ6) $(OBJ7) $(OBJ8) $(OBJ9) \
	     $(OBJ10) os.oo

OSWITCHES = $(SWITCHES) -DCOMPILE_HISTORY -O


OBJD0= interpret.do hooks.do utils.do bkpt.do
OBJD1= fixnum.do hunk.do list.do \
       step.do string.do vector.do sysprim.do daemon.do
OBJD2= prim.do random.do primutl.do extern.do
OBJD3= lookup.do debug.do intern.do
OBJD4= fasdump.do fasload.do
OBJD5= bf-memmag.do bf-gc.do bf-purify.do purutl.do comutl.do
OBJD6= bignum.do flonum.do generic.do
OBJD7= storage.do
OBJD8= character.do fileio.do stringprim.do ttyio.do
OBJD9= boot.do
OBJD10= syntax.do bitstr.do regex.do rgxprim.do

D_OBJECTS = $(OBJD0) $(OBJD1) $(OBJD2) $(OBJD3) $(OBJD4) \
	    $(OBJD5) $(OBJD6) $(OBJD7) $(OBJD8) $(OBJD9) \
	    $(OBJD10) os.do

D_ASMOBJ = $(D_CFILE) bf-gcloop.o

DSWITCHES = -g -Ox $(SWITCHES) -DENABLE_DEBUGGING_TOOLS

OBJF0= interpret.o hooks.o utils.o
OBJF1= fixnum.o hunk.o list.o \
       step.o string.o vector.o sysprim.o daemon.o
OBJF2= prim.o random.o primutl.o extern.o
OBJF3= lookup.o debug.o intern.o
OBJF4= fasdump.o fasload.o
OBJF5= bf-memmag.o bf-gc.o bf-purify.o comutl.o
OBJF6= bignum.o flonum.o generic.o
OBJF7= storage.o
OBJF8= character.o fileio.o stringprim.o ttyio.o
OBJF9= boot.o
OBJF10= syntax.o bitstr.o regex.o rgxprim.o

F_OBJECTS = $(OBJF0) $(OBJF1) $(OBJF2) $(OBJF3) $(OBJF4) \
	    $(OBJF5) $(OBJF6) $(OBJF7) $(OBJF8) $(OBJF9) \
	    $(OBJF10) os.o

F_ASMOBJ = $(F_CFILE) bf-gcloop.o # sbrk.o

FSWITCHES = $(SWITCHES) -DCOMPILE_FUTURES
# -DUSE_STACKLETS -DENABLE_DEBUGGING_TOOLS -DFUTURE_LOGGING


# This tells make how to generate the scheme object files

.SUFFIXES: .oo .do .fo .o .s .opp .dpp .fpp
.c.opp: ; @$(ECHO) "#** Generating" $@
	make Breakup
	cc -E $(OSWITCHES) $*.c | \
	Breakup | \
	sed -e 's/^#.*//' -e 's/^[ 	]*$$//' -e 's/^$$//' | \
	sed -n -e '/^..*/p' > $*.opp
.c.dpp: ; @$(ECHO) "#** Generating" $@
	make Breakup
	cc -E $(DSWITCHES) -DDEBUG $*.c | \
	Breakup | \
	sed -e 's/^#.*//' -e 's/^[ 	]*$$//' -e 's/^$$//' | \
	sed -n -e '/^..*/p' > $*.dpp
.c.fpp: ; @$(ECHO) "#** Generating" $@
	make Breakup
	cc -E $(FSWITCHES) $*.c | \
	Breakup | \
	sed -e 's/^#.*//' -e 's/^[ 	]*$$//' -e 's/^$$//' | \
	sed -n -e '/^..*/p' > $*.fpp
.c.oo: ; @$(ECHO) "#**   Generating" $@ because of $?
	$(CC) -c $(OSWITCHES) $*.c
	rm -f $*.oo
	mv $*.o $*.oo
.c.do: ; @$(ECHO) "#**   Generating" $@ because of $?
	$(CC) -c $(DSWITCHES) $*.c
	rm -f $*.do
	mv $*.o $*.do

# The following used to be .c.fo

.c.o: ; @$(ECHO) "#**   Generating" $@ because of $?
	rm -f $*.o
	$(CC) -c $(FSWITCHES) $*.c

#.c.o: ; @$(ECHO) "#**   Generating" $@ because of $?
#	$(CC) -c $(PSWITCHES) $*.c
.c.s: ; @$(ECHO) "#**   Generating" $@
	$(CC) -S $(OSWITCHES) $*.c

# Here are the main blocks to make

all: fscheme

world: all scm6001 scm6003 bchscheme # kwhscheme

everything: world Bintopsb Ppband

remove:
	rm -f *.o *.oo *.do *.o *.a68 bf-gcloop.m4 cmp68020.m4 zero_car_cdr.m4
	rm -f scheme bchscheme dscheme fscheme scm6001 scm6003 kwhscheme
	rm -f Findprim Bintopsb Psbtobin Breakup Wsize Ppband
	rm -f usrdef.c dusrdef.c fusrdef.c usr6001.c usr6003.c usrKWH.c
	rm -f scheme.touch os.touch translate.touch usrdef.touch
	rm -f lint.out lint.dout lint.out

relink:
	@$(ECHO) "#**   Re-linking" $@ because of user request
	rm -f fscheme
	ld -o fscheme zero_car_cdr.o /lib/crt0.o $(F_OBJECTS) \
		$(F_PRIM_OBJECTS) $(F_ASMOBJ) -lmach $(LIB) -lc $(LD_SWITCH)

tcpem:	tcpem.o inser.o outser.o
	@$(ECHO) "#**   Re-linking" $@ because of $?
	rm -f tcpem
	$(CC) -o tcpem tcpem.o inser.o outser.o

scheme: $(OBJECTS) $(PRIM_OBJECTS) $(ASMOBJ)
	@$(ECHO) "#**   Re-linking" $@ because of $?
	rm -f scheme
	$(CC) -o scheme $(OBJECTS) \
		$(PRIM_OBJECTS) $(ASMOBJ) $(LIB) $(LD_SWITCH)

bchscheme: $(BCHOBJECTS) $(PRIM_OBJECTS) $(ASMOBJ)
	@$(ECHO) "#**   Re-linking" $@ because of $?
	rm -f bchscheme
	$(CC) -o bchscheme $(BCHOBJECTS) \
		$(PRIM_OBJECTS) $(ASMOBJ) $(LIB) $(LD_SWITCH)

scm6001: $(OBJECTS) $(6001_OBJECTS) $(ASMOBJ)
	@$(ECHO) "#**   Re-linking" $@ because of $?
	rm -f scm6001
	$(CC) -o scm6001 $(OBJECTS) \
		$(6001_OBJECTS) $(ASMOBJ) $(6001_LIB) $(LD_SWITCH)

scm6003: $(OBJECTS) $(6003_OBJECTS) $(ASMOBJ)
	@$(ECHO) "#**   Re-linking" $@ because of $?
	rm -f scm6003
	$(CC) -o scm6003 $(OBJECTS) \
		$(6003_OBJECTS) $(ASMOBJ) $(6003_LIB) $(LD_SWITCH)

kwhscheme: $(BCHOBJECTS) $(KWH_OBJECTS) $(ASMOBJ)
	@$(ECHO) "#**   Re-linking" $@ because of $?
	rm -f kwhscheme
	$(CC) -o kwhscheme $(BCHOBJECTS) \
		$(KWH_OBJECTS) $(ASMOBJ) $(KWH_LIB) $(LD_SWITCH)

dscheme: $(D_OBJECTS) $(D_PRIM_OBJECTS) $(D_ASMOBJ)
	@$(ECHO) "#**   Re-linking" $@ because of $?
	rm -f dscheme
	$(CC) -g -Ox -o dscheme $(D_OBJECTS) \
		$(D_PRIM_OBJECTS) $(D_ASMOBJ) $(LIB) $(LD_SWITCH)

fscheme: $(PARALLEL) zero_car_cdr.o $(F_OBJECTS) $(F_PRIM_OBJECTS) $(F_ASMOBJ)
	@$(ECHO) "#**   Re-linking" $@ because of $?
	rm -f fscheme
	ld -o fscheme zero_car_cdr.o /lib/crt0.o $(F_OBJECTS) \
		$(F_PRIM_OBJECTS) $(F_ASMOBJ) -ldbfork -lmach $(LIB) -lc -lelog $(LD_SWITCH)

ajcscheme: $(F_OBJECTS) $(F_PRIM_OBJECTS) $(F_ASMOBJ)
	@$(ECHO) "#**   Re-linking" $@ because of $?
	rm -f ajcscheme
	$(CC) -g -o ajcscheme $(F_OBJECTS) \
		$(F_PRIM_OBJECTS) $(F_ASMOBJ) $(LIB) $(LD_SWITCH)

# Primitive table files

# The first two are for VMS.

usrdef.txt:
	make_vmslist usrdef.txt $(PRIM_SOURCES) $(SOURCES)

vmsusrdef.c:
	echo "$$ Findprim -o usrdef.c -l [-.vms]usrdef.txt"
	$(CC) -c $(OSWITCHES) usrdef.c
#
# These should depend on SOURCES, but these change rarely,
# so they are left out to be generated by hand.
#

usrdef.c: $(PRIM_SOURCES) usrdef.touch Findprim  # $(SOURCES)
	@$(ECHO) "#**   Re-making" $@ because of $?
	rm -f usrdef.c
	./Findprim $(PRIM_SOURCES) $(SOURCES) >usrdef.c

dusrdef.c: $(D_PRIM_SOURCES) usrdef.touch Findprim  # $(SOURCES)
	@$(ECHO) "#**   Re-making" $@ because of $?
	rm -f dusrdef.c
	./Findprim $(D_PRIM_SOURCES) $(SOURCES) >dusrdef.c

fusrdef.c: $(F_PRIM_SOURCES) usrdef.touch Findprim  $(SOURCES)
	@$(ECHO) "#**   Re-making" $@ because of $?
	rm -f fusrdef.c
	./Findprim $(F_PRIM_SOURCES) $(SOURCES) >fusrdef.c

usr6001.c: $(6001_SOURCES) usrdef.touch Findprim  # $(SOURCES)
	@$(ECHO) "#**   Re-making" $@ because of $?
	rm -f usr6001.c
	./Findprim $(6001_SOURCES) $(SOURCES) >usr6001.c

usr6003.c: $(6003_SOURCES) usrdef.touch Findprim  # $(SOURCES)
	@$(ECHO) "#**   Re-making" $@ because of $?
	rm -f usr6003.c
	./Findprim $(6003_SOURCES) $(SOURCES) >usr6003.c

usrKWH.c: $(KWH_SOURCES) usrdef.touch Findprim  # $(SOURCES)
	@$(ECHO) "#**   Re-making" $@ because of $?
	rm -f usrKWH.c
	./Findprim $(KWH_SOURCES) $(SOURCES) >usrKWH.c

Findprim: Findprim.c
	@$(ECHO) "#**   Re-generating" $@ because of $?
	rm -f Findprim
	$(CC) $(MAC) -o Findprim Findprim.c

Bintopsb: translate.touch trap.h fasl.h load.c bltdef.h Bintopsb.c
	@$(ECHO) "#**   Re-making" $@ because of $?
	rm -f Bintopsb
	$(CC) $(CFLAG) $(MAC) -O -o Bintopsb Bintopsb.c $(MATH_LIB)

Psbtobin: translate.touch fasl.h dump.c Psbtobin.c
	@$(ECHO) "#**   Re-making" $@ because of $?
	rm -f Psbtobin
	$(CC) $(CFLAG) $(MAC) -O -o Psbtobin Psbtobin.c $(MATH_LIB)

Breakup: Breakup.c
	rm -f Breakup
	$(CC) $(MAC) -O -o Breakup Breakup.c

Wsize: Wsize.c
	rm -f Wsize
	$(CC) $(MAC) -o Wsize Wsize.c $(MATH_LIB)

Ppband: config.h types.h const.h object.h sdata.h fasl.h load.c Ppband.c
	rm -f Ppband
	$(CC) $(MAC) -o Ppband Ppband.c

# scheme.touch depends also on butterfly.h rename.c

scheme.touch: scheme.h config.h bkpt.h object.h scode.h sdata.h	\
	gc.h interpret.h stack.h futures.h types.h errors.h returns.h \
	const.h fixobj.h default.h extern.h prim.h interrupt.h \
	butterfly.h
	@$(ECHO) "#**   Resetting" $@ because of $?
	touch scheme.touch

os.touch: os.c unix.c scheme.touch zones.h
	@$(ECHO) "#**   Resetting" $@ because of $?
	touch os.touch

translate.touch: config.h object.h bignum.h bitstr.h types.h \
	sdata.h const.h character.h missing.c translate.h $(GC_HEAD_FILES) \
	comlin.h comlin.c
	@$(ECHO) "#**   Resetting" $@ because of $?
	touch translate.touch

usrdef.touch: usrdef.h config.h object.h prim.h
	@$(ECHO) "#**   Resetting" $@ because of $?
	touch usrdef.touch

HEAD_FILES = scheme.touch primitive.h zones.h locks.h \
	flonum.h bignum.h stringprim.h character.h \
	$(GC_HEAD_FILES) trap.h lookup.h history.h cmpint.h

lint.out: $(SOURCES) $(PRIM_SOURCES) $(ASMSRC) usrdef.c $(HEAD_FILES)
	rm -f lint.out
	lint $(OSWITCHES) $(SOURCES) $(PRIM_SOURCES) $(ASMSRC) usrdef.c >lint.out

lint.dout: $(SOURCES) $(D_PRIM_SOURCES) $(ASMSRC) dusrdef.c $(HEAD_FILES)
	rm -f lint.dout
	lint $(DSWITCHES) $(SOURCES) $(D_PRIM_SOURCES) $(ASMSRC) dusrdef.c \
	>lint.dout

lint.out: $(SOURCES) $(F_PRIM_SOURCES) $(ASMSRC) fusrdef.c $(HEAD_FILES)
	rm -f lint.out
	lint $(FSWITCHES) $(SOURCES) $(F_PRIM_SOURCES) $(ASMSRC) fusrdef.c \
	>lint.out

# Here are the source file dependencies on the header files

# The files in OBJ0, etc. are explicitly listed below.
$(OBJF1) $(OBJD1) $(OBJ1): scheme.touch primitive.h
$(OBJF2) $(OBJD2) $(OBJ2): scheme.touch primitive.h
$(OBJF3) $(OBJD3) $(OBJ3): scheme.touch lookup.h trap.h primitive.h locks.h
$(OBJF4) $(OBJD4) $(OBJ4): scheme.touch primitive.h trap.h $(GC_HEAD_FILES) \
	fasl.h load.c dump.c
$(OBJF5) $(OBJD5) $(OBJ5): scheme.touch primitive.h $(GC_HEAD_FILES) zones.h
$(OBJF6) $(OBJD6) $(OBJ6): scheme.touch primitive.h flonum.h zones.h bignum.h
$(OBJF7) $(OBJD7) $(OBJ7): scheme.touch gctype.c
$(OBJF8) $(OBJD8) $(OBJ8): scheme.touch primitive.h stringprim.h character.h
$(OBJF9) $(OBJD9) $(OBJ9): scheme.touch primitive.h version.h character.h \
	stringprim.h 
$(PRIM_OBJECTS) $(D_PRIM_OBJECTS) $(F_PRIM_OBJECTS): $(HEAD_FILES)
$(6001_OBJECTS) $(6003_OBJECTS) $(KWH_OBJECTS): $(HEAD_FILES)

#
# These files have explicit dependencies
#

interpret.oo interpret.do interpret.o: scheme.touch locks.h trap.h \
	lookup.h  history.h cmpint.h zones.h

hooks.oo hooks.do hooks.o: scheme.touch primitive.h winder.h history.h

utils.oo utils.do utils.o: scheme.touch primitive.h flonum.h winder.h \
	history.h cmpint.h

bkpt.do: scheme.touch

os.oo os.do os.o: scheme.touch os.touch zones.h

compiler.oo compiler.do compiler.o: config.h object.h sdata.h \
	types.h errors.h const.h returns.h

bchmmg.oo bchgcl.oo bchpur.oo: scheme.touch primitive.h bchgcc.h \
	$(GC_HEAD_FILES)

bchdmp.oo: scheme.touch primitive.h bchgcc.h $(GC_HEAD_FILES) fasl.h dump.c

Sgraph.oo Sgraph.do Sgraph.o: Sgraph.h
Sgraph_xt.oo Sgraph_xt.do Sgraph_xt.o: Sgraph.h
Sgraph_ar.oo Sgraph_ar.do Sgraph_ar.o: Sgraph.h array.h
fft.oo fft.do fft.o: array.h
array.oo array.do array.o: array.h
image.oo image.do image.o: array.h image.h
hooks.oo hooks.do hooks.o: winder.h
utils.oo utils.do utils.o: winder.h

syntax.oo syntax.do syntax.o: scheme.touch primitive.h stringprim.h \
	character.h edwin.h syntax.h
bitstr.oo bitstr.do bitstr.o: scheme.touch primitive.h bignum.h bitstr.h
regex.oo regex.do regex.o: scheme.touch character.h syntax.h regex.h
rgxprim.oo rgxprim.do rgxprim.o: scheme.touch primitive.h stringprim.h \
	character.h edwin.h syntax.h regex.h

# Butterfly dependencies

bf-memmag.o: scheme.touch locks.h
bf-gc.o: scheme.touch primitive.h gccode.h transact.h locks.h zones.h
bf-interco.o: scheme.touch primitive.h locks.h zones.h transact.h
bf-io.o: scheme.touch character.h primitive.h transact.h
bf-meter.o: scheme.touch primitive.h transact.h
butterfly.o: scheme.touch primitive.h zones.h locks.h fasl.h
codeman.o: scheme.touch primitive.h cmp68kgc.h zones.h transact.h
fasload.o: scheme.touch primitive.h gccode.h trap.h cl-symbol.h \
	   transact.h load.c
fileser.o: scheme.touch transact.h
inser.o: interrupt.h transact.h
os.o: cl-fileio.h zones.h transact.h
outser.o: transact.h
servers.o: scheme.touch transact.h
tcpem.o: interrupt.h transact.h

# Lint crud

lint.out lint.dout lint.out:					\
	scheme.touch primitive.h fasl.h				\
	flonum.h bignum.h locks.h zones.h  unexec.c

