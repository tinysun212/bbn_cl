;;;                                ********
;;;
;;; Copyright 1992 by BBN Systems and Technologies, A division of Bolt,
;;; Beranek and Newman Inc.
;;; 
;;; Permission to use, copy, modify and distribute this software and its
;;; documentation is hereby granted without fee, provided that the above
;;; copyright notice and this permission appear in all copies and in
;;; supporting documentation, and that the name Bolt, Beranek and Newman
;;; Inc. not be used in advertising or publicity pertaining to distribution
;;; of the software without specific, written prior permission.  In
;;; addition, BBN makes no respresentation about the suitability of this
;;; software for any purposes.  It is provided "AS IS" without express or
;;; implied warranties including (but not limited to) all implied warranties
;;; of merchantability and fitness.  In no event shall BBN be liable for any
;;; special, indirect or consequential damages whatsoever resulting from
;;; loss of use, data or profits, whether in an action of contract,
;;; negligence or other tortuous action, arising out of or in connection
;;; with the use or performance of this software.
;;; 
;;;                                 ********
;;; 
;;;;;; -*- Scheme -*-

(declare (usual-integrations))

(set-working-directory-pathname! "/usr/blisp/mach/runtime")

(fluid-let (((access compiler:touchify? compiler-package) true))
  ((access compiler:batch-compile compiler-package)
   '(
     "bf-futu"
     "bf-hacks"
     "bf-hard"
     "bf-io"
     "bf-load"
     "bf-rep"
     "bf-repu"
     "bf-trace"
     "bitstr"
     "boot"
     "cdebug"
     "char"
     "cl-decls"
     "cl-optimp"
     "cl-syntax"
     "comand"
     "crock"
     "datime"
     "debug"
     "defstr"
     "defstru"
     "equals"
     "fixart"
     "format"
     "future"
     "future1"
     "future2"
     "future3"
     "future4"
     "future5"
     "gensym"
     "hash"
     "histry"
     "implmd"
     "input"
     "io"
     "lambda"
     "list"
     "macros"
     "msort"
     "narith"
     "numpar"
     "numunp"
     "output"
     "parse1"
     "parse2"
     "parse3"
     "pp"
     "repuse"
     "resource"
     "scan"
     "scode"
     "scomb"
     "sdata"
     "sfile"
     "spmd"
     "string"
     "stypes"
     "syntax"
     "syntax1"
     "sysclk"
     "system"
     "types"
     "unpars"
     "unsyn"
     "ustruc"
     "utabs"
     "vector"
     "where"
     "which"
     "wind"
     "world"
     "xlist"
     )))

(set-working-directory-pathname! "/usr/blisp/mach/sf")

(fluid-let (((access compiler:touchify? compiler-package) false))
  ((access compiler:batch-compile compiler-package)
   '(
     "cgen"
     "chtype"
     "copy"
     "emodel"
     "eqsets"
     "free"
     "gconst"
     "make"
     "mvalue"
     "object"
     "packag"
     "pardec"
     "pthmap"
     "subst"
     "tables"
     "toplev"
     "usicon"
     "usiexp"
     "xform"
     )))

(set-working-directory-pathname! "/usr/blisp/mach/zcomp")

(fluid-let (((access compiler:touchify? compiler-package) false))
  ((access compiler:batch-compile compiler-package)
   '(
     "base/object"
     "machines/butterfly/insmac"
     "base/pmlook"
     "base/infutl"
     "machines/butterfly/dassm1"
     "base/toplev"
     "rtlbase/regset"
     "machines/butterfly/dassm3"
     "base/hashtb"
     "base/scode"
     "base/mvalue"
     "machines/butterfly/inerly"
     "base/debug"
     "base/queue"
     "base/pmerly"
     "machines/butterfly/coerce"
     "base/sets"
     "base/enumer"
     "machines/butterfly/rgspcm"
     "machines/butterfly/dassm2"
     "base/pmpars"
     "back/asmmac"
     "base/infnew"
     "base/utils"
     "base/cfg1"
     "rtlbase/rtlobj"
     "base/cfg3"
     "base/cfg2"
     "base/contin"
     "base/lvalue"
     "base/blocks"
     "base/subprb"
     "base/rvalue"
     "rtlbase/rtlcfg"
     "base/ctypes"
     "base/proced"
     "rtlbase/rtlty1"
     "machines/butterfly/assmd"
     "back/insseq"
     "rtlbase/rtlreg"
     "rtlbase/rgraph"
     "machines/butterfly/machin"
     "rtlbase/rtlty2"
     "back/bitutl"
     "fgopt/folcon"
     "back/symtab"
     "fgopt/closan"
     "rtlbase/rtline"
     "fgopt/outer"
     "back/lapgn3"
     "machines/butterfly/insutl"
     "fgopt/contan"
     "fgopt/simple"
     "back/regmap"
     "fggen/fggen"
     "rtlbase/rtlcon"
     "fgopt/operan"
     "fgopt/offset"
     "fggen/declar"
     "back/linear"
     "fgopt/simapp"
     "fgopt/conect"
     "fgopt/order"
     "back/lapgn2"
     "fgopt/blktyp"
     "rtlbase/rtlexp"
     "machines/butterfly/lapgen"
     "fgopt/desenv"
     "back/syerly"
     "rtlgen/rgstmt"
     "rtlopt/rcsesr"
     "back/bittop"
     "machines/butterfly/instr2"
     "rtlopt/rcse2"
     "rtlgen/rgcomb"
     "machines/butterfly/rules1"
     "rtlopt/rcseht"
     "machines/butterfly/instr4"
     "rtlgen/rgretn"
     "machines/butterfly/rules3"
     "back/syntax"
     "rtlopt/ralloc"
     "rtlopt/rdebug"
     "rtlgen/fndblk"
     "rtlopt/rlife"
     "back/lapgn1"
     "rtlgen/opncod"
     "machines/butterfly/instr1"
     "rtlopt/rcse1"
     "rtlgen/rgrval"
     "machines/butterfly/rules4"
     "rtlopt/rcserq"
     "rtlgen/rgproc"
     "machines/butterfly/rules2"
     "machines/butterfly/instr3"
     "rtlopt/rcseep"
     "rtlgen/rtlgen"
     "rtlopt/rdeath"
     "bbn-inline-code"
     "touchify"

     ;; Questionable

     "base/decls"
     "base/pbs"
     "base/switch"
     "base/macros"
     "base/pkging"
     "etc/direct"
     "etc/butils"
     "machines/butterfly/make"

     ;; May not make it

     "fltasm"

     )))