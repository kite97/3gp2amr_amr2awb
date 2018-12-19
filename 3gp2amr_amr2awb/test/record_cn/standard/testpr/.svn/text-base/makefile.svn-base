#
#  Copyright(C) 2000 EASTCOM-BUPT Inc.
#
#  Filename            : $RCSfile: makefile,v $
#  Last Revision       : $Revision: 1.5 $
#  Last Revision Date  : $Date: 2002/12/05 03:24:10 $
#  Description         :
#

EXE=${CINDIR}/sbin/siptest1/sip.$(FILE)

SRC_C=siptest.C
SRC_c=
SRC_ec=
SRC_pc=	

FILE=`if test "\`uname\`" = "HP-UX"; then echo sl; else echo so;fi`

MY_FLAGS = -I../include

include ../../makefile.incl
