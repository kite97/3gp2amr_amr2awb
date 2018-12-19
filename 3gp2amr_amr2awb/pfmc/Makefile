#
#  Copyright(C) 2010 EASTCOM-BUPT Inc.
#
#  Filename            : $RCSfile: makefile.normal,v $
#  Last Revision       : $Revision: 1.2 $
#  Last Revision Date  : $Date: 2012/09/20 06:47:52 $
#  Description         : 
#
SRC_DIR = src
SRC_C = \
       	$(SRC_DIR)/pfmclog.C\
	$(SRC_DIR)/filemanager.C\
       	$(SRC_DIR)/pfmcmessage.C\
       	$(SRC_DIR)/udpsender.C\
       	$(SRC_DIR)/pfmcmanager.C\
       	$(SRC_DIR)/interface.C\
	$(SRC_DIR)/pfmc.C

OBJS=$(SRC_C:.C=.o)

CXXFLAGS = -I./include -O2


.SUFFIXES: .C .o .h

all: pfmc

pfmc: $(OBJS)
	g++ -o $@ $(OBJS)

.C.o:
	@echo compiling $< "...\c"
	g++ $(CXXFLAGS) -o $@ -c $<
	@echo OK

clean:
	@rm -vf pfmc $(OBJS)
