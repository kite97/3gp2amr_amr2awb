CC = gcc
MAKEFILENAME = makefile

CLAMRDIR = ../../c_src/amr

CFLAGS = -O3 -Wall #-DVAD2
CFLAGS_CLAMR = -std=c99 $(CFLAGS) -I$(CLAMRDIR)

LDFLAGS = -lm

#
# source/object files
#
CLAMR_SRCS=\
$(CLAMRDIR)/sp_enc.c\
$(CLAMRDIR)/interf_enc.c\
$(CLAMRDIR)/sp_dec.c\
$(CLAMRDIR)/interf_dec.c\
$(CLAMRDIR)/dec_acelp.c\
$(CLAMRDIR)/dec_dtx.c\
$(CLAMRDIR)/dec_gain.c\
$(CLAMRDIR)/dec_if.c\
$(CLAMRDIR)/dec_lpc.c\
$(CLAMRDIR)/dec_main.c\
$(CLAMRDIR)/dec_rom.c\
$(CLAMRDIR)/dec_util.c\
$(CLAMRDIR)/enc_acelp.c\
$(CLAMRDIR)/enc_dtx.c\
$(CLAMRDIR)/enc_gain.c\
$(CLAMRDIR)/enc_if.c\
$(CLAMRDIR)/enc_lpc.c\
$(CLAMRDIR)/enc_main.c\
$(CLAMRDIR)/enc_rom.c\
$(CLAMRDIR)/enc_util.c\
$(CLAMRDIR)/if_rom.c

CLAMR_OBJS=$(CLAMR_SRCS:.c=.o)

ALL_SRCS=$(CLAMR_SRCS)
ALL_OBJS=$(CLAMR_OBJS)

all: amrnb_dec amrnb_enc amrwb_dec amrwb_enc amrnb_dec_raw amrwb_dec_raw
allobj: $(ALL_OBJS) amrnb_dec.o amrnb_enc.o amrwb_dec.o amrwb_enc.o amrnb_dec_raw.o amrwb_dec_raw.o

amrnb_dec.o: amrnb_dec.c $(CLAMRDIR)/typedef.h
	$(CC) -c $(CFLAGS_CLAMR) $< -o $@

amrnb_enc.o: amrnb_enc.c $(CLAMRDIR)/typedef.h
	$(CC) -c $(CFLAGS_CLAMR) $< -o $@

amrwb_dec.o: amrwb_dec.c $(CLAMRDIR)/typedef.h
	$(CC) -c $(CFLAGS_CLAMR) $< -o $@

amrwb_enc.o: amrwb_enc.c $(CLAMRDIR)/typedef.h
	$(CC) -c $(CFLAGS_CLAMR) $< -o $@

amrnb_dec_raw.o: amrnb_dec_raw.c $(CLAMRDIR)/typedef.h
	$(CC) -c $(CFLAGS_CLAMR) $< -o $@

amrwb_dec_raw.o: amrwb_dec_raw.c $(CLAMRDIR)/typedef.h
	$(CC) -c $(CFLAGS_CLAMR) $< -o $@

amrnb_dec: amrnb_dec.o $(CLAMR_OBJS)
	$(CC) $^ $(LDFLAGS) -o $@

amrnb_enc: amrnb_enc.o $(CLAMR_OBJS)
	$(CC) $^ $(LDFLAGS) -o $@

amrwb_dec: amrwb_dec.o $(CLAMR_OBJS)
	$(CC) $^ $(LDFLAGS) -o $@

amrwb_enc: amrwb_enc.o $(CLAMR_OBJS)
	$(CC) $^ $(LDFLAGS) -o $@

amrnb_dec_raw: amrnb_dec_raw.o $(CLAMR_OBJS)
	$(CC) $^ $(LDFLAGS) -o $@

amrwb_dec_raw: amrwb_dec_raw.o $(CLAMR_OBJS)
	$(CC) $^ $(LDFLAGS) -o $@

$(CLAMRDIR)/%.o: $(CLAMRDIR)/%.c
	$(CC) -c $(CFLAGS_CLAMR) $< -o $@

clean:
	rm -f $(CLAMR_OBJS) amrnb_dec.o amrnb_enc.o amrwb_dec.o amrwb_enc.o amrnb_dec_raw.o amrwb_dec_raw.o
	rm -f amrnb_dec amrnb_enc amrwb_dec amrwb_enc amrnb_dec_raw amrwb_dec_raw

cleanall: clean
	rm -f $(all)
