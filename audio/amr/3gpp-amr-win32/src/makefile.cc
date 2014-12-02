#****************************************************************
#
#      GSM AMR-NB speech codec   R98   Version 7.6.0   December 12, 2001
#                                R99   Version 3.3.0                
#                                REL-4 Version 4.1.0                
#
#****************************************************************
#
#      File             : makefile
#      Purpose          : cc makefile for AMR SPC fixed point library
#                       : and standalone encoder/decoder program
#
#                             make [MODE=DEBUG] [VAD=VAD#] [target [target...]]
#
#                         Important targets are:
#                             default           (same as not specifying a
#                                                target at all)
#                                               remove all objects and libs;
#                                               build libraries; then build
#                                               encoder & decoder programs
#                             depend            make new dependency list
#                             clean             Remove all object/executable/
#                                               verification output files
#                             clean_depend      Clean dependency list
#                             clean_all         clean & clean_depend & rm *.a
#
#
#                         Specifying MODE=DEBUG compiles in debug mode
#                         (libaries compiled in DEBUG mode will be linked)
#                         Specifying MODE=WMOPS enables WMOPS counting
#                         (FIP operation library compiled in WMOPS mode will
#                          be linked)
#
#                         Specifying VAD=VAD1 compiles VAD option 1
#                         Specifying VAD=VAD2 compiles VAD option 2
#
#                         The makefile uses the GNU C compiler (gcc); change
#                         the line CC=gcc below if another compiler is desired
#                         (CFLAGSxxx probably must be changed then as well)
#                         
#
# $Id $
#
#****************************************************************

CC = CC
MAKEFILENAME = makefile.cc

# Use MODE=DEBUG for debuggable library (default target builds both)
#
# default mode = NORM ==> no debug, no wmops
#
MODE=NORM

# Use VAD=VAD1 for VAD option 1, or VAD=VAD2 for VAD option 2
#
# default mode = VAD1
#
VAD=VAD1

#
# compiler flags (for normal, DEBUG, and WMOPS compilation)
#
CFLAGS_NORM  = -O4 -DWMOPS=0
CFLAGS_DEBUG = -g -DDEBUG -DWMOPS=0
CFLAGS_WMOPS = -O4 -DWMOPS=1

CFLAGS = -I. $(CFLAGS_$(MODE)) -D$(VAD)
CFLAGSDEPEND = -MM $(CFLAGS)                    # for make depend


TMP=$(MODE:NORM=)
TMP2=$(TMP:DEBUG=_debug)
#
# construct SPC library name:
#   spc.a        in normal or wmops mode
#   spc_debug.a  in debug mode (MODE=DEBUG)
#
SPCLIB=spc$(TMP2:WMOPS=).a

#
# construct FIP operation library name:
#   fipop.a        in normal mode
#   fipop_debug.a  in debug mode (MODE=DEBUG)
#   fipop_wmops.a  in wmops mode (MODE=WMOPS)
#
FIPOPLIB=fipop$(TMP2:WMOPS=_wmops).a


#
# source/object files
#
SPC_OBJS=  agc.o autocorr.o az_lsp.o bits2prm.o \
       cl_ltp.o convolve.o c1035pf.o d_plsf.o d_plsf_5.o \
       d_gain_c.o d_gain_p.o dec_lag6.o d1035pf.o cor_h.o \
       enc_lag3.o enc_lag6.o g_code.o g_pitch.o int_lpc.o \
       inter_36.o inv_sqrt.o \
       lag_wind.o levinson.o lsp_az.o lsp_lsf.o ol_ltp.o \
       pitch_fr.o pitch_ol.o pow2.o pre_big.o pre_proc.o pred_lt.o preemph.o \
       prm2bits.o \
       pstfilt.o q_gain_c.o q_gain_p.o q_plsf.o q_plsf_5.o lsfwt.o reorder.o \
       residu.o lsp.o lpc.o ec_gains.o spreproc.o syn_filt.o \
       weight_a.o qua_gain.o gc_pred.o q_plsf_3.o post_pro.o \
       dec_lag3.o dec_gain.o d_plsf_3.o d4_17pf.o c4_17pf.o d3_14pf.o \
       c3_14pf.o \
       d2_11pf.o c2_11pf.o d2_9pf.o c2_9pf.o cbsearch.o spstproc.o gain_q.o \
       cod_amr.o dec_amr.o sp_enc.o sp_dec.o ph_disp.o \
       g_adapt.o calc_en.o qgain795.o qgain475.o sqrt_l.o set_sign.o s10_8pf.o \
       bgnscd.o gmed_n.o \
       mac_32.o ex_ctrl.o c_g_aver.o lsp_avg.o int_lsf.o c8_31pf.o d8_31pf.o \
       p_ol_wgh.o ton_stab.o vad1.o dtx_enc.o dtx_dec.o a_refl.o \
       b_cn_cod.o calc_cor.o hp_max.o vadname.o \
       vad2.o r_fft.o lflg_upd.o \
       e_homing.o d_homing.o


ENCODER_SRCS=coder.c 
DECODER_SRCS=decoder.c
FIPOP_SRCS=basicop2.c count.c oper_32b.c copy.c log2.c set_zero.c \
           strfunc.c n_proc.c sid_sync.c

ENCODER_OBJS=$(ENCODER_SRCS:.c=.o) 
DECODER_OBJS=$(DECODER_SRCS:.c=.o)
FIPOP_OBJS=$(FIPOP_SRCS:.c=.o)

ALL_SRCS=$(ENCODER_SRCS) $(DECODER_SRCS) $(FIPOP_SRCS) $(SPC_OBJS:.o=.c)

#
# default target: build standalone speech encoder and decoder
#
default: clean_all spclib fipoplib encoder decoder


encoder: $(ENCODER_OBJS) $(SPCLIB) $(FIPOPLIB)
	$(CC) -o encoder $(CFLAGS) $(ENCODER_OBJS) $(SPCLIB) $(FIPOPLIB) $(LDFLAGS)

decoder: $(DECODER_OBJS) $(SPCLIB) $(FIPOPLIB)
	$(CC) -o decoder $(CFLAGS) $(DECODER_OBJS) $(SPCLIB) $(FIPOPLIB) $(LDFLAGS)




#
# how to compile a .c file into a .o
#
.SUFFIXES: .c .h .o
.c.o:
	$(CC) -c $(CFLAGS) $<


#
# build normal and DEBUG version of SPC library from scratch
#
spclib_allmodes:
	rm -f spc.a spc_debug.a
	$(MAKE) -f $(MAKEFILENAME) $(MFLAGS) $(MAKEDEFS) MODE=      clean spclib
	$(MAKE) -f $(MAKEFILENAME) $(MFLAGS) $(MAKEDEFS) MODE=DEBUG clean spclib

#
# build the speech coder library
#
spclib: $(SPC_OBJS)
	$(AR) rc $(SPCLIB) $(SPC_OBJS)
	ranlib $(SPCLIB)


#
# build normal, DEBUG, and WMOPS version of FIP operation library from scratch
#
fipoplib_allmodes:
	rm -f fipop.a fipop_debug.a fipop_wmops.a
	$(MAKE) -f $(MAKEFILENAME) $(MFLAGS) $(MAKEDEFS)            clean fipoplib
	$(MAKE) -f $(MAKEFILENAME) $(MFLAGS) $(MAKEDEFS) MODE=DEBUG clean fipoplib
	$(MAKE) -f $(MAKEFILENAME) $(MFLAGS) $(MAKEDEFS) MODE=WMOPS clean fipoplib

#
# build the FIP operation library
#
fipoplib:	$(FIPOP_OBJS)
	$(AR) rc $(FIPOPLIB) $(FIPOP_OBJS)
	ranlib $(FIPOPLIB)


#
# make / clean dependency list
#
depend:
	$(MAKE) -f $(MAKEFILENAME) $(MFLAGS) $(MAKEDEFS) clean_depend
	$(CC) $(CFLAGSDEPEND) $(ALL_SRCS) >> $(MAKEFILENAME)

clean_depend:
	chmod u+w $(MAKEFILENAME)
	(awk 'BEGIN{f=1}{if (f) print $0}/^\# DO NOT DELETE THIS LINE -- make depend depends on it./{f=0}'\
	    < $(MAKEFILENAME) > .depend && \
	mv .depend $(MAKEFILENAME)) || exit 1;

#
# remove object/executable files
#
clean:
	rm -f *.o core

clean_all: clean
	rm -f *.a encoder decoder

# DO NOT DELETE THIS LINE -- make depend depends on it.
coder.o: coder.c typedef.h typedefs.h cnst.h n_proc.h mode.h frame.h \
 strfunc.h sp_enc.h pre_proc.h cod_amr.h lpc.h levinson.h lsp.h \
 q_plsf.h cl_ltp.h pitch_fr.h ton_stab.h gain_q.h gc_pred.h g_adapt.h \
 p_ol_wgh.h vad.h vad1.h cnst_vad.h vad2.h dtx_enc.h sid_sync.h \
 vadname.h e_homing.h
decoder.o: decoder.c typedef.h typedefs.h n_proc.h cnst.h mode.h \
 frame.h strfunc.h sp_dec.h dec_amr.h dtx_dec.h dtx_enc.h q_plsf.h \
 gc_pred.h d_plsf.h c_g_aver.h ec_gains.h ph_disp.h bgnscd.h lsp_avg.h \
 pstfilt.h preemph.h agc.h post_pro.h d_homing.h
basicop2.o: basicop2.c typedef.h typedefs.h basic_op.h
count.o: count.c typedef.h typedefs.h count.h
oper_32b.o: oper_32b.c typedef.h typedefs.h basic_op.h oper_32b.h \
 count.h
copy.o: copy.c copy.h typedef.h typedefs.h basic_op.h count.h
log2.o: log2.c log2.h typedef.h typedefs.h basic_op.h count.h log2.tab
set_zero.o: set_zero.c set_zero.h typedef.h typedefs.h basic_op.h \
 count.h
strfunc.o: strfunc.c strfunc.h mode.h frame.h
n_proc.o: n_proc.c
sid_sync.o: sid_sync.c sid_sync.h typedef.h typedefs.h mode.h frame.h \
 basic_op.h count.h
agc.o: agc.c agc.h typedef.h typedefs.h basic_op.h count.h cnst.h \
 inv_sqrt.h
autocorr.o: autocorr.c autocorr.h typedef.h typedefs.h basic_op.h \
 oper_32b.h count.h cnst.h
az_lsp.o: az_lsp.c az_lsp.h typedef.h typedefs.h basic_op.h oper_32b.h \
 count.h cnst.h grid.tab
bits2prm.o: bits2prm.c bits2prm.h typedef.h typedefs.h mode.h \
 basic_op.h count.h bitno.tab cnst.h
cl_ltp.o: cl_ltp.c cl_ltp.h typedef.h typedefs.h mode.h pitch_fr.h \
 ton_stab.h cnst.h basic_op.h count.h oper_32b.h convolve.h g_pitch.h \
 pred_lt.h enc_lag3.h enc_lag6.h q_gain_p.h
convolve.o: convolve.c convolve.h typedef.h typedefs.h basic_op.h \
 count.h
c1035pf.o: c1035pf.c c1035pf.h typedef.h typedefs.h basic_op.h count.h \
 cnst.h inv_sqrt.h set_sign.h cor_h.h s10_8pf.h gray.tab
d_plsf.o: d_plsf.c d_plsf.h typedef.h typedefs.h cnst.h mode.h \
 basic_op.h count.h copy.h q_plsf_5.tab
d_plsf_5.o: d_plsf_5.c d_plsf.h typedef.h typedefs.h cnst.h mode.h \
 basic_op.h count.h lsp_lsf.h reorder.h copy.h q_plsf_5.tab
d_gain_c.o: d_gain_c.c d_gain_c.h typedef.h typedefs.h mode.h \
 gc_pred.h basic_op.h oper_32b.h count.h cnst.h log2.h pow2.h \
 gains.tab
d_gain_p.o: d_gain_p.c d_gain_p.h typedef.h typedefs.h mode.h \
 basic_op.h oper_32b.h count.h cnst.h gains.tab
dec_lag6.o: dec_lag6.c dec_lag6.h typedef.h typedefs.h basic_op.h \
 count.h
d1035pf.o: d1035pf.c d1035pf.h typedef.h typedefs.h basic_op.h count.h \
 cnst.h gray.tab
cor_h.o: cor_h.c cor_h.h typedef.h typedefs.h cnst.h basic_op.h \
 count.h inv_sqrt.h
enc_lag3.o: enc_lag3.c enc_lag3.h typedef.h typedefs.h basic_op.h \
 count.h cnst.h
enc_lag6.o: enc_lag6.c enc_lag6.h typedef.h typedefs.h basic_op.h \
 count.h
g_code.o: g_code.c g_code.h typedef.h typedefs.h basic_op.h count.h \
 cnst.h
g_pitch.o: g_pitch.c g_pitch.h typedef.h typedefs.h mode.h basic_op.h \
 oper_32b.h count.h cnst.h
int_lpc.o: int_lpc.c int_lpc.h typedef.h typedefs.h basic_op.h count.h \
 cnst.h lsp_az.h
inter_36.o: inter_36.c inter_36.h typedef.h typedefs.h basic_op.h \
 count.h cnst.h inter_36.tab
inv_sqrt.o: inv_sqrt.c inv_sqrt.h typedef.h typedefs.h basic_op.h \
 count.h inv_sqrt.tab
lag_wind.o: lag_wind.c lag_wind.h typedef.h typedefs.h basic_op.h \
 oper_32b.h count.h lag_wind.tab
levinson.o: levinson.c levinson.h typedef.h typedefs.h cnst.h \
 basic_op.h oper_32b.h count.h
lsp_az.o: lsp_az.c lsp_az.h typedef.h typedefs.h basic_op.h oper_32b.h \
 count.h
lsp_lsf.o: lsp_lsf.c lsp_lsf.h typedef.h typedefs.h basic_op.h count.h \
 lsp_lsf.tab
ol_ltp.o: ol_ltp.c ol_ltp.h typedef.h typedefs.h mode.h p_ol_wgh.h \
 vad.h vad1.h cnst_vad.h vad2.h cnst.h pitch_ol.h count.h basic_op.h
pitch_fr.o: pitch_fr.c pitch_fr.h typedef.h typedefs.h mode.h \
 basic_op.h oper_32b.h count.h cnst.h enc_lag3.h enc_lag6.h inter_36.h \
 inv_sqrt.h convolve.h
pitch_ol.o: pitch_ol.c pitch_ol.h typedef.h typedefs.h mode.h vad.h \
 vad1.h cnst_vad.h vad2.h basic_op.h oper_32b.h count.h cnst.h \
 inv_sqrt.h calc_cor.h hp_max.h
pow2.o: pow2.c pow2.h typedef.h typedefs.h basic_op.h count.h pow2.tab
pre_big.o: pre_big.c pre_big.h typedef.h typedefs.h mode.h cnst.h \
 basic_op.h oper_32b.h syn_filt.h weight_a.h residu.h count.h
pre_proc.o: pre_proc.c pre_proc.h typedef.h typedefs.h basic_op.h \
 oper_32b.h count.h
pred_lt.o: pred_lt.c pred_lt.h typedef.h typedefs.h basic_op.h count.h \
 cnst.h
preemph.o: preemph.c preemph.h typedef.h typedefs.h basic_op.h count.h
prm2bits.o: prm2bits.c prm2bits.h typedef.h typedefs.h mode.h \
 basic_op.h count.h bitno.tab cnst.h
pstfilt.o: pstfilt.c pstfilt.h typedef.h typedefs.h mode.h cnst.h \
 preemph.h agc.h basic_op.h set_zero.h weight_a.h residu.h copy.h \
 syn_filt.h count.h
q_gain_c.o: q_gain_c.c q_gain_c.h typedef.h typedefs.h mode.h \
 gc_pred.h basic_op.h oper_32b.h count.h log2.h pow2.h gains.tab
q_gain_p.o: q_gain_p.c q_gain_p.h typedef.h typedefs.h mode.h \
 basic_op.h oper_32b.h count.h cnst.h gains.tab
q_plsf.o: q_plsf.c q_plsf.h typedef.h typedefs.h cnst.h mode.h \
 basic_op.h
q_plsf_5.o: q_plsf_5.c q_plsf.h typedef.h typedefs.h cnst.h mode.h \
 basic_op.h count.h lsp_lsf.h reorder.h lsfwt.h q_plsf_5.tab
lsfwt.o: lsfwt.c lsfwt.h typedef.h typedefs.h cnst.h basic_op.h \
 count.h
reorder.o: reorder.c reorder.h typedef.h typedefs.h basic_op.h count.h
residu.o: residu.c residu.h typedef.h typedefs.h basic_op.h count.h \
 cnst.h
lsp.o: lsp.c lsp.h typedef.h typedefs.h q_plsf.h cnst.h mode.h \
 basic_op.h oper_32b.h copy.h az_lsp.h int_lpc.h count.h lsp.tab
lpc.o: lpc.c lpc.h typedef.h typedefs.h levinson.h cnst.h mode.h \
 basic_op.h oper_32b.h autocorr.h lag_wind.h count.h window.tab
ec_gains.o: ec_gains.c ec_gains.h typedef.h typedefs.h gc_pred.h \
 mode.h basic_op.h oper_32b.h count.h cnst.h gmed_n.h gains.tab
spreproc.o: spreproc.c spreproc.h cnst.h mode.h typedef.h typedefs.h \
 basic_op.h oper_32b.h weight_a.h syn_filt.h residu.h copy.h count.h
syn_filt.o: syn_filt.c syn_filt.h typedef.h typedefs.h basic_op.h \
 count.h cnst.h
weight_a.o: weight_a.c weight_a.h typedef.h typedefs.h basic_op.h \
 count.h cnst.h
qua_gain.o: qua_gain.c qua_gain.h typedef.h typedefs.h gc_pred.h \
 mode.h basic_op.h oper_32b.h count.h cnst.h pow2.h qua_gain.tab
gc_pred.o: gc_pred.c gc_pred.h typedef.h typedefs.h mode.h basic_op.h \
 oper_32b.h cnst.h count.h log2.h copy.h
q_plsf_3.o: q_plsf_3.c q_plsf.h typedef.h typedefs.h cnst.h mode.h \
 basic_op.h count.h lsp_lsf.h reorder.h lsfwt.h copy.h q_plsf_3.tab
post_pro.o: post_pro.c post_pro.h typedef.h typedefs.h basic_op.h \
 oper_32b.h count.h
dec_lag3.o: dec_lag3.c dec_lag3.h typedef.h typedefs.h basic_op.h \
 count.h
dec_gain.o: dec_gain.c dec_gain.h typedef.h typedefs.h gc_pred.h \
 mode.h basic_op.h oper_32b.h count.h cnst.h pow2.h log2.h \
 qua_gain.tab qgain475.tab
d_plsf_3.o: d_plsf_3.c d_plsf.h typedef.h typedefs.h cnst.h mode.h \
 basic_op.h count.h lsp_lsf.h reorder.h copy.h q_plsf_3.tab
d4_17pf.o: d4_17pf.c d4_17pf.h typedef.h typedefs.h basic_op.h count.h \
 cnst.h gray.tab
c4_17pf.o: c4_17pf.c c4_17pf.h typedef.h typedefs.h basic_op.h count.h \
 inv_sqrt.h cnst.h cor_h.h set_sign.h gray.tab
d3_14pf.o: d3_14pf.c d3_14pf.h typedef.h typedefs.h basic_op.h count.h \
 cnst.h
c3_14pf.o: c3_14pf.c c3_14pf.h typedef.h typedefs.h basic_op.h count.h \
 inv_sqrt.h cnst.h cor_h.h set_sign.h
d2_11pf.o: d2_11pf.c d2_11pf.h typedef.h typedefs.h basic_op.h count.h \
 cnst.h
c2_11pf.o: c2_11pf.c c2_11pf.h typedef.h typedefs.h basic_op.h count.h \
 inv_sqrt.h cnst.h cor_h.h set_sign.h c2_11pf.tab
d2_9pf.o: d2_9pf.c d2_9pf.h typedef.h typedefs.h basic_op.h count.h \
 cnst.h c2_9pf.tab
c2_9pf.o: c2_9pf.c c2_9pf.h typedef.h typedefs.h basic_op.h count.h \
 inv_sqrt.h cnst.h cor_h.h set_sign.h c2_9pf.tab
cbsearch.o: cbsearch.c cbsearch.h typedef.h typedefs.h mode.h c2_9pf.h \
 c2_11pf.h c3_14pf.h c4_17pf.h c8_31pf.h c1035pf.h basic_op.h count.h \
 cnst.h
spstproc.o: spstproc.c spstproc.h typedef.h typedefs.h mode.h \
 basic_op.h oper_32b.h count.h syn_filt.h cnst.h
gain_q.o: gain_q.c gain_q.h typedef.h typedefs.h mode.h gc_pred.h \
 g_adapt.h basic_op.h count.h qua_gain.h cnst.h g_code.h q_gain_c.h \
 calc_en.h qgain795.h qgain475.h set_zero.h
cod_amr.o: cod_amr.c cod_amr.h typedef.h typedefs.h cnst.h mode.h \
 lpc.h levinson.h lsp.h q_plsf.h cl_ltp.h pitch_fr.h ton_stab.h \
 gain_q.h gc_pred.h g_adapt.h p_ol_wgh.h vad.h vad1.h cnst_vad.h \
 vad2.h dtx_enc.h basic_op.h count.h copy.h set_zero.h qua_gain.h \
 pre_big.h ol_ltp.h spreproc.h pred_lt.h spstproc.h cbsearch.h \
 convolve.h
dec_amr.o: dec_amr.c dec_amr.h typedef.h typedefs.h cnst.h mode.h \
 dtx_dec.h dtx_enc.h q_plsf.h gc_pred.h d_plsf.h c_g_aver.h frame.h \
 ec_gains.h ph_disp.h bgnscd.h lsp_avg.h basic_op.h count.h copy.h \
 set_zero.h syn_filt.h agc.h int_lpc.h dec_gain.h dec_lag3.h \
 dec_lag6.h d2_9pf.h d2_11pf.h d3_14pf.h d4_17pf.h d8_31pf.h d1035pf.h \
 pred_lt.h d_gain_p.h d_gain_c.h int_lsf.h lsp_lsf.h ex_ctrl.h \
 sqrt_l.h lsp.tab bitno.tab b_cn_cod.h
sp_enc.o: sp_enc.c sp_enc.h typedef.h typedefs.h cnst.h pre_proc.h \
 mode.h cod_amr.h lpc.h levinson.h lsp.h q_plsf.h cl_ltp.h pitch_fr.h \
 ton_stab.h gain_q.h gc_pred.h g_adapt.h p_ol_wgh.h vad.h vad1.h \
 cnst_vad.h vad2.h dtx_enc.h basic_op.h count.h set_zero.h prm2bits.h
sp_dec.o: sp_dec.c sp_dec.h typedef.h typedefs.h cnst.h dec_amr.h \
 mode.h dtx_dec.h dtx_enc.h q_plsf.h gc_pred.h d_plsf.h c_g_aver.h \
 frame.h ec_gains.h ph_disp.h bgnscd.h lsp_avg.h pstfilt.h preemph.h \
 agc.h post_pro.h basic_op.h count.h set_zero.h bits2prm.h
ph_disp.o: ph_disp.c ph_disp.h typedef.h typedefs.h mode.h basic_op.h \
 count.h cnst.h copy.h ph_disp.tab
g_adapt.o: g_adapt.c g_adapt.h typedef.h typedefs.h basic_op.h \
 oper_32b.h count.h cnst.h gmed_n.h
calc_en.o: calc_en.c calc_en.h typedef.h typedefs.h mode.h basic_op.h \
 oper_32b.h count.h cnst.h log2.h
qgain795.o: qgain795.c qgain795.h typedef.h typedefs.h g_adapt.h \
 basic_op.h oper_32b.h count.h cnst.h log2.h pow2.h sqrt_l.h calc_en.h \
 mode.h q_gain_p.h mac_32.h gains.tab
qgain475.o: qgain475.c qgain475.h typedef.h typedefs.h gc_pred.h \
 mode.h basic_op.h mac_32.h oper_32b.h count.h cnst.h pow2.h log2.h \
 qgain475.tab
sqrt_l.o: sqrt_l.c sqrt_l.h typedef.h typedefs.h basic_op.h count.h \
 sqrt_l.tab
set_sign.o: set_sign.c set_sign.h typedef.h typedefs.h basic_op.h \
 count.h inv_sqrt.h cnst.h
s10_8pf.o: s10_8pf.c s10_8pf.h typedef.h typedefs.h cnst.h basic_op.h \
 count.h
bgnscd.o: bgnscd.c bgnscd.h typedef.h typedefs.h cnst.h basic_op.h \
 count.h copy.h set_zero.h gmed_n.h sqrt_l.h
gmed_n.o: gmed_n.c gmed_n.h typedef.h typedefs.h basic_op.h count.h
mac_32.o: mac_32.c mac_32.h typedef.h typedefs.h basic_op.h oper_32b.h \
 count.h
ex_ctrl.o: ex_ctrl.c ex_ctrl.h typedef.h typedefs.h cnst.h basic_op.h \
 count.h copy.h set_zero.h gmed_n.h sqrt_l.h
c_g_aver.o: c_g_aver.c c_g_aver.h typedef.h typedefs.h mode.h cnst.h \
 basic_op.h count.h set_zero.h
lsp_avg.o: lsp_avg.c lsp_avg.h typedef.h typedefs.h cnst.h basic_op.h \
 oper_32b.h count.h q_plsf_5.tab copy.h
int_lsf.o: int_lsf.c int_lsf.h typedef.h typedefs.h basic_op.h count.h \
 cnst.h
c8_31pf.o: c8_31pf.c c8_31pf.h typedef.h typedefs.h basic_op.h count.h \
 cnst.h inv_sqrt.h cor_h.h set_sign.h s10_8pf.h
d8_31pf.o: d8_31pf.c d8_31pf.h typedef.h typedefs.h basic_op.h count.h \
 cnst.h
p_ol_wgh.o: p_ol_wgh.c p_ol_wgh.h typedef.h typedefs.h mode.h vad.h \
 vad1.h cnst_vad.h vad2.h basic_op.h oper_32b.h count.h cnst.h \
 corrwght.tab gmed_n.h inv_sqrt.h calc_cor.h hp_max.h
ton_stab.o: ton_stab.c ton_stab.h typedef.h typedefs.h mode.h cnst.h \
 basic_op.h count.h oper_32b.h set_zero.h copy.h
vad1.o: vad1.c vad.h vad1.h typedef.h typedefs.h cnst_vad.h vad2.h \
 basic_op.h count.h oper_32b.h
dtx_enc.o: dtx_enc.c dtx_enc.h typedef.h typedefs.h cnst.h q_plsf.h \
 mode.h gc_pred.h basic_op.h oper_32b.h copy.h set_zero.h log2.h \
 lsp_lsf.h reorder.h count.h lsp.tab
dtx_dec.o: dtx_dec.c dtx_dec.h typedef.h typedefs.h dtx_enc.h cnst.h \
 q_plsf.h mode.h gc_pred.h d_plsf.h c_g_aver.h frame.h basic_op.h \
 oper_32b.h copy.h set_zero.h log2.h lsp_az.h pow2.h a_refl.h \
 b_cn_cod.h syn_filt.h lsp_lsf.h reorder.h count.h q_plsf_5.tab \
 lsp.tab
a_refl.o: a_refl.c a_refl.h typedef.h typedefs.h basic_op.h oper_32b.h \
 count.h cnst.h
b_cn_cod.o: b_cn_cod.c b_cn_cod.h typedef.h typedefs.h basic_op.h \
 oper_32b.h count.h cnst.h window.tab
calc_cor.o: calc_cor.c calc_cor.h typedef.h typedefs.h basic_op.h \
 oper_32b.h count.h cnst.h
hp_max.o: hp_max.c hp_max.h typedef.h typedefs.h basic_op.h oper_32b.h \
 count.h cnst.h
vadname.o: vadname.c vadname.h
vad2.o: vad2.c typedef.h typedefs.h cnst.h basic_op.h oper_32b.h \
 count.h log2.h pow2.h vad2.h
r_fft.o: r_fft.c typedef.h typedefs.h cnst.h basic_op.h oper_32b.h \
 count.h vad2.h
lflg_upd.o: lflg_upd.c typedef.h typedefs.h cnst.h basic_op.h \
 oper_32b.h count.h vad2.h mode.h
e_homing.o: e_homing.c e_homing.h typedef.h typedefs.h cnst.h
d_homing.o: d_homing.c d_homing.h typedef.h typedefs.h mode.h \
 bits2prm.h d_homing.tab bitno.tab cnst.h
