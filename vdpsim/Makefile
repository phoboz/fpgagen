#
# Makefile
#

SRC=../src
SRCS = $(SRC)/vdp.vhd $(SRC)/DualPortRAM.vhd $(SRC)/vdp_common.vhd vdp_tb.vhd vram.vhd cpu.vhd
OBJS = $(notdir $(SRCS:.vhd=.o)) vram_c.o video_c.o cpu_c.o

FLAGS=--ieee=synopsys -fexplicit 

all: vdp_tb

vdp_tb.o: vdp.o vram.o video.o cpu.o
vdp.o: DualPortRAM.o vdp_common.o
vram.o: vram_c.o
video.o: video_c.o
cpu.o: cpu_c.o

%.o: %.vhd
	ghdl -a $(FLAGS) $<

%.o: $(SRC)/%.vhd
	ghdl -a $(FLAGS) $<

work-obj93.cf: $(SRCS)
	ghdl -i $(SRCS)

run: vdp_tb Makefile
	ghdl -r $< --ieee-asserts=disable --stop-time=100ms 

view: video.png
	display video.png

video.png: vdp_tb Makefile
	ghdl -r $< --ieee-asserts=disable
	rm -f video.png
	avconv -vcodec rawvideo -f rawvideo -pix_fmt rgb565 -s 855x262 -i video.rgb -f image2 -vcodec png video.png
	convert video.png -sample 855x524! video.png

vdp_tb: $(OBJS)
	ghdl -e $(FLAGS) -Wl,vram_c.o -Wl,video_c.o -Wl,cpu_c.o $@

vdp.ghw:  vdp_tb Makefile
	ghdl -r $< --ieee-asserts=disable --stop-time=200us --wave=$@

wave: vdp.ghw
	gtkwave vdp.sav

clean::
	rm -f work-obj93.cf *.o *~ *.lst *.ghw $(ROOT) *.out
