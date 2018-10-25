#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <arpa/inet.h>

#define SULV_U  (0) /* Uninitialized   */
#define SULV_X  (1) /* Forcing Unknown */
#define SULV_0  (2) /* Forcing 0       */
#define SULV_1  (3) /* Forcing 1       */
#define SULV_Z  (4) /* High Impedance  */
#define SULV_W  (5) /* Weak Unknown    */
#define SULV_L  (6) /* Weak 0          */
#define SULV_H  (7) /* Weak 1          */
#define SULV__  (8) /* Don't care      */

unsigned short regs[][2] = {
  { 4, 0x8004}, // Enable the palette
  { 4, 0x8144}, // Enable the display/mode 5
  { 4, 0x8230}, // Set the scroll A name table base to 0xC000
  { 4, 0x8405}, // Set the scroll B name table base to 0xA000
  { 4, 0x8570}, // Set the sprite table base to 0xE000
  { 4, 0x8D3F}, // Set the hscroll base to 0xFC00
  { 4, 0x9001}, // Set the scroll size to V32 H64
  { 4, 0x8F02}, // Set the auto-increment data to 2

  { 4, 0xc000}, // prepare cram access
};

unsigned short *cram = NULL;
int cram_entries = 0;

void cpu_c(char clk, char rst_n, char sel[1], char dtack_n, char rnw[1], char ds[2], char a[5], char d[16]) {
  static char last_clk = -1;
  static int state = -1;
  static int wait = 0;
  static int cnt = 0;

  // internal signal states
  static int sel_i = 0;
  static int rnw_i = 1;
  static int ds_i  = 3;
  static int a_i   = 0;
  static int d_i   = 0;

  if(!cram) {
    FILE *f=fopen("SpriteMaskingTestRomSrc/cram.bin", "rb");
    if(!f) { perror("cram"); exit(-1); }
    fseek(f, 0, SEEK_END);
    int len = ftell(f);
    fseek(f, 0, SEEK_SET);

    cram = malloc(len);
    fread(cram, 1, len, f);

    // adjust endianess
    for(int i=0;i<len/2;i++) cram[i] = htons(cram[i]);

    // ignore trailing 0-entries
    cram_entries = len/2;
    while(!cram[cram_entries-1]) cram_entries--;
      
    fclose(f);
  }
  
  if((rst_n != SULV_1)&&(rst_n != SULV_H)) {
    state = 0;
    wait = 2;
    cnt = 0;
    sel_i = 0;
  } else {
  
    // only work on rising clock edge
    if((clk != last_clk) && (clk == SULV_0) || (clk == SULV_L)) {
      // just waiting
      if(wait || sel_i) {
	int ack = (dtack_n == SULV_0)||(dtack_n == SULV_L);

	if(sel_i) {
	  if(ack) sel_i = 0;
	} else     wait--;
      } else {
	
	// state 0: writing regs
	if(state == 0) {
	  sel_i = 1;
	  ds_i = 0;
	  rnw_i = 0;
	  a_i = regs[cnt][0];
	  d_i = regs[cnt++][1];
	  wait = 0;

	  // all regsisters written?
	  if(cnt == sizeof(regs)/(2*sizeof(unsigned short))) {
	    state = 1;  // next-> write cram
	    wait = 2;
	    cnt = 0;
	  }
	}

	// state 1: load cram
	else if(state == 1) {
	  sel_i = 1;
	  rnw_i = 0;
	  ds_i = 0;
	  a_i = 0;
	  d_i = cram[cnt++];
	  wait = 0;

	  // all cram entries written?
	  if(cnt == cram_entries) {
	    state = 3;
	    cnt = 0;
	  }
	}
      }
    }
  }

  last_clk = clk;
    
  // drive all signals
  for(int i=0;i<16;i++)
    d[i] = (d_i&(0x8000>>i))?SULV_1:SULV_0;
  for(int i=0;i<5;i++)
    a[i] = (a_i&(0x10>>i))?SULV_1:SULV_0;
  for(int i=0;i<2;i++)
    ds[i] = (ds_i&(2>>i))?SULV_1:SULV_0;

  sel[0] = sel_i?SULV_1:SULV_0;
  rnw[0] = rnw_i?SULV_1:SULV_0;
}
