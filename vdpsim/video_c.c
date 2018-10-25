// avconv -vcodec rawvideo -f rawvideo -pix_fmt rgb565 -s 855x262 -i video.rgb -f image2 -vcodec png video.png
// convert video.png -resize 427x262! video.png

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SULV_U  (0) /* Uninitialized   */
#define SULV_X  (1) /* Forcing Unknown */
#define SULV_0  (2) /* Forcing 0       */
#define SULV_1  (3) /* Forcing 1       */
#define SULV_Z  (4) /* High Impedance  */
#define SULV_W  (5) /* Weak Unknown    */
#define SULV_L  (6) /* Weak 0          */
#define SULV_H  (7) /* Weak 1          */
#define SULV__  (8) /* Don't care      */

FILE *raw = NULL;

void video_c(char clk, char r[4], char g[4], char b[4], char hs, char vs) {
  static char last_clk = 0;
  static int ignore = 100;
  static int vskip = 1;
  static int vdetect = 0;

  // only work on rising clock edge
  if(clk == last_clk) return;
  last_clk = clk;

  if((clk == SULV_0) || (clk == SULV_L)) return;
  // subcnt
  static int subcnt = 1;
  if(--subcnt) return;

  subcnt = 4;
  
  // ignore first few events
  if(ignore) {
    ignore--;
    return;
  }


  unsigned int i, cr = 0, cg = 0, cb = 0;
  for(i=0;i<4;i++) {
    cr <<= 1; cg <<= 1; cb <<= 1;
    if((r[i] == SULV_1)||(r[i] == SULV_H)) cr |= 1;
    if((g[i] == SULV_1)||(g[i] == SULV_H)) cg |= 1;
    if((b[i] == SULV_1)||(b[i] == SULV_H)) cb |= 1;
  }

  static int last_hs = 0, last_vs = 0;
  int chs = ((hs == SULV_1)||(hs == SULV_H));
  int cvs = ((vs == SULV_1)||(vs == SULV_H));

  static int hcnt = 0;
  static int vcnt = 0;

  int pix = ((cr << 12)&0xf000) | ((cg << 7)&0x0780) | ((cb << 1)&0x3e);

  if(raw) {
    // colorize hs and vs
    if(!chs) pix = 0xf000;
    if(!cvs) pix = 0x0780;

    
    // write pixel
    fputc(pix & 0xff, raw);
    fputc((pix >> 8)&0xff, raw);
  }
  
  hcnt++;
  
  if((chs != last_hs) && chs) {
    printf("HS@%d %d\n", vcnt, hcnt);
    hcnt = 0;
    vcnt++;

    if(vdetect) {   
      // begin of a new image: open file
      if(!raw) raw = fopen("video.rgb", "wb");
      else { fclose(raw); printf("DONE\n"); exit(0); }
      vdetect = 0;
    }
  }
  last_hs = chs;
  
  if((cvs != last_vs) && cvs) {
    printf("VS@%d/%d\n", vcnt, hcnt);
    vcnt = 0;

    if(vskip) {
      vskip--;
    } else {
      vdetect = 1;
    }
  }
  last_vs = cvs;
}
