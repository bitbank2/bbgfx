/****************************************************************/
/* BBGFX Demo                                                   */
/* Sample code to show the capabilities of the BBGFX library.   */
/* Copyright (c) 2009-2017 BitBank Software, Inc.               */
/*                                                              */
/* Change History:                                              */
/* 8/20/09 Larry B - started project                            */
/* 5/30/17 Larry B - rewrote for Linux Framebuffer & SPI LCD    */
/****************************************************************/
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <linux/fb.h>
#include <time.h>
#include <sys/mman.h>
#include <sys/ioctl.h>

#include <zlib.h>
#include "bbgfx.h"
#include "bbgfx_demo.h"
#include "mini_io.h"
#include "mini_pil.h"

#ifndef FALSE
#define FALSE 0
#define TRUE 1
#endif

// defaults to sending output to /dev/fb0
// comment out this #define to use the SPI-LCD instead
#define USE_FB

#ifndef USE_FB
#include <spi_lcd.h>
#endif

int LoadAssets(void);
void InitFB(void);
void ShutdownFB(void);
void BBGFXDemo(void);
static int iFBPitch;
static unsigned char *pFB;
static int fbfd; // framebuffer handle
static int iScreenSize;
static struct fb_var_screeninfo vinfo;
static struct fb_fix_screeninfo finfo;

#define BALL_COUNT 32
BBGFX_DC bbdc;
BBBitmap *pBBMList;
signed int iDotX[BALL_COUNT], iDotY[BALL_COUNT], iDotDX[BALL_COUNT], iDotDY[BALL_COUNT];
unsigned char ucDotColor[BALL_COUNT];

int iOrientation;
int bSkipFrame = FALSE; // when things are running too slowly
int bAutoStart; /* simulate key presses to start the game */
int bUserAbort = TRUE;
int bFocus = FALSE; // LIB0009
int bPerformanceCounter = FALSE; /* Flag indication a high resolution timer is available */
unsigned char *pSound; // pointer to replacement tune
char szAppName[] = "BBGFX Demo";
int iDisplayWidth, iVideoSize;
int iDisplayHeight;
int iDisplayBpp;
int iMenuOffset;
uint64_t iCurrTime, iTargTime;
uint64_t iFrameTime; /* Clock ticks per frame */
unsigned long ulKeys, ulJoy; /* Current key presses, joystick movements */
int iFPS, iTimerFreq; /* Frequency in ticks per second of the high performance timer */
long lOldTime;
unsigned long lStartTime, lEndTime; /* Used for computing average FPS */
int cx, cy; /* Current viewable display size */
int iPitch, iScreenPitch; /* Current distance to next video line */
int iFrame, iOldFrame; /* Frame count/speed variables */
int iScreenBpp; /* How many colors does the display have? */
unsigned char *pBitmap, *pScreen; /* Pointer to bitmap memory */
char pszHome[256]; /* Home directory */

void BBGStartTimer(int iFramesPerSecond);
void BBGTimerDelay(void);
void BBGCreateVideoBuffer(int iScreenX, int iScreenY, int iScreenBPP, unsigned char **pBuffer);

//
// Return the current time in nanoseconds
//
uint64_t BB_Clock()
{
        uint64_t ns;
        struct timespec time;

        clock_gettime(CLOCK_MONOTONIC, &time);
        ns = time.tv_nsec + (time.tv_sec * 1000000000LL);
        return ns;
} /* BB_Clock() */

int main(int argc, char *argv[])
{

    iDisplayWidth = 240; // arbitrary values for now
    iDisplayHeight = 320;

#ifdef USE_FB
    InitFB(); // figure out the display specs
#else
    if (spilcdInit(LCD_ILI9341, 0, 32000000, 16, 18, 11)) 
    {
    }
#endif

    pBBMList = MiniIOAlloc(BITMAP_COUNT*sizeof(BBBitmap));
    BBGCreateVideoBuffer(iDisplayWidth, iDisplayHeight, 16, &pBitmap);

    if(LoadAssets())
    {
        printf("Error loading assets\n");
        return -1;
    }

    // Load up BBGFX_DC structure with our bitmap info
    pBBMList[BITMAP_SCREEN].iBitsPerPixel = 16;
    pBBMList[BITMAP_SCREEN].iWidth = iDisplayWidth;
    pBBMList[BITMAP_SCREEN].iHeight = iDisplayHeight;
    pBBMList[BITMAP_SCREEN].iPitch = iPitch;
    pBBMList[BITMAP_SCREEN].pBits = pBitmap;
    // set the virtual display as the destination bitmap
    memcpy(&bbdc.bmDest, &pBBMList[BITMAP_SCREEN], sizeof(BBBitmap));

    BBGFXDemo();
   
#ifdef USE_FB
   ShutdownFB();
#else
   spilcdShutdown();
#endif

   return 0;

} /* main() */

void ShutdownFB(void)
{
// Release the framebuffer
   munmap(pFB, iScreenSize);
   close(fbfd);

} /* ShutdownFB() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : InitFB(void)                                               *
 *                                                                          *
 *  PURPOSE    : Initialize a pointer to the display.                       *
 *                                                                          *
 ****************************************************************************/
void InitFB(void)
{

	fbfd = open("/dev/fb0", O_RDWR);
	if (fbfd)
	{
                // get the fixed screen info
                   ioctl(fbfd, FBIOGET_FSCREENINFO, &finfo);
                // get the variable screen info
                   ioctl(fbfd, FBIOGET_VSCREENINFO, &vinfo);
                   iScreenPitch = iFBPitch = (vinfo.xres * vinfo.bits_per_pixel) / 8;
                   iScreenSize = finfo.smem_len;
		printf("Screen = %dx%d, %d Bpp\n", vinfo.xres, vinfo.yres, vinfo.bits_per_pixel);
                   pScreen = pFB = (unsigned char *)mmap(0, iScreenSize, PROT_READ | PROT_WRITE, MAP_SHARED, fbfd, 0);
	}

} /* InitFB() */

void BBG16to32(void *source, void *dest, int iLen)
{
int x;
unsigned short us, *s;
uint32_t u32, *d;

    s = (unsigned short *)source;
    d = (uint32_t *)dest;
    for (x=0; x<iLen; x++)
    {
        us = *s++;
        u32 = ((us & 0x1f) << 3);
        u32 |= ((us & 0x1c) >> 2); // blue
        u32 |= ((us & 0x7e0) << 5);
        u32 |= ((us & 0x600) >> 1); // green
        u32 |= ((us & 0xf800) << 8);
        u32 |= ((us & 0xe000) << 3);
        u32 |= 0xff000000; // alpha
        *d++ = u32;
    } // for x

} /* BBG16to32() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : BBGScreenUpdate(char, HDC, int, int)                       *
 *                                                                          *
 *  PURPOSE    : Paint the screen where it needs it.                        *
 *                                                                          *
 ****************************************************************************/
void BBGScreenUpdate(int iWidth, int iHeight)
{
#ifdef USE_FB 
int y;
unsigned short *s, *d;

// Note - this assumes the framebuffer is set to 16 or 32-bits per pixel
      for (y=0; y<iHeight; y++)
         {
         s = (unsigned short *)&pBitmap[y*iPitch];
         d = (unsigned short *)&pScreen[(y * iScreenPitch)];
         if (vinfo.bits_per_pixel == 16)
             memcpy(d, s, iWidth*sizeof(short));
         else if (vinfo.bits_per_pixel == 32)
             BBG16to32(s, d, iWidth);
         }
#else
	spilcdBitBlt(0,0,240,320,pBitmap,480);
#endif
}/* BBGScreenUpdate() */

void BBGDrawBG(void)
{
static int iLineOffset = 0;
static int iColorOffset = 0;
static int iColorDelta = 1;
int i, r, g, b;

   // Background 0 = flowing colors and lines
   if (iMenuOffset == 0)
      {
      // draw changing background color gradient
      bbdc.rDest.left = 0;
      bbdc.rDest.top = 0;
      bbdc.rDest.right = iDisplayWidth-1;
      bbdc.rDest.bottom = iDisplayHeight-1;
      // color offset goes between 0 and 0x20
      // use to transition between 2 starting and 2 ending colors for the gradient
      r = ((0xe)*iColorOffset)>>5;
      g = 1-((1*iColorOffset)>>5);
      b = 0x11 - (((0x02)*iColorOffset)>>5);
      bbdc.ulColor = (r<<11) | (g<<5) | b; // start
      r = ((0xe)*iColorOffset)>>5;
      g = 0x32 - (((0x17)*iColorOffset)>>5);
      b = 0x17 + (((0x8)*iColorOffset)>>5);
      bbdc.ulColor2 = (r<<11) | (g<<5) | b; // end
      BBGGradientFill(&bbdc);

      iColorOffset += iColorDelta; // increase green
      if (iColorOffset < 0)
         {
         iColorOffset = 0;
         iColorDelta = -iColorDelta;
         }
      if (iColorOffset >= 0x20)
         {
         iColorOffset = 0x20;
         iColorDelta = -iColorDelta;
         }
      // draw slowly moving crosshatched lines
      bbdc.ucPattern = 0x2d;
      bbdc.ulColor = 0x4d5; // cyan
      bbdc.ucTranslucency = 254; // 255
      for (i=iLineOffset; i<iDisplayHeight; i+= 40)
         {
         bbdc.rDest.top = i;
         bbdc.rDest.left = 0;
         bbdc.rDest.right = iDisplayWidth-1;
         BBGHLine(&bbdc);
         }
      for (i=iLineOffset; i<iDisplayWidth; i+= 40)
         {
         bbdc.rDest.top = 0;
         bbdc.rDest.bottom = iDisplayHeight-1;
         bbdc.rDest.left = i;
         BBGVLine(&bbdc);
         }
      iLineOffset++;
      if (iLineOffset >= 40)
         iLineOffset -= 40;
      BBG_BitBlit(&bbdc, pBBMList, BITMAP_1BPP, 0, 0, 0xf800, iLineOffset*6); // Set translucency to 0 for "fast blit"
      }
   else if (iMenuOffset == 1)
      { // Background 1 = Windows BMP (RLE compressed) copied over background
      for (i=0; i<iDisplayWidth; i+= 16)
         {
         BBG_BitBlit(&bbdc, pBBMList, BITMAP_PATTERN, i, 0, 0, 0); // Set translucency to 0 for "fast blit"
         }
      }
   else  // jpeg background
      {
      BBG_BitBlit(&bbdc, pBBMList, BITMAP_BACKGROUND, 0, 0, 0, 0); // Set translucency to 0 for "fast blit"
      }
   // Draw an elipse
   bbdc.rDest.left = 20;
   bbdc.rDest.top = 260;
   bbdc.rDest.right = 24 + (iFrame & 0x3f);
   bbdc.rDest.bottom = 264 + ((iFrame & 0x3f)>>1);
   bbdc.ulColor = 0;
   BBGCircle(&bbdc);

} /* BBGDrawBG() */

void BBGDrawFG(void)
{
int i, x, y, cx, cy;
int iBitmap;
unsigned char ucTrans;
//int iTime;

   // Draw a rotating clock on the left side
   memcpy(&bbdc.bmSrc, &pBBMList[BITMAP_CLOCK], sizeof(BBBitmap));
   bbdc.rSrc.top = 50; // center of source rotation
   bbdc.rSrc.left = 50;
   memcpy(&bbdc.bmDest, &pBBMList[BITMAP_TEMP], sizeof(BBBitmap));
   bbdc.rDest.top = 50; // center in destination
   bbdc.rDest.left = 50;
   i = iFrame % 360;
   BBGRotate(&bbdc, i);
   memcpy(&bbdc.bmDest, &pBBMList[BITMAP_SCREEN], sizeof(BBBitmap)); // restore screen as destination
   BBG_BitBlit(&bbdc, pBBMList, BITMAP_TEMP, 0, (iDisplayHeight-100)>>1, 0xf81f, 255);
   // Show stretching on an alpha bitmap (with alpha adjust)
   i = (iFrame & 0xff)>>2;
   BBG_StretchBlit(&bbdc, pBBMList, BITMAP_BLUE_DOT, iDisplayWidth-32-(i>>1), (iDisplayHeight>>1)-(i>>1), i+1, i+1, 0xf81f, 255-(i<<2));
   
   // Draw the 240x127 US flag scaled up and down
   i = (iFrame >> 1) & 0x7f; // get 64 sizes * 2 (and translucency levels)   
   if (i >= 0x40)
      i = 0x7f-i;
   ucTrans = i << 2;
   cx = (240*i)>>6;
   cy = (127*i)>>6;
   x = (iDisplayWidth - cx)>>1;
//   y = (iDisplayHeight - cy)>>1;
   BBG_StretchBlit(&bbdc, pBBMList, BITMAP_FLAG, x, 0/*y*/, cx, cy, 0xf81f, ucTrans);
   
//   iTime = BB_Clock();
//   for (x = 0; x < 32; x++)
//   {
   // draw the "dots" on top of the menu and update their positions (bounce them off of the "walls")
   for (i=0; i<BALL_COUNT; i++)
      {
      BBG_BitBlit(&bbdc, pBBMList, BITMAP_RED_DOT+ucDotColor[i], iDotX[i], iDotY[i], 0xf81f, 255);
      iDotX[i] += iDotDX[i];
      iDotY[i] += iDotDY[i];
      if (iDotX[i] >= (iDisplayWidth-40) || iDotX[i] <= 0) // bounce it
         iDotDX[i] = -iDotDX[i];
      if (iDotY[i] >= (iDisplayHeight-40) || iDotY[i] <= 0) // bounce it
         iDotDY[i] = -iDotDY[i];
    } // for i
//   } // for x
//   iTime = BB_Clock() - iTime;

   // draw the 4 menu buttons (centered)
   x = (iDisplayWidth - 90)>>1;
   y = (iDisplayHeight - (32*MENU_COUNT))>>1;
   for (i=0; i<MENU_COUNT; i++)
      {
      iBitmap = (iMenuOffset == i) ? BITMAP_BUTTONS_ON : BITMAP_BUTTONS_OFF;
      BBG_DrawFilmStrip(&bbdc, pBBMList, iBitmap, x, y+i*32, i, 90, 0xf81f, 0); //255);
      }
   
} /* BBGDrawFG() */

void BBGFXDemo(void)
{
int i;

   bUserAbort = FALSE;
   iFrame = 0;
   iMenuOffset = 0;
   BBGStartTimer(60);
// initialize the positions and directions for the 4 "dots"
   for (i=0; i<BALL_COUNT; i++)
      {
      iDotX[i] = rand() % (iDisplayWidth -40);
      iDotY[i] = rand() % (iDisplayHeight -40);
      iDotDX[i] = (rand() & 15)-8;
      ucDotColor[i] = rand() & 3;
      if (iDotDX[i] == 0) // don't let it be stagnant
         iDotDX[i]++;
      iDotDY[i] = (rand() & 15)-8;
      if (iDotDY[i] == 0) // don't let it be stagnant
         iDotDY[i]++;
      }
// Create a temporary BB bitmap for the rotated image
   pBBMList[BITMAP_TEMP].iBitsPerPixel = 16;
   pBBMList[BITMAP_TEMP].iHeight = 100;
   pBBMList[BITMAP_TEMP].iWidth = 100;
   pBBMList[BITMAP_TEMP].iPitch = 100*2;
   pBBMList[BITMAP_TEMP].pBits = MiniIOAlloc(100*100*2);
   
   while (!bUserAbort && iFrame < 1200)
      {
         BBGDrawBG(); // draw the background colors/images;
         BBGDrawFG(); // draw the foreground colors/images
	 BBGScreenUpdate(iDisplayWidth, iDisplayHeight);
         iFrame++;
         BBGTimerDelay();
      }
      
} /* BBGFXDemo() */

//
// Load the bitmaps used in the demo
//
int LoadAssets(void)
{
int i;

           i = BBG_LoadBitmap(&pBBMList[BITMAP_BUTTONS_ON], "assets/buttons1.gif", NULL, FALSE);
           i |= BBG_LoadBitmap(&pBBMList[BITMAP_BUTTONS_OFF], "assets/buttons2.gif", NULL, FALSE);
           i |= BBG_LoadBitmap(&pBBMList[BITMAP_FLAG], "assets/flag.gif",NULL, FALSE);
           i |= BBG_LoadBitmap(&pBBMList[BITMAP_CLOCK], "assets/clock.gif", NULL, FALSE);
           i |= BBG_LoadBitmap(&pBBMList[BITMAP_RED_DOT], "assets/redDot.png", NULL, FALSE);
           i |= BBG_LoadBitmap(&pBBMList[BITMAP_BLUE_DOT], "assets/blueDot.png", NULL, FALSE);
           i |= BBG_LoadBitmap(&pBBMList[BITMAP_GREEN_DOT], "assets/greenDot.png", NULL, FALSE);
           i |= BBG_LoadBitmap(&pBBMList[BITMAP_PURPLE_DOT], "assets/purpleDot.png", NULL, FALSE);
           i |= BBG_LoadBitmap(&pBBMList[BITMAP_PATTERN], "assets/pattern.bmp", NULL, FALSE);
           i |= BBG_LoadBitmap(&pBBMList[BITMAP_BACKGROUND], "assets/background.jpg", NULL, TRUE);
           i |= BBG_LoadBitmap(&pBBMList[BITMAP_1BPP], "assets/1bpp.bmp", NULL, FALSE);
           i |= BBG_LoadBitmap(&pBBMList[BITMAP_CRASH], "assets/poi.png", NULL, FALSE);

	i |= BBG_LoadBitmap(&pBBMList[BITMAP_SQUARE], "assets/square.png", NULL, FALSE);
	return i;
}/* LoadAssets() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : BBGCreateVideoBuffer(int,int,int,char **)                  *
 *                                                                          *
 *  PURPOSE    : Create the buffer used to hold video memory.               *
 *                                                                          *
 ****************************************************************************/
void BBGCreateVideoBuffer(int iScreenX, int iScreenY, int iScreenBPP, unsigned char **pBuffer)
{
	*pBuffer = (unsigned char *)MiniIOAlloc((iScreenX * iScreenBPP * iScreenY)/8);
	iPitch = iScreenX*2;

} /* BBGCreateVideoBuffer() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : BBGStartTimer(int)                                         *
 *                                                                          *
 *  PURPOSE    : Initialize the timer feature for frame to frame delays.    *
 *                                                                          *
 ****************************************************************************/
void BBGStartTimer(int iFramesPerSecond)
{

   iFPS = iFramesPerSecond; /* Keep a copy of current game frame rate */
   iCurrTime = BB_Clock();
   iFrameTime = (1000000000L / iFramesPerSecond);
   iTargTime = iCurrTime + iFrameTime;

} /* BBGStartTimer() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : BBGTimerDelay(void)                                        *
 *                                                                          *
 *  PURPOSE    : Delay the right amount of time to keep the game in synch.  *
 *                                                                          *
 ****************************************************************************/
void BBGTimerDelay(void)
{
signed int j;
static int iSkipped = 0;

      iCurrTime = BB_Clock();
      j = (signed int)(iTargTime - iCurrTime);
      if (j > 10000) // we can trust usleep to sleep this much
         usleep(j/1000);
      while (iCurrTime < iTargTime && iTargTime - iCurrTime < (unsigned)iFrameTime)
         {
            iCurrTime = BB_Clock();
         }
   if (j > iFrameTime*2) /* We need to catch up */
      iTargTime = iCurrTime;
   iTargTime += iFrameTime; /* Update next target tick value */
   if (j < 0) // && bAutoSkip) // we're running too slow
      {
      iSkipped++;
      if (iSkipped < 10)
         bSkipFrame = TRUE; // don't skip more than 4 frames in a row
      else
         {
         iSkipped = 0; // allow 1 to be drawn
         bSkipFrame = FALSE;
         }
      }
   else
      bSkipFrame = FALSE;
} /* BBGTimerDelay() */

