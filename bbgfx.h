#ifndef BBGFX_DEFINED
#define BBGFX_DEFINED

// BitBank Graphics Library Definitions
// Copyright (c) 2009-2017 BitBank Software, Inc.
// Written by Larry Bank
// bitbank@pobox.com
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

typedef struct tag_bbbitmap
{
unsigned char *pBits; // pointer to raw pixels
int iPitch; // pitch in bytes
int iWidth, iHeight;
int iBitsPerPixel;
} BBBitmap;

typedef struct tag_bbrect
{
int left, top, right, bottom;
} BBRect;

typedef struct tag_bbgfx_dc
{
BBBitmap bmSrc, bmDest; // source and destination bitmaps
BBRect rSrc, rDest; // src/dest rectangles (coordinates)
unsigned long ulColor; // RGB color for drawing lines or transparent color
unsigned long ulColor2; // for gradient fills and source color for swap
unsigned long ulNewColor; // for blit with color substitution
unsigned char ucPattern; // line bit pattern
unsigned char ucTranslucency; // 0=no contribution, 255=100%
unsigned short usAngle; // drawing angle (0,90,180,270)
unsigned short usTemp[2048]; // 4K useful for ARM temp space
} BBGFX_DC;

// For BBG565Gray function
enum
{
BBG_BIAS_NONE=0,  // no bias to the gray level
BBG_BIAS_WHITE,	// add 50% white
BBG_BIAS_BLACK,	// add 50% black
};

void BBGHLine(BBGFX_DC *pDC);
void BBGVLine(BBGFX_DC *pDC);
void BBGHFlip(BBGFX_DC *pDC);
void BBGVFlip(BBGFX_DC *pDC);
void BBGLine(BBGFX_DC *pDC);
void BBGFill(BBGFX_DC *pDC);
void BBGGradientFill(BBGFX_DC *pDC);
void BBGBlitAlpha(BBGFX_DC *pDC);
void BBGStretchAlpha(BBGFX_DC *pDC);
void BBGBlitTransparent(BBGFX_DC *pDC);
void BBGBlitFast(BBGFX_DC *pDC);
void BBGStretchBlitAA(BBGFX_DC *bbdc);
void BBGBlitColorSwap(BBGFX_DC *pDC);
void BBGBlitPattern(BBGFX_DC *pDC);
void BBGStretchBlit(BBGFX_DC *pDC);
void BBGStretchPattern(BBGFX_DC *pDC);
void BBGCircle(BBGFX_DC *pDC);
void BBGRotate(BBGFX_DC *pDC, int iAngle);
void BBGRotate90(BBGFX_DC *pDC);
void BBG565Gray(BBGFX_DC *pDC, unsigned char ucBias);
void ARMGFXCopy16(unsigned short *pSrc, unsigned short *pDest, int iWidth);
void DrawScan90(unsigned short *s, unsigned short *d, unsigned short *pEnd, int iScreenPitch);
void DrawScan270(unsigned short *s, unsigned short *d, unsigned short *pEnd, int iScreenPitch);

// Helper functions
int BBG_LoadBitmap(BBBitmap *pBBM, char *szName, unsigned char *pData, int bFilter);
void BBG_BitBlit(BBGFX_DC *bbdc, BBBitmap *pBBMList, int iBitmap, int x, int y, unsigned short usColor, unsigned char ucTrans);
void BBG_DrawFilmStrip(BBGFX_DC *bbdc, BBBitmap *pBBMList, int iBitmap, int x, int y, int iIndex, int iWidth, unsigned short usColor, unsigned char ucTrans);
void BBG_StretchBlit(BBGFX_DC *bbdc, BBBitmap *pBBMList, int iBitmap, int x, int y, int cx, int cy, unsigned short usColor, unsigned char ucTrans);
#endif // BBGFX_DEFINED
