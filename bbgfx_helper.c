//
// BitBank GFX Library - Helper routines
// 
// Project started 9/23/09
// Copyright (c) 2009-2017 BitBank Software, Inc.
// Written by Larry Bank
// Change History
// 9/23/09 - first version
// 5/28/17 - converted to Linux
//
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
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "bbgfx.h"
#include <mini_io.h>
#include <mini_pil.h>

void BBG_StretchBlit(BBGFX_DC *bbdc, BBBitmap *pBBMList, int iBitmap, int x, int y, int cx, int cy, unsigned short usColor, unsigned char ucTrans)
{
int dx, dy;

   if (x >= bbdc->bmDest.iWidth || y >= bbdc->bmDest.iHeight || cx == 0 || cy == 0)
      return; // nothing to do
      
   memcpy(&bbdc->bmSrc, &pBBMList[iBitmap], sizeof(BBBitmap));
   dx = (cx * 256)/bbdc->bmSrc.iWidth;
   dy = (cy * 256)/bbdc->bmSrc.iHeight;
   
   bbdc->ulColor = usColor;
   if (x < 0)
      {
      bbdc->rSrc.left = - (x*256)/dx;
      cx += x; 
      if (bbdc->rSrc.left >= pBBMList[iBitmap].iWidth)
         return; // not visible
      bbdc->rDest.left = 0;
      }
   else
      {
      bbdc->rSrc.left = 0;
      bbdc->rDest.left = x;
      }
   if (y < 0)
      {
      bbdc->rSrc.top = - (y*256)/dy;
      cy += y;
      if (bbdc->rSrc.top >= pBBMList[iBitmap].iHeight)
         return; // not visible
      bbdc->rDest.top = 0;
      }
   else
      {
      bbdc->rSrc.top = 0;
      bbdc->rDest.top = y;
      }
   // assume full source bitmap will be used
   bbdc->rSrc.bottom = pBBMList[iBitmap].iHeight-1;
   bbdc->rSrc.right = pBBMList[iBitmap].iWidth-1;
   if (cx + bbdc->rDest.left >= bbdc->bmDest.iWidth)
      { // too wide; trim to destination bitmap size
      int i, j;
      i = bbdc->bmDest.iWidth - bbdc->rDest.left - 1;
      i = cx - i; // amount of reduction
      j = (i*256)/cx;
      cx -= i;
      // reduce source rectangle by same amount
      bbdc->rSrc.right -= ((bbdc->rSrc.right*j)>>8);
      }
   if (cy + bbdc->rDest.top >= bbdc->bmDest.iHeight)
      { // too tall; trim to destination bitmap size
      int i, j;
      i = bbdc->bmDest.iHeight - bbdc->rDest.top - 1;
      i = cy - i; // amount of reduction
      j = (i*256)/cy;
      cy -= i;
      // reduce source rectangle by same amount
      bbdc->rSrc.bottom -= ((bbdc->rSrc.bottom*j)>>8);
      }
   bbdc->ucTranslucency = ucTrans;
   bbdc->rDest.right = bbdc->rDest.left + cx;
   bbdc->rDest.bottom = bbdc->rDest.top + cy;
   if (bbdc->bmSrc.iBitsPerPixel == 1)
      BBGStretchPattern(bbdc);
   else if (bbdc->bmSrc.iBitsPerPixel == 16)
      BBGStretchBlit(bbdc);
   else
      BBGStretchAlpha(bbdc);
} /* BBG_StretchBlit() */

// draw horizontal filmstrip images
void BBG_DrawFilmStrip(BBGFX_DC *bbdc, BBBitmap *pBBMList, int iBitmap, int x, int y, int iIndex, int iWidth, unsigned short usColor, unsigned char ucTrans)
{
BBBitmap *bb;

   if (x >= bbdc->bmDest.iWidth || y >= bbdc->bmDest.iHeight)
      return; // nothing to do
   // faster to copy the structure elements than to call memcpy()
   bb = &pBBMList[iBitmap];
   bbdc->bmSrc.pBits = bb->pBits;
   bbdc->bmSrc.iPitch = bb->iPitch;
   bbdc->bmSrc.iWidth = bb->iWidth;
   bbdc->bmSrc.iHeight = bb->iHeight;
   bbdc->bmSrc.iBitsPerPixel = bb->iBitsPerPixel;

   bbdc->rSrc.top = 0;
   bbdc->rSrc.bottom = pBBMList[iBitmap].iHeight-1;
   bbdc->rSrc.left = iIndex * iWidth;
   bbdc->rSrc.right = bbdc->rSrc.left + iWidth - 1;
   bbdc->ulColor = usColor;
   bbdc->ucTranslucency = ucTrans;
   bbdc->rDest.left = x;
   bbdc->rDest.top = y;
   if (bb->iBitsPerPixel == 1) // only 1 routine can handle 1bpp images
      BBGBlitPattern(bbdc);
   else if (ucTrans == 0)
      BBGBlitFast(bbdc); // totally translucent means the bitmap has no transparent pixels
   else
      BBGBlitTransparent(bbdc);

} /* BBG_DrawFilmStrip() */

void BBG_BitBlit(BBGFX_DC *bbdc, BBBitmap *pBBMList, int iBitmap, int x, int y, unsigned short usColor, unsigned char ucTrans)
{
BBBitmap *bb;

   if (x >= bbdc->bmDest.iWidth || y >= bbdc->bmDest.iHeight)
      return; // nothing to do
//   memcpy(&bbdc.bmSrc, &pBBMList[iBitmap], sizeof(BBBitmap));
   // faster to copy the structure elements than to call memcpy()
   bb = &pBBMList[iBitmap];
   bbdc->bmSrc.pBits = bb->pBits;
   bbdc->bmSrc.iPitch = bb->iPitch;
   bbdc->bmSrc.iWidth = bb->iWidth;
   bbdc->bmSrc.iHeight = bb->iHeight;
   bbdc->bmSrc.iBitsPerPixel = bb->iBitsPerPixel;
   
   if (x < 0)
      {
      bbdc->rSrc.left = -x;
      if (bbdc->rSrc.left >= pBBMList[iBitmap].iWidth)
         return; // not visible
      bbdc->rDest.left = 0;
      }
   else
      {
      bbdc->rSrc.left = 0;
      bbdc->rDest.left = x;
      }
   if (y < 0)
      {
      bbdc->rSrc.top = -y;
      if (bbdc->rSrc.top >= pBBMList[iBitmap].iHeight)
         return; // not visible
      bbdc->rDest.top = 0;
      }
   else
      {
      bbdc->rSrc.top = 0;
      bbdc->rDest.top = y;
      }
   bbdc->rSrc.bottom = pBBMList[iBitmap].iHeight-1;
   bbdc->rSrc.right = pBBMList[iBitmap].iWidth-1;
   bbdc->ulColor = usColor;
   bbdc->ucTranslucency = ucTrans;
   if (bb->iBitsPerPixel == 1) // only one routine can handle 1bpp
      BBGBlitPattern(bbdc);
   else if (ucTrans == 0)
      BBGBlitFast(bbdc);
   else if (pBBMList[iBitmap].iBitsPerPixel == 16)
      BBGBlitTransparent(bbdc);
   else
      BBGBlitAlpha(bbdc);
      
} /* BBG_BitBlit() */

/****************************************************************************

    FUNCTION: BBG_LoadBitmap(BBBitmap, char)

    PURPOSE:  Load an image file into a BBBitmap

****************************************************************************/
int BBG_LoadBitmap(BBBitmap *pBBM, char *szName, unsigned char *pData, int bFilter)
{
PIL_PAGE pp, ppTemp;
PIL_FILE pf;
int i;

    if (pBBM == NULL)
	return PIL_ERROR_INVPARAM;

    memset(&pf,0,sizeof(pf));
    if (pData == NULL) // file
       {
          if (szName == NULL)
             return PIL_ERROR_INVPARAM;

          i = mPILOpen(szName, &pf, 0); // read from a file
          if (i)
             return i; // error
       }
    else // data coming from a pointer
       {    
       pf.pData = pData;
       pf.cState = PIL_FILE_STATE_LOADED;
       pf.iFileSize = 0x1000000; // DEBUG - this size needs to be set properly
       i = mPILOpen(NULL, &pf, 0);
       if (i)
          return i; // error
       }
    i = mPILRead(&pf, &ppTemp, 0, 0); // get image info
    if (i)
       return i; // error
    pp.cCompression = PIL_COMP_NONE; // decompress it
    if (bFilter) // don't let the jpeg loader reduce to RGB565 yet
       i = mPILConvert(&ppTemp, &pp, 0);
    else
       i = mPILConvert(&ppTemp, &pp, PIL_CONVERT_16BPP); // ok to let the JPEG code reduce to RGB565
    if (i) // error
       {
       if (pData == NULL) // came from a file; need to close it
          mPILClose(&pf);
       return i;
       }
    if (bFilter && pp.cBitsperpixel == 24) // use dither to convert to RGB565
       mPILModify(&pp, PIL_MODIFY_COLORS, 16, PIL_COLORS_ERRORDIFF);
    if (pp.cFlags & PIL_PAGEFLAGS_BOTTOMUP) // need to flip it
       mPILModify(&pp, PIL_MODIFY_FLIPV, 0, 0);
    if (pp.cBitsperpixel == 32) // 24bpp with alpha channel
       { // convert to 16bpp and preserve the alpha channel
       int x, y, lsize;
       unsigned char *puc, *pDest;
       unsigned long ul, *pul;
       unsigned short us, *pus;
       // split up the alpha channel into another "plane"
       // and convert the rgb888 pixels into rgb565
       lsize = mPILCalcSize(pp.iWidth, 16);
       pDest = MiniIOAlloc((lsize*pp.iHeight) + (lsize>>1)*pp.iHeight);
       pus = (unsigned short *)pDest;
       pul = (unsigned long *)pp.pData;
       puc = &pDest[lsize*pp.iHeight]; // alpha plane starts after rgb565 data
       for (y=0; y<pp.iHeight; y++)
          {
          for (x=0; x<pp.iWidth; x++)
             {
             ul = pul[x]; // grab a 32-bit pixel
             us = (unsigned short)((ul & 0xf8)>>3); // blue
             us |= (unsigned short)((ul & 0xfc00)>>5); // green
             us |= (unsigned short)((ul & 0xf80000)>>8); // red
             pus[x] = us;
             puc[x] = (unsigned char)(ul >> 24); // alpha
             } // for x
          pus += (lsize>>1);
          pul += (pp.iPitch>>2);
          puc += (lsize>>1);
          } /// for y
       MiniIOFree(pp.pData);
       pp.pData = pDest;
       pp.iPitch = lsize;
       pBBM->iBitsPerPixel = 24; // in this case, 24bpp = rgb565 + 8-bit alpha
       }
    else if (pp.cBitsperpixel > 1 && pp.cBitsperpixel != 16)
       {
       i = mPILModify(&pp, PIL_MODIFY_COLORS, 16, 0); // convert to 16bpp
       pBBM->iBitsPerPixel = 16;
       }
    else
       pBBM->iBitsPerPixel = pp.cBitsperpixel;
    pBBM->iHeight = pp.iHeight;
    pBBM->iWidth = pp.iWidth;
    pBBM->pBits = pp.pData;
    pBBM->iPitch = pp.iPitch;
    if (pData == NULL) // if the image data didn't come from a pointer
       mPILFree(&ppTemp);
    
    if (pData == NULL) // came from a file; need to close it
       mPILClose(&pf);
    return PIL_ERROR_SUCCESS;
    
} /* BBG_LoadBitmap() */

