// BitBank portable graphics library
// project started 3/15/09
// 10/10/10 - Added BBGFastBlit, BBGStretchBlit
// 10/14/10 - Added BBGVLine & BBGHLine
// 5/28/17 - converted to Linux
// todo: bbgstretchalpha, bbgblitalpha, bbgline
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

void BBGStretchAlpha(BBGFX_DC *pDC)
{
} /* BBGStretchAlpha() */

void BBGBlitTransparent(BBGFX_DC *pDC)
{
unsigned short usTransColor, *pSrc, *pDest;
unsigned char ucTrans, ucITrans;
int x, y, cx, cy;
unsigned long ulMask, ulS, ulD;

   pSrc = (unsigned short *)&pDC->bmSrc.pBits[(pDC->rSrc.top * pDC->bmSrc.iPitch) + (pDC->rSrc.left<<1)];
   pDest = (unsigned short *)&pDC->bmDest.pBits[(pDC->rDest.top * pDC->bmDest.iPitch) + (pDC->rDest.left<<1)];
   ucTrans = pDC->ucTranslucency;   
   usTransColor = (unsigned short)pDC->ulColor; // transparent color
   cy = pDC->rSrc.bottom - pDC->rSrc.top + 1; // height to draw
   cx = pDC->rSrc.right - pDC->rSrc.left + 1; // width to draw
// check if we're completely out of bounds
   if (pDC->rDest.top >= pDC->bmDest.iHeight)
      return; // past bottom
   if (pDC->rDest.left >= pDC->bmDest.iWidth)
      return; // past right side
   if (ucTrans < 5) // nothing to draw
      return;
   // see if we're out of bounds, if so, trim to fit
   if (cy > pDC->bmDest.iHeight - pDC->rDest.top) // will go past bottom
      cy = pDC->bmDest.iHeight - pDC->rDest.top; // new height
   if (cx > pDC->bmDest.iWidth - pDC->rDest.left) // will go past right edge
      cx = pDC->bmDest.iWidth - pDC->rDest.left; // new width
   if (cx <= 0 || cy <= 0) // nothing to draw
      return;
   if (ucTrans == 255) // opaque?
      {
      for (y=0; y<cy; y++)
         {
         for (x=0; x<cx; x++)
            {
            if (pSrc[x] != usTransColor)
               pDest[x] = pSrc[x];
            }
         pSrc += (pDC->bmSrc.iPitch>>1);
         pDest += (pDC->bmDest.iPitch>>1);
         }
      }
   else // need to calculate translucent pixels
      {
      ulMask = 0x07E0F81F;
      ucTrans >>= 3; // we only use 32-levels
      ucITrans = 32 - ucTrans; // inverted
      for (y=0; y<cy; y++)
         {
         for (x=0; x<cx; x++)
            {
            if (pSrc[x] != usTransColor)
               { // apply translucency
               ulS = pSrc[x]; // get src pixel
               ulS = ulS | (ulS << 16); // double it
               ulS &= ulMask; // ready
               ulD = pDest[x]; // get dest pixel
               ulD = ulD | (ulD<<16); // double it
               ulD &= ulMask; // ready
               ulD = (ulS * ucTrans) + (ulD * ucITrans);
               ulD = (ulD >> 5) & ulMask;
               ulD = ulD | (ulD >> 16); // combine g+r+b
               pDest[x] = (unsigned short)ulD;
               }
            }
         pSrc += (pDC->bmSrc.iPitch>>1);
         pDest += (pDC->bmDest.iPitch>>1);
         }
      }
} /* BBGBlitTransparent() */

void BBGBlitAlpha(BBGFX_DC *pDC)
{
} /* BBGBlitAlpha() */

void BBGHFlip(BBGFX_DC *pDC)
{
unsigned short us, *pSrc;
int x, y;
int cx;

   pSrc = (unsigned short *)&pDC->bmSrc.pBits[(pDC->rSrc.top * pDC->bmSrc.iPitch) + (pDC->rSrc.left<<1)];
   for (y=0; y < pDC->bmSrc.iHeight; y++)
      {
      cx = pDC->bmSrc.iWidth-1;
      for (x=0; x<(pDC->bmSrc.iWidth>>1); x++)
         {
         us = pSrc[x];
         pSrc[x] = pSrc[cx];
         pSrc[cx] = us;
         }
      pSrc += (pDC->bmSrc.iPitch >> 1);
      }

} /* BBGHFlip() */

void BBGVFlip(BBGFX_DC *pDC)
{
unsigned short us, *pSrc, *pDest;
int x, y;

   pSrc = (unsigned short *)&pDC->bmSrc.pBits[(pDC->rSrc.top * pDC->bmSrc.iPitch) + (pDC->rSrc.left<<1)];
   pDest = &pSrc[(pDC->bmSrc.iHeight-1)*(pDC->bmSrc.iPitch>>1)];
   for (y=0; y < (pDC->bmSrc.iHeight>>1); y++)
      {
      for (x=0; x<pDC->bmSrc.iWidth; x++)
         {
         us = pSrc[x];
         pSrc[x] = pDest[x];
         pDest[x] = us;
         }
      pSrc += (pDC->bmSrc.iPitch >> 1);
      pDest -= (pDC->bmSrc.iPitch >> 1);
      }

} /* BBGVFlip() */

void BBGLine(BBGFX_DC *pDC)
{
} /* BBGLine() */

void BBGHLine(BBGFX_DC *pDC)
{
int i, cx, iCount;
unsigned short *d, *pDest;
unsigned long *pulDest;
unsigned char ucPattern;
unsigned long ulS, ulD, ulMask;
unsigned char ucTrans, ucITrans;

    if (pDC->rDest.left >= pDC->bmDest.iWidth ||
        pDC->rDest.right >= pDC->bmDest.iWidth ||
        pDC->rDest.top >= pDC->bmDest.iHeight)
        return; // invalid coordinates
    if (pDC->rDest.right > pDC->rDest.left)
    {
       cx = pDC->rDest.right - pDC->rDest.left + 1;
       pDest = (unsigned short *)&pDC->bmDest.pBits[(pDC->rDest.top * pDC->bmDest.iPitch)+ (pDC->rDest.left<<1)];
    }
    else
    {
       cx = pDC->rDest.left - pDC->rDest.right + 1;
       pDest = (unsigned short *)&pDC->bmDest.pBits[(pDC->rDest.top * pDC->bmDest.iPitch)+ (pDC->rDest.right<<1)];
    }
    ulMask = 0x07e0f81f;
    ucPattern = pDC->ucPattern;
    if (pDC->ucTranslucency == 255) // opaque, just store
    {
        i = 8;
        while (i && ucPattern)
        {
            d = pDest;
            iCount = cx;
            if (ucPattern & 0x80) // draw this line
            {
                if ((int)d & 2) // not dword aligned
                {
                    *d++ = (unsigned short)pDC->ulColor;
                    iCount--;
                }
                pulDest = (unsigned long *)d;
                ulD = pDC->ulColor;
                ulD |= (ulD << 16);
                while (iCount > 1)
                {
                    *pulDest++ = ulD;
                    iCount -= 2;
                }
                if (iCount) // straggler pixel
                {
                    *(unsigned short *)pulDest = (unsigned short)ulD;
                }
            }
            ucPattern <<= 1;
            i--;
            pDest += (pDC->bmDest.iPitch>>1);
        }
    }
    else // translucent
    {
        ucTrans = pDC->ucTranslucency >> 3;
        ucITrans = 32 - ucTrans;
        ulS = pDC->ulColor;
        ulS |= (ulS << 16);
        ulS &= ulMask;
        ulS *= ucTrans; // source pixel ready
        i = 8;
        while (i && ucPattern)
        {
            d = pDest;
            iCount = cx;
            if (ucPattern & 0x80) // draw this line
            {
                while (iCount)
                {
                    ulD = *d;
                    ulD |= (ulD << 16);
                    ulD &= ulMask;
                    ulD = ulS + (ulD * ucITrans);
                    ulD = (ulD >> 5) & ulMask;
                    ulD |= (ulD >> 16);
                    *d++ = (unsigned short)ulD;
                    iCount--;
                }
            }
            ucPattern <<= 1;
            i--;
            pDest += (pDC->bmDest.iPitch>>1);
        }
    }
} /* BBGHLine() */

void BBGVLine(BBGFX_DC *pDC)
{
int i, cy;
unsigned short *d, *pDest, usColor;
unsigned char ucPattern;
unsigned long ulS, ulD, ulMask;
unsigned char ucTrans, ucITrans;

    if (pDC->rDest.left >= pDC->bmDest.iWidth ||
        pDC->rDest.bottom >= pDC->bmDest.iHeight ||
        pDC->rDest.top >= pDC->bmDest.iHeight)
        return; // invalid coordinates
    if (pDC->rDest.bottom > pDC->rDest.top)
    {
       cy = pDC->rDest.bottom - pDC->rDest.top + 1;
       pDest = (unsigned short *)&pDC->bmDest.pBits[(pDC->rDest.top * pDC->bmDest.iPitch)+ (pDC->rDest.left<<1)];
    }
    else
    {
       cy = pDC->rDest.top - pDC->rDest.bottom + 1;
       pDest = (unsigned short *)&pDC->bmDest.pBits[(pDC->rDest.bottom * pDC->bmDest.iPitch)+ (pDC->rDest.left<<1)];
    }
    ulMask = 0x07e0f81f;
    usColor = (unsigned short)pDC->ulColor;
    if (pDC->ucTranslucency == 255) // opaque, just store
    {
        while (cy)
        {
            d = pDest;
            i = 8;
            ucPattern = pDC->ucPattern;
            while (i && ucPattern)
            {
                if (ucPattern & 0x80) // draw this line
                {
                    *d = usColor;
                }
                i--;
                ucPattern <<= 1;
                d++;
            }
            pDest += (pDC->bmDest.iPitch>>1);
            cy--;
        }
    }
    else // translucent
    {
        ucTrans = pDC->ucTranslucency >> 3;
        ucITrans = 32 - ucTrans;
        ulS = pDC->ulColor;
        ulS |= (ulS << 16);
        ulS &= ulMask;
        ulS *= ucTrans; // source pixel ready
        while (cy)
        {
            d = pDest;
            i = 8;
            ucPattern = pDC->ucPattern;
            while (i && ucPattern)
            {
                if (ucPattern & 0x80) // draw this line
                {
                    ulD = *d;
                    ulD |= (ulD << 16);
                    ulD &= ulMask;
                    ulD = ulS + (ulD * ucITrans);
                    ulD = (ulD >> 5) & ulMask;
                    ulD |= (ulD >> 16);
                    *d = (unsigned short)ulD;
                }
                i--;
                ucPattern <<= 1;
                d++;
            }
            pDest += (pDC->bmDest.iPitch>>1);
            cy--;
        }
    }
} /* BBGVLine() */

void BBGStretchBlit(BBGFX_DC *pDC)
{
int iScaleX, iScaleY;
int cx, cy, x, y, i, j;
int iXSum, iYSum;
unsigned short *pSrc, *pDest, usPixel, usTransparent;
unsigned char ucTrans, ucITrans;
int iDestPitch;
unsigned long ulMask, ulS, ulD;

//   if (pDC->bmSrc.iBitsPerPixel != 16 || pDC->bmDest.iBitsPerPixel != 16)
//      return; // only supports 16bpp
   // Calculate the horizontal and vertical scale factors
   i = (pDC->rSrc.bottom - pDC->rSrc.top) + 1;
   j = (pDC->rDest.bottom - pDC->rDest.top) + 1;
   iScaleY = (i * 256)/j;
   i = (pDC->rSrc.right - pDC->rSrc.left) + 1;
   j = (pDC->rDest.right - pDC->rDest.left) + 1;
   iScaleX = (i * 256)/j;
   // Get source pointer
   pSrc = (unsigned short *)&pDC->bmSrc.pBits[(pDC->bmSrc.iPitch * pDC->rSrc.top) + (pDC->rSrc.left << 1)];
   pDest = (unsigned short *)&pDC->bmDest.pBits[(pDC->bmDest.iPitch * pDC->rDest.top) + (pDC->rDest.left << 1)];
   ucTrans = (pDC->ucTranslucency)>>3; // we only use 32-levels
   ucITrans = 32 - ucTrans; // inverted
   usTransparent = (unsigned short)pDC->ulColor; // transparent color
   cx = (pDC->rDest.right - pDC->rDest.left)+1;
   cy = (pDC->rDest.bottom - pDC->rDest.top)+1;
   iDestPitch = (pDC->bmDest.iPitch - cx*2)>>1; // delta src pitch
   iYSum = 0;
   if (pDC->ucTranslucency == 255) // opaque
      {
      for (y=0; y<cy; y++)
         {
         iXSum = 0;
         for (x=0; x<cx; x++)
            {
            usPixel = pSrc[iXSum>>8];
            iXSum += iScaleX;
            if (usPixel != usTransparent) // can draw it
               {
               pDest[0] = usPixel;
               }
            pDest++;
            } // for x
         iYSum += iScaleY;
         pSrc += (iYSum >> 8)*(pDC->bmSrc.iPitch>>1);
         iYSum &= 0xff; // get rid of whole part
         pDest += iDestPitch; // skip to next dest line
         } // for y
      }
   else // translucent
      {
      ulMask = 0x07E0F81F;
      for (y=0; y<cy; y++)
         {
         iXSum = 0;
         for (x=0; x<cx; x++)
            {
            usPixel = pSrc[iXSum>>8];
            iXSum += iScaleX;
            if (usPixel != usTransparent) // can draw it
               {
               ulS = usPixel | (usPixel << 16); // double it
               ulS &= ulMask; // ready
               ulD = pDest[0]; // get dest pixel
               ulD = ulD | (ulD<<16); // double it
               ulD &= ulMask; // ready
               ulD = (ulS * ucTrans) + (ulD * ucITrans);
               ulD = (ulD >> 5) & ulMask;
               ulD = ulD | (ulD >> 16); // combine g+r+b
               pDest[0] = (unsigned short)ulD;
               }
            pDest++;
            } // for x
         iYSum += iScaleY;
         pSrc += (iYSum >> 8)*(pDC->bmSrc.iPitch>>1);
         iYSum &= 0xff; // get rid of whole part
         pDest += iDestPitch; // skip to next dest line
         } // for y
      }
} /* BBGStretchBlit() */

//
// Return an anti-aliased pixel from the 2x2 block around
// around the source pointer
//
unsigned long BBGAAPixel32(BBBitmap *bmSrc, unsigned long *pSrc, int xsum, int ysum)
{
    signed long lfrac;
    unsigned long ulAlpha;
    unsigned long r1, g1, b1;
    unsigned long r2, g2, b2;
    unsigned long r3, g3, b3;

    if (((xsum >> 16) >= bmSrc->iWidth-1) || ((ysum>>16) >= bmSrc->iHeight-1))
    {
        // up against an edge, return the pixel
        return pSrc[0];
    }
    else // do bilinear interpolation calculations
    {
        // calculate the fractional contributions of the 4 pixels
        // use the product of the x/y differences
        ulAlpha = pSrc[0] & 0xff000000; // don't filter the source alpha
        // pixel 0,0 + 1,0
        r1 = (pSrc[0] & 0xff0000) >> 16;
        g1 = (pSrc[0] & 0xff00) >> 8;
        b1 = (pSrc[0] & 0xff);
        lfrac = (xsum >> 8) & 0xff;
        // pixel 1,0
        r2 = (pSrc[1] & 0xff0000) >> 16;
        g2 = (pSrc[1] & 0xff00) >> 8;
        b2 = (pSrc[1] & 0xff);
        r1 = (r1 * (256 - lfrac)) + (r2 * lfrac);
        g1 = (g1 * (256 - lfrac)) + (g2 * lfrac);
        b1 = (b1 * (256 - lfrac)) + (b2 * lfrac);
        // average of pixels 0,0 and 1,0
        r1 >>= 8;
        g1 >>= 8;
        b1 >>= 8;
        // pixel 0,1
        pSrc += (bmSrc->iPitch>>2);
        r2 = (pSrc[0] & 0xff0000) >> 16;
        g2 = (pSrc[0] & 0xff00) >> 8;
        b2 = (pSrc[0] & 0xff);
        // pixel 1,1
        r3 = (pSrc[1] & 0xff0000) >> 16;
        g3 = (pSrc[1] & 0xff00) >> 8;
        b3 = (pSrc[1] & 0xff);
        r2 = (r2 * (256 - lfrac)) + (r3 * lfrac);
        g2 = (g2 * (256 - lfrac)) + (g3 * lfrac);
        b2 = (b2 * (256 - lfrac)) + (b3 * lfrac);
        // average of pixels 0,1 and 1,1
        r2 >>= 8;
        g2 >>= 8;
        b2 >>= 8;
        lfrac = (ysum >> 8) & 0xff;
        // calculate average of top and bottom pair
        r1 = (r1 * (256 - lfrac)) + (r2 * lfrac);
        g1 = (g1 * (256 - lfrac)) + (g2 * lfrac);
        b1 = (b1 * (256 - lfrac)) + (b2 * lfrac);
        // average of all 4 pixels
        r1 >>= 8;
        g1 >>= 8;
        b1 >>= 8;

        return (ulAlpha | (r1 << 16) | (g1 << 8) | b1);
    }

} /* BBGAAPixel32() */

unsigned short BBGAAPixel16(BBBitmap *bmSrc, unsigned short *pSrc, int xsum, int ysum)
{
    signed long lfrac;
    unsigned long r1, g1, b1;
    unsigned long r2, g2, b2;
    unsigned long r3, g3, b3;

    if (((xsum >> 16) >= bmSrc->iWidth-1) || ((ysum>>16) >= bmSrc->iHeight-1))
    {
        // up against an edge, return the pixel
        return pSrc[0];
    }
    else // do bilinear interpolation calculations
    {
        // calculate the fractional contributions of the 4 pixels
        // use the product of the x/y differences
//        ulAlpha = pSrc[0] & 0xff000000; // don't filter the source alpha
        // pixel 0,0 + 1,0
        r1 = (pSrc[0] & 0xf800) >> 8;
        g1 = (pSrc[0] & 0x7e0) >> 3;
        b1 = ((pSrc[0] & 0x1f) << 3);
        lfrac = (xsum >> 8) & 0xff;
        // pixel 1,0
        r2 = (pSrc[1] & 0xf800) >> 8;
        g2 = (pSrc[1] & 0x7e0) >> 3;
        b2 = ((pSrc[1] & 0x1f) << 3);
        r1 = (r1 * (256 - lfrac)) + (r2 * lfrac);
        g1 = (g1 * (256 - lfrac)) + (g2 * lfrac);
        b1 = (b1 * (256 - lfrac)) + (b2 * lfrac);
        // average of pixels 0,0 and 1,0
        r1 >>= 8;
        g1 >>= 8;
        b1 >>= 8;
        // pixel 0,1
        pSrc += (bmSrc->iPitch>>1);
        r2 = (pSrc[0] & 0xf800) >> 8;
        g2 = (pSrc[0] & 0x7e0) >> 3;
        b2 = ((pSrc[0] & 0x1f) << 3);
        // pixel 1,1
        r3 = (pSrc[1] & 0xf800) >> 8;
        g3 = (pSrc[1] & 0x7e0) >> 3;
        b3 = ((pSrc[1] & 0x1f) << 3);
        r2 = (r2 * (256 - lfrac)) + (r3 * lfrac);
        g2 = (g2 * (256 - lfrac)) + (g3 * lfrac);
        b2 = (b2 * (256 - lfrac)) + (b3 * lfrac);
        // average of pixels 0,1 and 1,1
        r2 >>= 8;
        g2 >>= 8;
        b2 >>= 8;
        lfrac = (ysum >> 8) & 0xff;
        // calculate average of top and bottom pair
        r1 = (r1 * (256 - lfrac)) + (r2 * lfrac);
        g1 = (g1 * (256 - lfrac)) + (g2 * lfrac);
        b1 = (b1 * (256 - lfrac)) + (b2 * lfrac);
        // average of all 4 pixels
        r1 >>= 8;
        g1 >>= 8;
        b1 >>= 8;

        return (unsigned short)(((r1 & 0xf8) << 8) | ((g1 & 0xfc) << 3) | (b1 >> 3));
    }

} /* BBGAAPixel16() */
//
// Stretch a bitmap using bilinear interpolation
//
void BBGStretchBlitAA(BBGFX_DC *bbdc)
{
    int curx, cury;
    int dx, dy;
    int x;
    int xsum, ysum;
    unsigned long *pulDest, *pulSrc;
    unsigned short *pusDest, *pusSrc;
    unsigned char *pucDest, *pucSrc;

        ysum = 0;
        cury = bbdc->rDest.bottom -  bbdc->rDest.top + 1;
        curx = bbdc->rDest.right -  bbdc->rDest.left + 1;

        dx = ((bbdc->rSrc.right - bbdc->rSrc.left + 1) * 65536)/ curx;
        dy = ((bbdc->rSrc.bottom - bbdc->rSrc.top + 1) * 65536)/ cury;

        pulDest = (unsigned long *)&bbdc->bmDest.pBits[bbdc->rDest.top * bbdc->bmDest.iPitch + bbdc->rDest.left * sizeof(long)];
        pusDest = (unsigned short *)&bbdc->bmDest.pBits[bbdc->rDest.top * bbdc->bmDest.iPitch + bbdc->rDest.left * sizeof(short)];
        pucDest = &bbdc->bmDest.pBits[(bbdc->bmDest.iPitch * bbdc->bmDest.iHeight)+ (bbdc->rDest.top * (bbdc->bmDest.iPitch>>1)) + bbdc->rDest.left];
        while (cury)  /* loop through the whole image */
           {
               /* Calculate source pointer */
               pulSrc = (unsigned long *)&bbdc->bmSrc.pBits[(bbdc->rSrc.top + (ysum >> 16)) * bbdc->bmSrc.iPitch + bbdc->rSrc.left * sizeof(long)];
               pusSrc = (unsigned short *)&bbdc->bmSrc.pBits[(bbdc->rSrc.top + (ysum >> 16)) * bbdc->bmSrc.iPitch + bbdc->rSrc.left * sizeof(short)];
               pucSrc = &bbdc->bmSrc.pBits[(bbdc->bmSrc.iPitch*bbdc->bmSrc.iHeight) + (bbdc->rSrc.top + (ysum >> 16)) * (bbdc->bmSrc.iPitch>>1) + bbdc->rSrc.left];
               xsum = 0;
               if (bbdc->bmSrc.iBitsPerPixel == 32)
               {
                   for (x = 0; x < curx; x++)
                   {
                       *pulDest++ = BBGAAPixel32(&bbdc->bmSrc, &pulSrc[xsum>>16], xsum, ysum);
                       xsum += dx;
                   }
               }
               else
               {
                   for (x = 0; x < curx; x++)
                   {
                       *pusDest++ = BBGAAPixel16(&bbdc->bmSrc, &pusSrc[xsum>>16], xsum, ysum);
                       if (bbdc->bmSrc.iBitsPerPixel == 24)
                       {
                           *pucDest++ = pucSrc[xsum>>16];
                       }
                       xsum += dx;
                   }
               }
               pulDest -= curx;  /* Move back over stuff drawn */
               pulDest += bbdc->bmDest.iWidth; /* Move down to next line */
               pusDest -= curx;
               pusDest += (bbdc->bmDest.iPitch>>1);
               ysum += dy;
               cury--;
          } // while cury

} /* BBGStretchBlitAA() */

void BBGStretchFast(BBGFX_DC *pDC)
{
int iScaleX, iScaleY;
int cx, cy, x, y, i, j;
int iXSum, iYSum;
unsigned short *pSrc, *pDest;
unsigned long *pulDest, ulPixel;

   if (pDC->bmSrc.iBitsPerPixel != 16 || pDC->bmDest.iBitsPerPixel != 16)
      return; // only supports 16bpp
   // Calculate the horizontal and vertical scale factors
   i = (pDC->rSrc.bottom - pDC->rSrc.top) + 1;
   j = (pDC->rDest.bottom - pDC->rDest.top) + 1;
   iScaleY = (i * 256)/j;
   i = (pDC->rSrc.right - pDC->rSrc.left) + 1;
   j = (pDC->rDest.right - pDC->rDest.left) + 1;
   iScaleX = (i * 256)/j;
   // Get source pointer
   pSrc = (unsigned short *)&pDC->bmSrc.pBits[(pDC->bmSrc.iPitch * pDC->rSrc.top) + (pDC->rSrc.left << 1)];
   pDest = (unsigned short *)&pDC->bmDest.pBits[(pDC->bmDest.iPitch * pDC->rDest.top) + (pDC->rDest.left << 1)];
   cx = (pDC->rDest.right - pDC->rDest.left)+1;
   cy = (pDC->rDest.bottom - pDC->rDest.top)+1;
   iYSum = 0;
   for (y=0; y<cy; y++)
      {
      iXSum = 0;
      x = cx;
      if ((int)pDest & 2) // dword aligned?
         {
         *pDest = *pSrc;
         iXSum += iScaleX;
         x--;
         pulDest = (unsigned long *)&pDest[1];
         }
      else
         {
         pulDest = (unsigned long *)pDest;
         }
      while (x > 1)
         {
         ulPixel = pSrc[iXSum>>8];
         iXSum += iScaleX;
         ulPixel |= (pSrc[iXSum>>8]<<16);
         iXSum += iScaleX;
         *pulDest++ = ulPixel; // store a pixel pair
         x -= 2;
         } // for x
      if (x) // last odd pixel
         {
         *(unsigned short *)pulDest = pSrc[iXSum>>8];
         }
      iYSum += iScaleY;
      pSrc += (iYSum >> 8)*(pDC->bmSrc.iPitch>>1);
      iYSum &= 0xff; // get rid of whole part
      pDest += (pDC->bmDest.iPitch>>1); // skip to next dest line
      } // for y
} /* BBGStretchFast() */

void BBGBlitPattern(BBGFX_DC *pDC)
{
unsigned short *d, *pDest;
unsigned char *s, *pSrc, ucTrans, ucITrans;
unsigned char ucMask, ucByte;
int x, y, cx, cy;
unsigned long ulMask, ulS, ulD;

   if (pDC->bmSrc.iBitsPerPixel != 1 || pDC->bmDest.iBitsPerPixel != 16)
      return; // invalid bit depths
   pSrc = (unsigned char *)&pDC->bmSrc.pBits[(pDC->rSrc.top * pDC->bmSrc.iPitch) + (pDC->rSrc.left<<1)];
   pDest = (unsigned short *)&pDC->bmDest.pBits[(pDC->rDest.top * pDC->bmDest.iPitch) + (pDC->rDest.left<<1)];
   cy = pDC->rSrc.bottom - pDC->rSrc.top + 1; // height to draw
   cx = pDC->rSrc.right - pDC->rSrc.left + 1; // width to draw
// check if we're completely out of bounds
   if (pDC->rDest.top >= pDC->bmDest.iHeight)
      return; // past bottom
   if (pDC->rDest.left >= pDC->bmDest.iWidth)
      return; // past right side
   // see if we're out of bounds, if so, trim to fit
   if (cy > pDC->bmDest.iHeight - pDC->rDest.top) // will go past bottom
      cy = pDC->bmDest.iHeight - pDC->rDest.top; // new height
   if (cx > pDC->bmDest.iWidth - pDC->rDest.left) // will go past right edge
      cx = pDC->bmDest.iWidth - pDC->rDest.left; // new width
   if (cx <= 0 || cy <= 0) // nothing to draw
      return;
   ulMask = 0x07E0F81F;
   ucTrans = pDC->ucTranslucency >> 3;
   ucITrans = 32 - ucTrans;
   ulS = pDC->ulColor;
   ulS |= (ulS << 16);
   ulS &= ulMask;
   ulS *= ucTrans; // ready
   for (y=0; y<cy; y++)
      {
      ucByte = *pSrc++;
      ucMask = 0x80;
      s = pSrc;
      d = pDest;
      for (x=0; x<cx; x++)
         {
         if (ucByte & ucMask) // draw the pixel
            {
            ulD = d[0]; // get dest pixel
            ulD = ulD | (ulD<<16); // double it
            ulD &= ulMask; // ready
            ulD = ulS + (ulD * ucITrans);
            ulD = (ulD >> 5) & ulMask;
            ulD = ulD | (ulD >> 16); // combine g+r+b
            d[0] = (unsigned short)ulD;
            }
         d++;
         ucMask >>= 1;
         if (!ucMask)
            {
            ucByte = *s++;
            ucMask = 0x80;
            }
         }
      pSrc += pDC->bmSrc.iPitch;
      pDest += (pDC->bmDest.iPitch>>1);
      }

} /* BBGBlitPattern() */

void BBGStretchPattern(BBGFX_DC *pDC)
{
int iScaleX, iScaleY;
int cx, cy, x, y, i, j;
int iXSum, iYSum;
unsigned short *pDest;
unsigned char *pSrc;
unsigned char ucTrans, ucITrans;
int iDestPitch;
unsigned long ulMask, ulS, ulD;

   if (pDC->bmSrc.iBitsPerPixel != 1 || pDC->bmDest.iBitsPerPixel != 16)
      return; // only supports 16bpp
   // Calculate the horizontal and vertical scale factors
   i = (pDC->rSrc.bottom - pDC->rSrc.top) + 1;
   j = (pDC->rDest.bottom - pDC->rDest.top) + 1;
   iScaleY = (i * 256)/j;
   i = (pDC->rSrc.right - pDC->rSrc.left) + 1;
   j = (pDC->rDest.right - pDC->rDest.left) + 1;
   iScaleX = (i * 256)/j;
   // Get source pointer
   pSrc = (unsigned char *)&pDC->bmSrc.pBits[(pDC->bmSrc.iPitch * pDC->rSrc.top) + (pDC->rSrc.left << 1)];
   pDest = (unsigned short *)&pDC->bmDest.pBits[(pDC->bmDest.iPitch * pDC->rDest.top) + (pDC->rDest.left << 1)];
   ucTrans = (pDC->ucTranslucency)>>3; // we only use 32-levels
   ucITrans = 32 - ucTrans; // inverted
   cx = (pDC->rDest.right - pDC->rDest.left)+1;
   cy = (pDC->rDest.bottom - pDC->rDest.top)+1;
   iDestPitch = (pDC->bmDest.iPitch - cx*2)>>1; // delta src pitch
   // prepare the source color
   ulMask = 0x07E0F81F;
   ulS = pDC->ulColor;
   ulS |= (ulS << 16);
   ulS &= ulMask;
   ulS *= ucTrans; // ready
   iYSum = 0;
      for (y=0; y<cy; y++)
         {
         iXSum = 0;
         for (x=0; x<cx; x++)
            {
            i = iXSum>>8;
            iXSum += iScaleX;
            if (pSrc[i>>3] & (0x80>>(i&7))) // draw this pixel
               {
               ulD = pDest[0]; // get dest pixel
               ulD = ulD | (ulD<<16); // double it
               ulD &= ulMask; // ready
               ulD = ulS + (ulD * ucITrans);
               ulD = (ulD >> 5) & ulMask;
               ulD = ulD | (ulD >> 16); // combine g+r+b
               pDest[0] = (unsigned short)ulD;
               }
            pDest++;
            } // for x
         iYSum += iScaleY;
         pSrc += (iYSum >> 8)*(pDC->bmSrc.iPitch>>1);
         iYSum &= 0xff; // get rid of whole part
         pDest += iDestPitch; // skip to next dest line
         } // for y
} /* BBGStretchPattern() */

void BBGBlitFast(BBGFX_DC *pDC)
{
unsigned short *pSrc, *pDest;
int y, cx, cy;

   pSrc = (unsigned short *)&pDC->bmSrc.pBits[(pDC->rSrc.top * pDC->bmSrc.iPitch) + (pDC->rSrc.left<<1)];
   pDest = (unsigned short *)&pDC->bmDest.pBits[(pDC->rDest.top * pDC->bmDest.iPitch) + (pDC->rDest.left<<1)];
   cy = pDC->rSrc.bottom - pDC->rSrc.top + 1; // height to draw
   cx = pDC->rSrc.right - pDC->rSrc.left + 1; // width to draw
// check if we're completely out of bounds
   if (pDC->rDest.top >= pDC->bmDest.iHeight)
      return; // past bottom
   if (pDC->rDest.left >= pDC->bmDest.iWidth)
      return; // past right side
   // see if we're out of bounds, if so, trim to fit
   if (cy > pDC->bmDest.iHeight - pDC->rDest.top) // will go past bottom
      cy = pDC->bmDest.iHeight - pDC->rDest.top; // new height
   if (cx > pDC->bmDest.iWidth - pDC->rDest.left) // will go past right edge
      cx = pDC->bmDest.iWidth - pDC->rDest.left; // new width
   if (cx <= 0 || cy <= 0) // nothing to draw
      return;
   for (y=0; y<cy; y++)
      {
      memcpy(pDest, pSrc, cx*2);
      pSrc += (pDC->bmSrc.iPitch>>1);
      pDest += (pDC->bmDest.iPitch>>1);
      }
} /* BBGBlitFast() */

void BBGGradientFill(BBGFX_DC *pDC)
{
} /* BBGGradientFill() */

void BBGFill(BBGFX_DC *pDC)
{
unsigned short usColor, *pDest;
int x, y, cx, cy;

   pDest = (unsigned short *)&pDC->bmDest.pBits[(pDC->rDest.top * pDC->bmDest.iPitch) + (pDC->rDest.left<<1)];
   usColor = (unsigned short)pDC->ulColor;
   
// make sure the defined area is within the bounds of the bitmap
   if (pDC->rDest.bottom >= pDC->bmDest.iHeight)
      return;
   if (pDC->rDest.left >= pDC->bmDest.iWidth)
      return;
   cy = pDC->rDest.bottom - pDC->rDest.top + 1;
   cx = pDC->rDest.right - pDC->rDest.left + 1;
// trim rectangle to fit in dest bitmap
   if (pDC->rDest.top + cy > pDC->bmDest.iHeight)
      cy = pDC->bmDest.iHeight - pDC->rDest.top;
   if (pDC->rDest.left + cx > pDC->bmDest.iWidth)
      cx = pDC->bmDest.iWidth - pDC->rDest.left;
   for (y=0; y<cy; y++)
      {
      for (x=0; x<cx; x++)
         {
         pDest[x] = usColor;
         }
      pDest += (pDC->bmDest.iPitch>>1);
      }
} /* BBGFill() */
