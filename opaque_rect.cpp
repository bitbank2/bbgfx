#include <windows.h>
#include "bbgfx.h"
//
// Test a vertical line for translucent
// returns true if tranlucent pixels found on the line
//
#define DIVIDE_BY_255(thisInput) ((thisInput+(((thisInput+128)>>8)+128))>>8 )

static inline void alphaBlend(const unsigned char &alpha, const unsigned short &srcPixel, unsigned short &dstPixel, unsigned char &dstAlpha)
{
unsigned long ulRSrc, ulRDest;
unsigned long ulGSrc, ulGDest;
unsigned long ulBSrc, ulBDest;
unsigned long ulAlphaNew, ulRNew, ulGNew, ulBNew;


    if (alpha == 255) // Src alpha = 255 or dest alpha == 0 -> just store
    {
        dstPixel = srcPixel;
    }
    else if (alpha == 0) // src alpha = 0, no change to dest
    {
        return;
    }
    else if (dstAlpha == 0) // src alpha > 0, dest alpha == 0
    {
        dstPixel = srcPixel;
    }
    else // slow case - need to do complete calculation
    {
        unsigned long tmpVar1, tmpVar2;
        unsigned long tmpVar3;

        ulRSrc = (srcPixel >> 8) & 0xf8;
        ulGSrc = (srcPixel >> 3) & 0xfc;
        ulBSrc = (srcPixel & 0x1f) << 3;

        ulRDest = (dstPixel >> 8) & 0xf8;
        ulGDest = (dstPixel >> 3) & 0xfc;
        ulBDest = (dstPixel & 0x1f) << 3;

        // Calculate new alpha
        tmpVar1 = alpha * dstAlpha;
        ulAlphaNew = alpha + dstAlpha - DIVIDE_BY_255(tmpVar1);

        if (ulAlphaNew > 255)
        {
            ulAlphaNew = 255; // clip to 255
        }

        tmpVar1 = 255 - alpha;
        // red
        tmpVar2 = ulRSrc * alpha;
        tmpVar3 = ulRDest * dstAlpha * tmpVar1;
        ulRNew = DIVIDE_BY_255(tmpVar2)+DIVIDE_BY_255(DIVIDE_BY_255(tmpVar3));
        // green
        tmpVar2 = ulGSrc * alpha;
        tmpVar3 = ulGDest * dstAlpha * tmpVar1;
        ulGNew = DIVIDE_BY_255(tmpVar2)+DIVIDE_BY_255(DIVIDE_BY_255(tmpVar3));
        // blue
        tmpVar2 = ulBSrc * alpha;
        tmpVar3 = ulBDest * dstAlpha * tmpVar1;
        ulBNew = DIVIDE_BY_255(tmpVar2)+DIVIDE_BY_255(DIVIDE_BY_255(tmpVar3));

        dstAlpha = (unsigned char)ulAlphaNew;
        dstPixel = (unsigned short)(((ulRNew & 0xf8)<<8) | ((ulGNew & 0xfc)<<3) | (ulBNew >> 3));
    } // slow case
} /* alphaBlend() */

//
// Test a vertical line for translucent
// returns true if tranlucent pixels found on the line
//
BOOL TestTranslucentV(BBBitmap *pBM
, int iLeft, int iTop, int iBottom)
{
    unsigned long *pul;
    unsigned char *puc;
    int y;
    BOOL bReturn = FALSE; // assume all is well (no translucent pixels)

    if (iTop >= iBottom)
        goto Exit; // bogus area

    if (pBM->iBitsPerPixel == 24)
    {
        puc = pBM->pBits + (pBM->iPitch * pBM->iHeight); // point to alpha plane
        puc += (iTop * (pBM->iPitch>>1)) + iLeft; // point to specific pixel
        for (y=iTop; y<=iBottom; y++)
        {
            if (*puc != 255)
            {
                bReturn = TRUE; // hit a translucent pixel; leave
                goto Exit;
            }
            puc += (pBM->iPitch>>1);
        }
    }
    else // 32bpp
    {
        pul = (unsigned long *)(pBM->pBits + (iTop * pBM->iPitch) + (iLeft * sizeof(DWORD))); // point to alpha plane
        for (y=iTop; y<=iBottom; y++)
        {
            if (*pul < 0xff000000)
            {
                bReturn = TRUE; // hit a translucent pixel; leave
                goto Exit;
            }
            pul += (pBM->iPitch>>2);
        }
    }
Exit:
    return bReturn;

} /* TestTranslucentV() */

//
// Test a horizontal line for translucent
// returns true if tranlucent pixels found on the line
//
BOOL TestTranslucentH(BBBitmap *pBM, int iTop, int iLeft, int iRight)
{
    unsigned long *pul;
    unsigned char *puc;
    int x;
    BOOL bReturn = FALSE; // assume all is well (no translucent pixels)

    if (iLeft >= iRight)
        goto Exit; // bogus area

    if (pBM->iBitsPerPixel == 24)
    {
        puc = pBM->pBits + (pBM->iPitch * pBM->iHeight); // point to alpha plane
        puc += (iTop * (pBM->iPitch>>1)) + iLeft; // point to specific pixel
        for (x=iLeft; x<=iRight; x++)
        {
            if (*puc != 255)
            {
                bReturn = TRUE; // hit a translucent pixel; leave
                goto Exit;
            }
            puc++;
        }
    }
    else // 32bpp
    {
        pul = (unsigned long *)(pBM->pBits + (iTop * pBM->iPitch) + (iLeft * sizeof(DWORD))); // point to alpha plane
        for (x = iLeft; x <= iRight; x++)
        {
            if (*pul < 0xff000000)
            {
                bReturn = TRUE; // hit a translucent pixel; leave
                goto Exit;
            }
            pul++;
        }
    }
Exit:
    return bReturn;

} /* TestTranslucentH() */

//
// Find the largest opaque rectangle in the center of a bitmap with alpha channel
// this function assumes that the translucent parts will be along the edge(s)
// The purpose of this function is to allow faster blitting of images with alpha channel
// since the "blend" calculations for 32-bit pixels are expensive, the opaque part can be
// drawn much faster
// change history:
// written by Larry Bank 8/21/2010
//
#define FLAG_LEFT   1
#define FLAG_RIGHT  2
#define FLAG_TOP    4
#define FLAG_BOTTOM 8

void FindOpaqueRectangle(BBBitmap *pBM, BBRect *pRect)
{
unsigned char ucFlags; // bit flags indicating which directions are active

    // Only valid for 24+32bpp images
    if (pBM->iBitsPerPixel <= 16)
    {
        memset(pRect, 0, sizeof(BBRect));
        return;
    }
    // Start with all directions active
    ucFlags = (FLAG_LEFT | FLAG_RIGHT | FLAG_TOP | FLAG_BOTTOM);

    // Start as 1 point in the center of the bitmap
    pRect->left = pRect->right = pBM->iWidth / 2;
    pRect->top = pRect->bottom = pBM->iHeight / 2;

    // while search is still active
    while (ucFlags) 
    {
        // Increment in each active direction
        if (ucFlags & FLAG_LEFT)
        {
            if (pRect->left == 0) // we're done
            {
                ucFlags &= ~FLAG_LEFT;
            }
            else
            {
                pRect->left--;
            }
        } // left

        if (ucFlags & FLAG_RIGHT)
        {
            if (pRect->right == pBM->iWidth) // we're done
            {
                ucFlags &= ~FLAG_RIGHT;
            }
            else
            {
                pRect->right++;
            }
        } // right

        if (ucFlags & FLAG_TOP)
        {
            if (pRect->top == 0) // we're done
            {
                ucFlags &= ~FLAG_TOP;
            }
            else
            {
                pRect->top--;
            }
        } // top

        if (ucFlags & FLAG_BOTTOM)
        {
            if (pRect->bottom == pBM->iHeight) // we're done
            {
                ucFlags &= ~FLAG_BOTTOM;
            }
            else
            {
                pRect->bottom++;
            }
        } // bottom

        // Test the pixels along each side of the rectangle to see if we can continue
        if (ucFlags & FLAG_LEFT) // test left edge
        {
            if (TestTranslucentV(pBM, pRect->left, pRect->top+1, pRect->bottom-1))
            {
                pRect->left++; // can't count that line
                ucFlags &= ~FLAG_LEFT;
            }
        } // left

        if (ucFlags & FLAG_RIGHT) // test right edge
        {
            if (TestTranslucentV(pBM, pRect->right, pRect->top+1, pRect->bottom-1))
            {
                pRect->right--; // can't count that line
                ucFlags &= ~FLAG_RIGHT;
            }
        } // right

        if (ucFlags & FLAG_TOP) // test top edge
        {
            if (TestTranslucentH(pBM, pRect->top, pRect->left+1, pRect->right-1))
            {
                pRect->top++; // can't count that line
                ucFlags &= ~FLAG_TOP;
            }
        } // top

        if (ucFlags & FLAG_BOTTOM) // test bottom edge
        {
            if (TestTranslucentH(pBM, pRect->bottom, pRect->left+1, pRect->right-1))
            {
                pRect->bottom--; // can't count that line
                ucFlags &= ~FLAG_BOTTOM;
            }
        } // bottom
    } // while

} /* FindOpaqueRectangle() */
