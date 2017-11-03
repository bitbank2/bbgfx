/* MINI_PIL - mini portable imaging library */
/* Copyright (c) 2000-2017 BitBank Software, Inc. */
/* Written by Larry Bank */
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
#include <ctype.h>
#include <math.h>
#include "mini_io.h"
#include "mini_pil.h"
#include <zlib.h>

#define PIL_IMAGE_READ  // include code to read images
#define PIL_IMAGE_WRITE  // include code to write images

static void JPEGPutMCUGray(PIL_PAGE *inpage, unsigned char *pSrc, unsigned char *cOutput, JPEGDATA *pJPEG, int x, int y, int lsize);
static void PILStoreCode(PIL_CODE *pPC, unsigned long ulCode, int iLen);
static void PILFlushCode(PIL_CODE *pPC);
static void JPEGGetMCU22(unsigned char *pImage, PIL_PAGE *pPage, int lsize, int x, int y, signed short *pMCUData);
static int PILReadBMP(PIL_PAGE *pInPage, PIL_PAGE *pOutPage);

/* GIF Defines and variables */
#define CTLINK 0
#define CTLAST 4096
#define CTFIRST 8192
#define MAX_HASH 5003
#define MAXMAXCODE 4096
static unsigned char cGIFBits[8] = {1,4,4,4,8,8,8,8}; // convert odd bpp values to ones we can handle
static unsigned char cGIFPass[8] = {8,0,8,4,4,2,2,1}; // GIF interlaced y delta

// Bit cutoffs for fast/slow huffman decode tables
#define FAST_DC_BITS 6
#define FAST_DC_MASK 0x3f
#define FAST_AC_BITS 8
#define FAST_AC_MASK 0xff

// JPEG magnitude values (number of bits needed)
static unsigned char cMagnitudes[128] = {0,1,2,2,3,3,3,3,4,4,4,4,4,4,4,4,
                                  5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
                                  6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
                                  6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
                                  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
                                  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
                                  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
                                  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7};

// 3-3-2 palette
// Blue 8-bit palette values
static unsigned char b_pal[4] = {00,0x55,0xaa,0xff};
// Red and Green 8-bit palette values
static unsigned char rg_pal[8] = {00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff};

// a 4 entry palette used for 2bpp windows bitmaps "2BP"
static unsigned char c2bpppal[12] = {0,0,0,85,85,85,170,170,170,255,255,255};

/* JPEG tables */
// zigzag ordering of DCT coefficients
static unsigned char cZigZag[64] = {0,1,5,6,14,15,27,28,
                    2,4,7,13,16,26,29,42,
                    3,8,12,17,25,30,41,43,
                    9,11,18,24,31,40,44,53,
                    10,19,23,32,39,45,52,54,
                    20,22,33,38,46,51,55,60,
                    21,34,37,47,50,56,59,61,
                    35,36,48,49,57,58,62,63};

// needed for H263 decoder
static unsigned char cZigZag2[64] = {0,1,8,16,9,2,3,10,
                     17,24,32,25,18,11,4,5,
                     12,19,26,33,40,48,41,34,
                     27,20,13,6,7,14,21,28,
                     35,42,49,56,57,50,43,36,
                     29,22,15,23,30,37,44,51,
                     58,59,52,45,38,31,39,46,
                     53,60,61,54,47,55,62,63};

// For AA&N IDCT method, multipliers are equal to quantization
// coefficients scaled by scalefactor[row]*scalefactor[col], where
// scalefactor[0] = 1
// scalefactor[k] = cos(k*PI/16) * sqrt(2)    for k=1..7
// For integer operation, the multiplier table is to be scaled by
// IFAST_SCALE_BITS.
static int iScaleBits[64] = {16384, 22725, 21407, 19266, 16384, 12873,  8867,  4520,
                     22725, 31521, 29692, 26722, 22725, 17855, 12299,  6270,
                     21407, 29692, 27969, 25172, 21407, 16819, 11585,  5906,
                     19266, 26722, 25172, 22654, 19266, 15137, 10426,  5315,
                     16384, 22725, 21407, 19266, 16384, 12873,  8867,  4520,
                     12873, 17855, 16819, 15137, 12873, 10114,  6967,  3552,
                     8867, 12299, 11585, 10426,  8867,  6967,  4799,  2446,
                     4520,  6270,  5906,  5315,  4520,  3552,  2446,  1247};

//
// Typical DC difference huffman tables for luminance and chrominance
//
static unsigned char huffl_dc[28] = {0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,
                              0,1,2,3,4,5,6,7,8,9,0xa,0xb};
static unsigned char huffcr_dc[28] = {0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,
                               0,1,2,3,4,5,6,7,8,9,0xa,0xb};
//
// Typical AC difference huffman tables for luminance and chrominance
//
static unsigned char huffl_ac[256] = {0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,0x7d,
              1,2,3,0,4,0x11,5,0x12,0x21,0x31,0x41,6,0x13,0x51,0x61,7,
              0x22,0x71,0x14,0x32,0x81,0x91,0xa1,8,0x23,0x42,0xb1,0xc1,0x15,0x52,0xd1,0xf0,
              0x24,0x33,0x62,0x72,0x82,9,0xa,0x16,0x17,0x18,0x19,0x1a,0x25,0x26,0x27,0x28,
              0x29,0x2a,0x34,0x35,0x36,0x37,0x38,0x39,0x3a,0x43,0x44,0x45,0x46,0x47,0x48,0x49,
              0x4a,0x53,0x54,0x55,0x56,0x57,0x58,0x59,0x5a,0x63,0x64,0x65,0x66,0x67,0x68,0x69,
              0x6a,0x73,0x74,0x75,0x76,0x77,0x78,0x79,0x7a,0x83,0x84,0x85,0x86,0x87,0x88,0x89,
              0x8a,0x92,0x93,0x94,0x95,0x96,0x97,0x98,0x99,0x9a,0xa2,0xa3,0xa4,0xa5,0xa6,0xa7,
              0xa8,0xa9,0xaa,0xb2,0xb3,0xb4,0xb5,0xb6,0xb7,0xb8,0xb9,0xba,0xc2,0xc3,0xc4,0xc5,
              0xc6,0xc7,0xc8,0xc9,0xca,0xd2,0xd3,0xd4,0xd5,0xd6,0xd7,0xd8,0xd9,0xda,0xe1,0xe2,
              0xe3,0xe4,0xe5,0xe6,0xe7,0xe8,0xe9,0xea,0xf1,0xf2,0xf3,0xf4,0xf5,0xf6,0xf7,0xf8,
              0xf9,0xfa};
static unsigned char huffcr_ac[256] = {0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,0x77,
              0,1,2,3,0x11,4,5,0x21,0x31,6,0x12,0x41,0x51,7,0x61,0x71,
              0x13,0x22,0x32,0x81,0x8,0x14,0x42,0x91,0xa1,0xb1,0xc1,9,0x23,0x33,0x52,0xf0,
              0x15,0x62,0x72,0xd1,0xa,0x16,0x24,0x34,0xe1,0x25,0xf1,0x17,0x18,0x19,0x1a,0x26,
              0x27,0x28,0x29,0x2a,0x35,0x36,0x37,0x38,0x39,0x3a,0x43,0x44,0x45,0x46,0x47,0x48,
              0x49,0x4a,0x53,0x54,0x55,0x56,0x57,0x58,0x59,0x5a,0x63,0x64,0x65,0x66,0x67,0x68,
              0x69,0x6a,0x73,0x74,0x75,0x76,0x77,0x78,0x79,0x7a,0x82,0x83,0x84,0x85,0x86,0x87,
              0x88,0x89,0x8a,0x92,0x93,0x94,0x95,0x96,0x97,0x98,0x99,0x9a,0xa2,0xa3,0xa4,0xa5,
              0xa6,0xa7,0xa8,0xa9,0xaa,0xb2,0xb3,0xb4,0xb5,0xb6,0xb7,0xb8,0xb9,0xba,0xc2,0xc3,
              0xc4,0xc5,0xc6,0xc7,0xc8,0xc9,0xca,0xd2,0xd3,0xd4,0xd5,0xd6,0xd7,0xd8,0xd9,0xda,
              0xe2,0xe3,0xe4,0xe5,0xe6,0xe7,0xe8,0xe9,0xea,0xf2,0xf3,0xf4,0xf5,0xf6,0xf7,0xf8,
              0xf9,0xfa};
// Sample quantization table which can be divided by 2 or 4 or multiplied by 2
// to give different quality levels
static unsigned char quant_lum[64] =
        {16,11,12,14,12,10,16,14,13,14,18,17,16,19,24,
        40,26,24,22,22,24,49,35,37,29,40,58,51,61,60,
        57,51,56,55,64,72,92,78,64,68,87,69,55,56,80,
        109,81,87,95,98,103,104,103,62,77,113,121,112,
        100,120,92,101,103,99};
static unsigned char quant_color[64] =
        {17,18,18,24,21,24,47,26,26,47,99,66,56,66,99,99,
        99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,
        99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,
        99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99};


static int iBitMasks[33] = {0,1,3,7,0xf,0x1f,0x3f,0x7f,0xff,0x1ff,0x3ff,0x7ff,0x0fff,0x1fff,0x3fff,0x7fff,0xffff,0x1ffff,0x3ffff,0x7ffff,0xfffff,0x1fffff,0x3fffff,0x7fffff,0xffffff,0x1ffffff,0x3ffffff,0x7ffffff,0xfffffff,0x1fffffff,0x3fffffff,0x7fffffff,0xffffffff};
static int PILCalcBSize(int x, int bpp);
int mPILFlipv(PIL_PAGE *pPage);
static int PILInvert(PIL_PAGE *pPage);
unsigned char * PILEncodeLine(int xsize, unsigned char *irlcptr, unsigned char *buf);
static void PILFixTIFFRGB(unsigned char *p, int xsize);
static unsigned short PILTIFFSHORT(unsigned char *p, BOOL bMotorola);
static unsigned long PILTIFFLONG(unsigned char *p, BOOL bMotorola);
int PILFixBitDir(PIL_PAGE *);
void PILDraw1Line(unsigned char *irlcptr, unsigned char *pDest);
static void JPEGGetHuffTables(unsigned char *pBuf, int iLen, JPEGDATA *pJPEG);
static void JPEGPutMCU11(PIL_PAGE *inpage, int x, int y, int lsize, signed short *pMCU, unsigned char *cOutput, JPEGDATA *pJPEG);
static void JPEGPutMCU11HALF(PIL_PAGE *inpage, int x, int y, int lsize, signed short *pMCU, unsigned char *cOutput, JPEGDATA *pJPEG);
static void JPEGPutMCU11QUARTER(PIL_PAGE *inpage, int x, int y, int lsize, signed short *pMCU, unsigned char *cOutput, JPEGDATA *pJPEG);
static void JPEGPutMCU12(PIL_PAGE *inpage, int x, int y, int lsize, signed short *pMCU, unsigned char *cOutput, JPEGDATA *pJPEG);
static void JPEGPutMCU12HALF(PIL_PAGE *inpage, int x, int y, int lsize, signed short *pMCU, unsigned char *cOutput, JPEGDATA *pJPEG);
static void JPEGPutMCU12QUARTER(PIL_PAGE *inpage, int x, int y, int lsize, signed short *pMCU, unsigned char *cOutput, JPEGDATA *pJPEG);
static void JPEGPutMCU21(PIL_PAGE *inpage, int x, int y, int lsize, signed short *pMCU, unsigned char *cOutput, JPEGDATA *pJPEG);
static void JPEGPutMCU21HALF(PIL_PAGE *inpage, int x, int y, int lsize, signed short *pMCU, unsigned char *cOutput, JPEGDATA *pJPEG);
static void JPEGPutMCU21QUARTER(PIL_PAGE *inpage, int x, int y, int lsize, signed short *pMCU, unsigned char *cOutput, JPEGDATA *pJPEG);
static void JPEGPutMCU22HALF(PIL_PAGE *inpage, int x, int y, int lsize, signed short *pMCU, unsigned char *cOutput, JPEGDATA *pJPEG);
static void JPEGPutMCU22QUARTER(PIL_PAGE *inpage, int x, int y, int lsize, signed short *pMCU, unsigned char *cOutput, JPEGDATA *pJPEG);
static void JPEGPutMCU22(PIL_PAGE *inpage, int x, int y, int lsize, signed short *pMCU, unsigned char *cOutput, JPEGDATA *pJPEG);
static void JPEGFixQuantD(JPEGDATA *pJPEG);
static void JPEGIDCT(JPEGDATA *pJPEG, signed short *pMCUSrc, int iQuantTable);
static int JPEGDecodeMCU(unsigned char *pBuf, int *iOff, int *iBit, signed short *pMCU, JPEGDATA *pJPEG, int *iDCPredictor);
static int JPEGDecodeMCUFast(unsigned char *pBuf, int *iOff, int *iBit, JPEGDATA *pJPEG, int *iDCPredictor);
static int GetJPEGxx(PIL_PAGE *inpage, int *iOff, int *iBit, signed short *pMCU, JPEGDATA *pJPEG, unsigned char *cOutput);
static int GetJPEGxxThumb(PIL_PAGE *inpage, int *iOff, int *iBit, signed short *pMCU, JPEGDATA *pJPEG, unsigned char *cOutput);
static int PILReadJPEG(PIL_PAGE *inpage, PIL_PAGE *outpage, int iOptions, BOOL bMJPEG);
static int PILReadPNG(PIL_PAGE *pInPage, PIL_PAGE *TempPage, int iOtions);
static void PILReadBlock(PIL_PAGE *pPage, int *iOff);
static int PILTIFFVALUE(unsigned char *p, BOOL bMotorola);

#ifndef _WIN32
char *strupr (char *a)
{
	char *ret = a;

	while (*a != '\0')
	{
		if (islower (*a))
		{
			*a = toupper (*a);
		}

		++a;
	}

	return ret;
}
#endif	// #ifndef _WIN32

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILFixTIFFRGB(char *, int)                                 *
 *                                                                          *
 *  PURPOSE    : Fix the byte order of 24bpp TIFF data.                     *
 *                                                                          *
 ****************************************************************************/
static void PILFixTIFFRGB(unsigned char *p, int xsize) /* Reverse Red/Blue order */
{
unsigned char c, *pEnd;

   pEnd = p + xsize * 3;
   while (p < pEnd)
      {
      c = p[2];
      p[2] = p[0];
      p[0] = c;
      p += 3;
      }

} /* PILFixTIFFRGB() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGGetHuffTables(char *, int , JPEGDATA *)                *
 *                                                                          *
 *  PURPOSE    : Get the Huffman table(s) into my internal structure.       *
 *                                                                          *
 ****************************************************************************/
static void JPEGGetHuffTables(unsigned char *pBuf, int iLen, JPEGDATA *pJPEG)
{
int i, j, iOffset, iTableOffset;
unsigned char ucTable;

   iOffset = 0;
   while (iLen > 0)  // while there are tables to copy
      {
      ucTable = pBuf[iOffset++]; // get table index
      if (ucTable & 0x10) // convert AC offset of 0x10 into offset of 4
         ucTable ^= 0x14;
      pJPEG->ucHuffTableUsed[ucTable] = 1; // mark this table as being defined
      if (ucTable <= 7) // tables are 0-3, AC+DC
         {
         iTableOffset = ucTable * HUFF_TABLEN;
         j = 0; // total bits
         for (i=0; i<16; i++)
            {
            j += pBuf[iOffset];
            pJPEG->ucHuffVals[iTableOffset+i] = pBuf[iOffset++];
            }
         iLen -= 17; // subtract length of bit lengths
         if (j == 0 || j > 256 || j > iLen) // bogus bit lengths
            {
            }
         iTableOffset += 16;
         for (i=0; i<j; i++)
            {  // copy huffman table
            pJPEG->ucHuffVals[iTableOffset+i] = pBuf[iOffset++];
            }
         iLen -= j;
         }
      }
} /* JPEGGetHuffTables() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGProcessTables(char *, int *, char *)                   *
 *                                                                          *
 *  PURPOSE    : Read useful data tables from the header (markers).         *
 *                                                                          *
 ****************************************************************************/
static unsigned char JPEGProcessTables(unsigned char *buf, int *iOff, JPEGDATA *pJPEG, int iDataSize)
{
int iOffset = *iOff;
int i, iTableOffset;
unsigned char cMarker, ucTable;
unsigned short sLen;
BOOL bDone;

   bDone = FALSE;
   cMarker = 0;
   while (!bDone && iOffset < PIL_BUFFER_SIZE && iOffset < (iDataSize+1))
      {
      if (buf[iOffset] != 0xff) // invalid marker, try to resync
         {
         iOffset += 2;
         continue;
         }
      cMarker = buf[iOffset+1];
      iOffset += 2;
      sLen = MOTOSHORT(&buf[iOffset]);
      switch (cMarker)
         {
         case 0xe1: /* APP1 = EXIF info */
            if (memcmp(&buf[iOffset+2],"Exif",4) == 0) // YES!
               {
               if (sLen > 8) // if there is any EXIF info (found a file with EXIF header and no info)
                  pJPEG->iEXIF = iOffset+8; // save the EXIF offset for later
               }
            iOffset += (int)sLen;   // skip this data for now
            break;
         case 0xe0: /* APP0 */
            iOffset += 2; // skip length
            sLen -= 2;
            pJPEG->xdpi = MOTOSHORT(&buf[iOffset + 10]);
            pJPEG->ydpi = MOTOSHORT(&buf[iOffset + 12]);
            if (buf[iOffset + 9] != 1) /* If not DPI, convert DPCM */
               {
               pJPEG->xdpi = (pJPEG->xdpi * 254) / 100;
               pJPEG->ydpi = (pJPEG->ydpi * 254) / 100;
               }
            iOffset += (int)sLen;
            break;
         case 0xcc: /* M_DAC */ // I don't support arithmetic tables, so ignore
            iOffset += (int)sLen;
            break;
         case 0xdb: /* M_DQT */
/* Get the quantization tables */
/* first byte has PPPPNNNN where P = precision and N = table number 0-3 */
            iOffset += 2; // skip length
            sLen -= 2; // subtract length length
            while (sLen > 0)
               {
               ucTable = buf[iOffset++]; // table number
               iTableOffset = (ucTable & 0xf) * DCTSIZE2;
               if (ucTable & 0xf0) // if word precision
                  {
                  for (i=0; i<DCTSIZE2; i++)
                     {
                     pJPEG->sQuantTable[i+iTableOffset] = MOTOSHORT(&buf[iOffset]);
                     iOffset += 2;
                     }
                  sLen -= (DCTSIZE2*2 + 1);
                  }
               else
                  {
                  for (i=0; i<DCTSIZE2; i++)
                     {
                     pJPEG->sQuantTable[i+iTableOffset] = (unsigned short)buf[iOffset++];
                     }
                  sLen -= (DCTSIZE2 + 1);
                  }
               }
            break;
         case 0xc4: /* M_DHT */ // get Huffman tables
            iOffset += 2; // skip length
            sLen -= 2; // subtract length length
            JPEGGetHuffTables(&buf[iOffset], sLen, pJPEG);
            iOffset += sLen; // skip to next marker
            break;
         case 0xdd: /* M_DRI - restart interval */
            if (sLen == 4)
               pJPEG->iResInterval = pJPEG->iResCount = MOTOSHORT(&buf[iOffset+2]);
            iOffset += sLen;
            break;
         case 0xfe: /* M_COM - comment - copy to page structure */
            i = sLen-2;
            if (i > 127)
               i = 127; // max length we can handle
            memset(pJPEG->szComment, 0, 128);
            memcpy(pJPEG->szComment, &buf[iOffset+2], i);
            iOffset += sLen;
            break;
         case 0xd8: /* SOI, return */
         case 0xd9: /* EOI, return */
         case 0xda: /* SOS, return */
            bDone = TRUE;
            break;
         case 0xc0:  // baseline sequential DCT
         case 0xc1:  // Extended sequential DCT
         case 0xc2:  // Progressive baseline DCT
         case 0xc3:  // lossless (sequential)
            bDone = TRUE;
            break;
         case 0xff: // not sure why, but Ricoh adds an extra 0xff
            iOffset--;
            break;
         default: /* Skip the unknown marker */
            iOffset += (int)sLen;
            break;
         }
      }

   *iOff = iOffset;
   return cMarker; /* Return the marker which allowed us to exit from the loop */

} /* JPEGProcessTables() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGGetSOI(JPEGDATA *)                                     *
 *                                                                          *
 *  PURPOSE    : Process the start of image marker.                         *
 *                                                                          *
 ****************************************************************************/
static void JPEGGetSOI(JPEGDATA *pJPEG)
{
   pJPEG->iResInterval = 0;
   pJPEG->xdpi = pJPEG->ydpi = 1;  // assume no valid DPI info

} /* JPEGGetSOI() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGGetSOF(char *, int *, JPEGDATA *)                      *
 *                                                                          *
 *  PURPOSE    : Process the start of frame marker.                         *
 *                                                                          *
 ****************************************************************************/
static BOOL JPEGGetSOF(unsigned char *buf, int *iOff, JPEGDATA *pJPEG)
{
int iOffset = *iOff;
int i;
unsigned short us, usLen;
unsigned char ucSamp;

   usLen = MOTOSHORT(&buf[iOffset]);
   iOffset += 2;
   if (buf[iOffset++] != 8) // we only support 8 bits per pixel
      return TRUE;
   us = MOTOSHORT(&buf[iOffset]); // ysize
   iOffset += 2;
   pJPEG->cy = us;
   us = MOTOSHORT(&buf[iOffset]); // xsize
   iOffset += 2;
   pJPEG->cx = us;
   pJPEG->ucNumComponents = buf[iOffset++]; // number of color components
   if (usLen != (8 + pJPEG->ucNumComponents*3)) // invalid length
      return TRUE;
   for (i=0; i<pJPEG->ucNumComponents; i++)
      {
      pJPEG->JPCI[i].component_id = buf[iOffset++];
      pJPEG->JPCI[i].component_index = (unsigned char)i;
      ucSamp = buf[iOffset++]; // get the h+v sampling factor
      if (i == 0) // Y component?
         pJPEG->jpegsample = ucSamp;
      pJPEG->JPCI[i].h_samp_factor = ucSamp >> 4;
      pJPEG->JPCI[i].v_samp_factor = ucSamp & 0xf;
      pJPEG->JPCI[i].quant_tbl_no = buf[iOffset++]; // quantization table number
      }
   if (pJPEG->ucNumComponents == 1)
      pJPEG->jpegsample = 0; // use this to differentiate from color 1:1
   *iOff = iOffset;
   return FALSE;

} /* JPEGGetSOF() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGGetSOS(char *, int *, JPEGDATA *)                      *
 *                                                                          *
 *  PURPOSE    : Process the start of scan marker.                          *
 *                                                                          *
 ****************************************************************************/
static BOOL JPEGGetSOS(unsigned char *buf, int *iOff, JPEGDATA *pJPEG)
{
short sLen;
int iOffset = *iOff;
int i, j;
unsigned char uc,c,cc;

    sLen = MOTOSHORT(&buf[iOffset]);
    iOffset += 2;

 // Assume no components in this scan
    for (i=0; i<4; i++)
       pJPEG->JPCI[i].component_needed = 0;

    uc = buf[iOffset++]; // get number of components
    pJPEG->ucComponentsInScan = uc;
    sLen -= 3;
    if (uc < 1 || uc > MAX_COMPS_IN_SCAN || sLen != (uc*2+3)) // check length of data packet
       return TRUE; // error
    for (i=0; i<uc; i++)
       {
       cc = buf[iOffset++];
       c = buf[iOffset++];
       sLen -= 2;
       for (j=0; j<4; j++) // search for component id
          {
          if (pJPEG->JPCI[j].component_id == cc)
             break;
          }
       if (j == 4) // error, not found
          return TRUE;
       pJPEG->JPCI[j].dc_tbl_no = c >> 4;
       pJPEG->JPCI[j].ac_tbl_no = c & 0xf;
       pJPEG->JPCI[j].component_needed = 1; // mark this component as being included in the scan
       }
    pJPEG->iScanStart = buf[iOffset++]; // Get the scan start for this scan
    pJPEG->iScanEnd = buf[iOffset++]; // Get the scan end for this scan
    c = buf[iOffset++]; // successive approximation bits
    pJPEG->cApproxBitsLow = c & 0xf;
    pJPEG->cApproxBitsHigh = c >> 4;

    *iOff = iOffset;
    return FALSE;

} /* JPEGGetSOS() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGMakeHuffTables(JPEGDATA *)                             *
 *                                                                          *
 *  PURPOSE    : Create the expanded Huffman tables for fast decode.        *
 *                                                                          *
 ****************************************************************************/
static void JPEGMakeHuffTables(JPEGDATA *pJPEG)
{
int code, repeat, count, codestart;
int j;
int iLen, iTable;
unsigned short *pTable, *pShort, *pLong;
unsigned long ul, *pLongTable;
int iBitNum; // current code bit length
int cc; // code
unsigned char *p, *pBits;

// first do DC components (up to 4 tables of 12-bit codes)
// we can save time and memory for the DC codes by knowing that there exist short codes (<= 6 bits)
// and long codes (>6 bits, but the first 5 bits are 1's).  This allows us to create 2 tables: a 6-bit and 7-bit
// to handle any DC codes
   for (iTable = 0; iTable < 4; iTable++)
      {
      if (pJPEG->ucHuffTableUsed[iTable])
         {
//         pJPEG->huffdcFast[iTable] = (int *)MiniIOAlloc(0x180); // short table = 128 bytes, long table = 256 bytes
         pShort = (unsigned short *)pJPEG->huffdcFast[iTable];
//         pJPEG->huffdc[iTable] = pJPEG->huffdcFast[iTable] + 0x20; // 0x20 longs = 128 bytes
         pLong = (unsigned short *)pJPEG->huffdc[iTable];
         pBits = &pJPEG->ucHuffVals[iTable * HUFF_TABLEN];
         p = pBits;
         p += 16; // point to bit data
         cc = 0; // start with a code of 0
         for (iBitNum = 1; iBitNum <= 16; iBitNum++)
            {
            iLen = *pBits++; // get number of codes for this bit length
            while (iLen)
               {
//               if (iBitNum > 6) // do long table
               if ((cc >> (iBitNum-5)) == 0x1f) // first 5 bits are 1 - use long table
                  {
                  count = 12 - iBitNum;
                  codestart = cc << count;
                  pTable = &pLong[codestart & 0x7f]; // use lower 7 bits of code
                  }
               else // do short table
                  {
                  count = 6 - iBitNum;
                  codestart = cc << count;
                  pTable = &pShort[codestart];
                  }
               code = *p++;  // get actual huffman code
               code |= (iBitNum << 8);
               if (count) // do it as dwords to save time
                  {
                  pLongTable = (unsigned long *)pTable;
                  repeat = 1 << (count-1); // store as dwords (/2)
                  ul = code | (code << 16);
                  for (j=0; j<repeat; j++)
                     *pLongTable++ = ul;
                  }
               else
                  {
                  pTable[0] = (unsigned short)code;
                  }
               cc++;
               iLen--;
               }
            cc <<= 1;
            }
         } // if table defined
      }
// now do AC components (up to 4 tables of 16-bit codes)
// We split the codes into a short table (9 bits or less) and a long table (first 5 bits are 1)
   for (iTable = 0; iTable < 4; iTable++)
      {
      if (pJPEG->ucHuffTableUsed[iTable+4])  // if this table is defined
         {
         pBits = &pJPEG->ucHuffVals[(iTable+4) * HUFF_TABLEN];
         p = pBits;
         p += 16; // point to bit data
//         pJPEG->huffacFast[iTable] = (int *)MiniIOAlloc(0x1400); // fast table = 1024 bytes, slow = 4096
//         pJPEG->huffac[iTable] = pJPEG->huffacFast[iTable] + 0x100; // 0x100 longs = 1024 bytes
         pShort = (unsigned short *)pJPEG->huffacFast[iTable];
         pLong = (unsigned short *)pJPEG->huffac[iTable];
         cc = 0; // start with a code of 0
         // construct the decode table
         for (iBitNum = 1; iBitNum <= 16; iBitNum++)
            {
            iLen = *pBits++; // get number of codes for this bit length
            while (iLen)
               {
               if ((cc >> (iBitNum-6)) == 0x3f) // first 6 bits are 1 - use long table
                  {
                  count = 16 - iBitNum;
                  codestart = cc << count;
                  pTable = &pLong[codestart & 0x3ff]; // use lower 10 bits of code
                  }
               else
                  {
                  count = 10 - iBitNum;
                  codestart = cc << count;
                  pTable = &pShort[codestart]; // 10 bits or shorter
                  }
               code = *p++;  // get actual huffman code
               code |= (iBitNum << 8);
               if (count) // do it as dwords to save time
                  {
                  repeat = 1 << (count-1); // store as dwords (/2)
                  ul = code | (code << 16);
                  pLongTable = (unsigned long *)pTable;
                  for (j=0; j<repeat; j++)
                     *pLongTable++ = ul;
                  }
               else
                  {
                  pTable[0] = (unsigned short)code;
                  }
               cc++;
               iLen--;
               }
            cc <<= 1;
            } // for each bit length
         } // if table defined
      }

} /* JPEGMakeHuffTables() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGFixQuantE(JPEGDATA *)                                  *
 *                                                                          *
 *  PURPOSE    : Fix and reorder the quantization table for faster encoding.*
 *                                                                          *
 ****************************************************************************/
static void JPEGFixQuantE(JPEGDATA *pJPEG)
{
int iTable, iTableOffset;
signed short sTemp[DCTSIZE2];
int i, iCount;
signed short *p;

   if (pJPEG->ucNumComponents == 1)
      iCount = 1;
   else
      iCount = 2;
      
   for (iTable=0; iTable<iCount; iTable++)
      {
      iTableOffset = iTable * DCTSIZE2;
      p = (signed short *)&pJPEG->sQuantTable[iTableOffset];
      for (i=0; i<DCTSIZE2; i++)
          {
          sTemp[i] = p[cZigZag[i]];
          }
      memcpy(&pJPEG->sQuantTable[iTableOffset], sTemp, DCTSIZE2*sizeof(short)); // copy back to original spot

// Prescale for DCT multiplication
      p = (signed short *)&pJPEG->sQuantTable[iTableOffset];
      for (i=0; i<DCTSIZE2; i++)
         {
         p[i] = (short)((p[i] * iScaleBits[i]) >> 11);
         }
      }
} /* JPEGFixQuantE() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGGet32Bits(char *, int *)                               *
 *                                                                          *
 *  PURPOSE    : Get 32-bits from the JPEG stream, skipping markers.        *
 *                                                                          *
 ****************************************************************************/
static unsigned long JPEGGet32Bits(unsigned char *pBuf, int *iOff)
{
unsigned char ucByte;
unsigned long ulBits;
int iOffset  = *iOff;

getabyte:
   ucByte = pBuf[iOffset++]; // byte 0
   if (ucByte == 0xff) // stuffed 0?
      {
      if (pBuf[iOffset] != 0)
         {
         iOffset++; // skip marker
         goto getabyte; // try again
         }
      else
         iOffset++; // skip the stuffed zero
      }
   ulBits  = (unsigned long)(ucByte << 8);
getabyte2:
   ucByte = pBuf[iOffset++]; // byte 1
   ulBits |= ucByte;
   if (ucByte == 0xff) // stuffed 0?
      {
      if (pBuf[iOffset] != 0)
         {
         iOffset++; // skip marker
         goto getabyte2; // try again
         }
      else
         iOffset++; // skip the stuffed zero
      }
   ulBits <<= 8;
getabyte3:
   ucByte = pBuf[iOffset++]; // byte 1
   ulBits |= ucByte;
   if (ucByte == 0xff) // stuffed 0?
      {
      if (pBuf[iOffset] != 0)
         {
         iOffset++; // skip marker
         goto getabyte3; // try again
         }
      else
         iOffset++; // skip the stuffed zero
      }
   ulBits <<= 8;
getabyte4:
   ucByte = pBuf[iOffset++]; // byte 1
   ulBits |= ucByte;
   if (ucByte == 0xff) // stuffed 0?
      {
      if (pBuf[iOffset] != 0)
         {
         iOffset++; // skip marker
         goto getabyte4; // try again
         }
      else
         iOffset++; // skip the stuffed zero
      }
   *iOff = iOffset;

   return ulBits;

} /* JPEGGet32Bits() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGGet16Bits(char *, int *)                               *
 *                                                                          *
 *  PURPOSE    : Get 16-bits from the JPEG stream, skipping markers.        *
 *                                                                          *
 ****************************************************************************/
static unsigned long JPEGGet16Bits(unsigned char *pBuf, int *iOff)
{
unsigned long ucByte1, ucByte2;
unsigned long ulBits;
int iOffset  = *iOff;

// Code for most frequent case: both bytes are OK

   ucByte1 = pBuf[iOffset]; // need to separate the reads in case of odd address
   ucByte2 = pBuf[iOffset+1];
   if (ucByte1 == 0xff || ucByte2 == 0xff) // one or more markers/stuffed bytes - use slow procedure
      {
getabyte:
      ucByte1 = pBuf[iOffset++]; // byte 0
      if (ucByte1 == 0xff) // stuffed 0?
         {
         if (pBuf[iOffset] != 0)
            {
            iOffset++; // skip marker
            goto getabyte; // try again
            }
         else
            iOffset++; // skip the stuffed zero
         }
      ulBits = ucByte1 << 8;
getabyte2:
      ucByte1 = pBuf[iOffset++]; // byte 1
      if (ucByte1 == 0xff) // stuffed 0?
         {
         if (pBuf[iOffset] != 0)
            {
            iOffset++; // skip marker
            goto getabyte2; // try again
            }
         else
            iOffset++; // skip the stuffed zero
         }
      ulBits |= ucByte1;
      }
   else // probable case of 2 good bytes
      {
      iOffset += 2;
      ulBits = (ucByte1 << 8) | ucByte2;
      }
   *iOff = iOffset;
   return ulBits;

} /* JPEGGet16Bits() */

#ifdef PIL_IMAGE_WRITE
/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGFDCT(JPEGDATA *, int *)                                *
 *                                                                          *
 *  PURPOSE    : Perform forward discrete cosine transform on macro block.  *
 *                                                                          *
 ****************************************************************************/
static void JPEGFDCT(JPEGDATA *pJPEG, signed short *pMCUSrc)
{
int iCol, iRow;
signed int tmp0,tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7,tmp10,tmp11,tmp12,tmp13;
signed int z1,z2,z3,z4,z5,z11,z13;

    // do rows first
    for (iRow=0; iRow<64; iRow+=8)
       {
       tmp0 = pMCUSrc[iRow+0] + pMCUSrc[iRow+7];
       tmp7 = pMCUSrc[iRow+0] - pMCUSrc[iRow+7];
       tmp1 = pMCUSrc[iRow+1] + pMCUSrc[iRow+6];
       tmp6 = pMCUSrc[iRow+1] - pMCUSrc[iRow+6];
       tmp2 = pMCUSrc[iRow+2] + pMCUSrc[iRow+5];
       tmp5 = pMCUSrc[iRow+2] - pMCUSrc[iRow+5];
       tmp3 = pMCUSrc[iRow+3] + pMCUSrc[iRow+4];
       tmp4 = pMCUSrc[iRow+3] - pMCUSrc[iRow+4];
       // even part
       tmp10 = tmp0 + tmp3;
       tmp13 = tmp0 - tmp3;
       tmp11 = tmp1 + tmp2;
       tmp12 = tmp1 - tmp2;
       pMCUSrc[iRow+0] = (short)(tmp10 + tmp11);
       pMCUSrc[iRow+4] = (short)(tmp10 - tmp11);
       z1 = (((tmp12 + tmp13) * 181) >> 8);  // 181>>8 = 0.7071
       pMCUSrc[iRow+2] = (short)(tmp13 + z1);
       pMCUSrc[iRow+6] = (short)(tmp13 - z1);
       // odd part
       tmp10 = tmp4 + tmp5;
       tmp11 = tmp5 + tmp6;
       tmp12 = tmp6 + tmp7;
       z5 = (((tmp10 - tmp12) * 98) >> 8); // 98 >>8 = 0.3826
       z2 = z5 + ((tmp10 * 139) >> 8); // 139 >>8 = 0.541196
       z4 = z5 + ((tmp12 * 334) >> 8); // 334 >>8 = 1.3065
       z3 = ((tmp11 * 181) >> 8);
       z11 = tmp7 + z3;
       z13 = tmp7 - z3;
       pMCUSrc[iRow+5] = (short)(z13 + z2);
       pMCUSrc[iRow+3] = (short)(z13 - z2);
       pMCUSrc[iRow+1] = (short)(z11 + z4);
       pMCUSrc[iRow+7] = (short)(z11 - z4);
       } // for each row
    // now do the columns
    for (iCol=0; iCol < 8; iCol++)
       {
       tmp0 = pMCUSrc[iCol+0*8] + pMCUSrc[iCol+7*8];
       tmp7 = pMCUSrc[iCol+0*8] - pMCUSrc[iCol+7*8];
       tmp1 = pMCUSrc[iCol+1*8] + pMCUSrc[iCol+6*8];
       tmp6 = pMCUSrc[iCol+1*8] - pMCUSrc[iCol+6*8];
       tmp2 = pMCUSrc[iCol+2*8] + pMCUSrc[iCol+5*8];
       tmp5 = pMCUSrc[iCol+2*8] - pMCUSrc[iCol+5*8];
       tmp3 = pMCUSrc[iCol+3*8] + pMCUSrc[iCol+4*8];
       tmp4 = pMCUSrc[iCol+3*8] - pMCUSrc[iCol+4*8];
       // even part
       tmp10 = tmp0 + tmp3;
       tmp13 = tmp0 - tmp3;
       tmp11 = tmp1 + tmp2;
       tmp12 = tmp1 - tmp2;
       pMCUSrc[iCol+0] = (short)(tmp10 + tmp11);
       pMCUSrc[iCol+4*8] = (short)(tmp10 - tmp11);
       z1 = (((tmp12 + tmp13) * 181) >> 8);
       pMCUSrc[iCol+2*8] = (short)(tmp13 + z1);
       pMCUSrc[iCol+6*8] = (short)(tmp13 - z1);
       // odd part
       tmp10 = tmp4 + tmp5;
       tmp11 = tmp5 + tmp6;
       tmp12 = tmp6 + tmp7;
       z5 = (((tmp10 - tmp12) * 98) >> 8);
       z2 = z5 + ((tmp10 * 139) >> 8);
       z4 = z5 + ((tmp12 * 334) >> 8);
       z3 = (tmp11 * 181) >> 8;
       z11 = tmp7 + z3;
       z13 = tmp7 - z3;
       pMCUSrc[iCol+5*8] = (short)(z13 + z2);
       pMCUSrc[iCol+3*8] = (short)(z13 - z2);
       pMCUSrc[iCol+1*8] = (short)(z11 + z4);
       pMCUSrc[iCol+7*8] = (short)(z11 - z4);
       } // for each column

} /* JPEGFDCT() */
#endif // PIL_IMAGE_WRITE

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGInitTables(JPEGDATA *)                                 *
 *                                                                          *
 *  PURPOSE    : Initialize the color space conversion tables.              *
 *                                                                          *
 ****************************************************************************/
static void JPEGInitTables(JPEGDATA *pJPEG)
{
signed int i;
unsigned char uc;

/* Create a range clipping table for YCC to RGB color conversion */
/* Create range clipping table with conversion for 16bpp pixels */
   for (i=0; i<256; i++)
      {
      pJPEG->usRangeTableB[i] = (unsigned short)(i >> 3) & 0x1f; // lower 5 bits = Blue
      pJPEG->usRangeTableG[i] = (unsigned short)(i & 0xfc) << 3; // middle 6 bits = Green
      pJPEG->usRangeTableR[i] = (unsigned short)(i & 0xf8) << 8; // upper 5 bits = Red
      pJPEG->cRangeTable2[i] = (unsigned char)i;
      }
   memset(&pJPEG->cRangeTable2[256],0xff,256);   // 256-511 clipped to max value
   memset(&pJPEG->cRangeTable2[512],0,512);   // 512-1023 are negative clipped to 0
   uc = 255;
   for (i=256; i<512; i++)
      {
      pJPEG->usRangeTableB[i] = (uc >> 3) & 0x1f; // lower 5 bits = Blue
      pJPEG->usRangeTableG[i] = (uc & 0xfc) << 3; // middle 6 bits = Green
      pJPEG->usRangeTableR[i] = (uc & 0xf8) << 8; // upper 5 bits = Red
      }
} /* JPEGInitTables() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILReadBlock(PIL_PAGE *, int *)                            *
 *                                                                          *
 *  PURPOSE    : Re-align and read the next chunk of image data.            *
 *                                                                          *
 ****************************************************************************/
static void PILReadBlock(PIL_PAGE *pPage, int *iOff)
{
int i;

   i = *iOff & 0xfffff000; // round to nearest page size
   pPage->iFilePos -= (PIL_BUFFER_SIZE - i); // re-align
   MiniIOSeek(pPage->iHandle, pPage->iFilePos, 0);
   MiniIORead(pPage->iHandle, pPage->pData, PIL_BUFFER_SIZE);
   pPage->iFilePos += PIL_BUFFER_SIZE;
   *iOff = *iOff & 0xfff; // get offset within the page

} /* PILReadBlock() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILGrayPalette(int)                                        *
 *                                                                          *
 *  PURPOSE    : Allocate a 4 or 8bpp grayscale palette.                    *
 *                                                                          *
 ****************************************************************************/
static unsigned char * PILGrayPalette(int iBpp)
{
int i, iVal, iDelta;
unsigned char *p, *d;

   iVal = 0;
   if (iBpp == 8)
      iDelta = 1;
   else
      iDelta = 0x11;
   d = p = MiniIOAlloc(780); /* Allocate memory for a palette */
   for (i=0; i<256; i++)
      {
      *d++ = (unsigned char)iVal;  /* RGB values are all equal for gray */
      *d++ = (unsigned char)iVal;
      *d++ = (unsigned char)iVal;
      iVal += iDelta;
      }
   return p;

} /* PILGrayPalette() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILFreeHuffTables()                                        *
 *                                                                          *
 *  PURPOSE    : Free the Huffman decode tables.                            *
 *                                                                          *
 ****************************************************************************/
static void PILFreeHuffTables(JPEGDATA *pJPEG)
{

   memset(&pJPEG->ucHuffTableUsed, 0, sizeof(pJPEG->ucHuffTableUsed));
   return;

} /* PILFreeHuffTables() */

#ifdef PIL_IMAGE_WRITE
/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILPrepJPEGStruct(void)                                    *
 *                                                                          *
 *  PURPOSE    : Allocate and initialize the JPEG structure.                *
 *                                                                          *
 ****************************************************************************/
JPEGDATA * mPILPrepJPEGStruct(void)
{
JPEGDATA *p;
int i;

   p = (JPEGDATA *)MiniIOAlloc(sizeof(JPEGDATA));
   if (p)
      {
/* Create a range clipping table for results of multiplications */
      for (i=0; i<128; i++)
         {
         p->cRangeTable[i] = (unsigned char)(0x80 + i);
         p->cRangeTable[i+896] = (unsigned char)i;
         }
      for (i=0; i<384; i++)
         {
         p->cRangeTable[i+128] = 0xff;
         p->cRangeTable[i+512] = 0;
         }
      // setup the pointers to the Huffman decode tables
      for (i=0; i<4; i++)
         {
         p->huffdcFast[i] = (int *)&p->ucHuffACDCBuf[i*0x180]; // first 128 bytes is fast DC table
         p->huffdc[i] = (int *)&p->ucHuffACDCBuf[i*0x180 + 0x80]; // next 256 bytes is slow DC table
         p->huffacFast[i] = (int *)&p->ucHuffACDCBuf[4*0x180 + i*0x1000]; // first 0x800 bytes is fast AC table
         p->huffac[i] = (int *)&p->ucHuffACDCBuf[4*0x180 + i*0x1000 + 0x800]; // next 0x800 bytes is slow AC table
         }
      JPEGInitTables(p); // set up clipping tables for JPEG pixel conversion
      }
   return p;
   
} /* PILPrepJPEGStruct() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILStoreCode(PIL_CODE *, ulong, int)                       *
 *                                                                          *
 *  PURPOSE    : Store a Huffman code into an output data stream.           *
 *                                                                          *
 ****************************************************************************/
static void PILStoreCode(PIL_CODE *pPC, unsigned long ulCode, int iLen)
{

   if (iLen)
      {
      pPC->iLen += iLen;
      pPC->ulAcc |= (ulCode << (32-pPC->iLen));
      while (pPC->iLen >= 8) // need to write to memory
         {
         unsigned char c = (unsigned char)(pPC->ulAcc >> 24);
         *pPC->pOut++ = c;
         if (c == 0xff) // insert a padded 0 to not mistake it for a marker (FFXX)
            *pPC->pOut++ = 0;
         pPC->ulAcc <<= 8;
         pPC->iLen -= 8;
         }
      }
} /* PILStoreCode() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILFlushCode(PIL_CODE *)                                   *
 *                                                                          *
 *  PURPOSE    : Flush any remaining data from the code accumulator.        *
 *                                                                          *
 ****************************************************************************/
static void PILFlushCode(PIL_CODE *pPC)
{
   while (pPC->iLen > 0)
      {
      *pPC->pOut++ = (unsigned char)(pPC->ulAcc >> 24);
      pPC->ulAcc <<= 8;
      pPC->iLen -= 8;
      }
} /* PILFlushCode() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGEncodeMCU()                                            *
 *                                                                          *
 *  PURPOSE    : Use the Huffman table to encode the block as JPEG data     *
 *                                                                          *
 ****************************************************************************/
static int JPEGEncodeMCU(JPEGDATA *pJPEG, signed short *pMCUData, PIL_CODE *pPC, int iDCPred)
{
//int iOff, iBitnum; // faster access
unsigned char cMagnitude;
unsigned char ucCode;
int i, iZeroCount, iDelta, iLen;
unsigned short *pHuff;
unsigned long ulCode;

   // compress the DC component
   iDelta = pMCUData[0] - iDCPred;
   iDCPred = pMCUData[0]; // this is the new DC value
   pHuff = (unsigned short *)pJPEG->pHuffDC;
   if (iDelta < 0)
      {
      iDelta = 0 - iDelta;
      if (iDelta < 128)
         cMagnitude = cMagnitudes[iDelta];
      else
         cMagnitude = 7 + cMagnitudes[iDelta>>7];
      iDelta = iBitMasks[cMagnitude] - iDelta;
      }
   else
      {
      if (iDelta < 128)
         cMagnitude = cMagnitudes[iDelta];
      else
         cMagnitude = 7 + cMagnitudes[iDelta>>7];
      }
   ulCode = (unsigned long)pHuff[cMagnitude];
   iLen = pHuff[cMagnitude + 512];
   ulCode = (ulCode << cMagnitude) | iDelta; // code in msb, followed by delta
   iLen += cMagnitude; // add lengths together
   PILStoreCode(pPC, ulCode, iLen);
   // Encode the AC components
   i = 1;
   while (i<64)
      {
      // count the number of leading zeros
      iZeroCount = 0;
      while (i < 64 && pMCUData[cZigZag2[i]] == 0)
         {
         i++;
         iZeroCount++;
         }
      if (i == 64) // special case, no more coefficients
         { // encode EOB (end of block)
         ulCode = (unsigned long)pHuff[1024];
         iLen = pHuff[1536];
         PILStoreCode(pPC, ulCode, iLen);
         goto encodemcuz;
         }
      else // Encode a zero count and AC coefficient
         {
         while (iZeroCount >= 16)  // maximum that can be encoded at once
            { // 16 zeros is called ZRL (f0)
            ulCode = (unsigned long)pHuff[1024+0xf0];
            iLen = pHuff[1536+0xf0];
            PILStoreCode(pPC, ulCode, iLen);
            iZeroCount -= 16;
            }
         // Encode a normal RRRR/SSSS pair
         iDelta = pMCUData[cZigZag2[i]];
         if (iDelta < 0)
            {
            iDelta = 0 - iDelta;
            if (iDelta < 128)
               cMagnitude = cMagnitudes[iDelta];
            else
               cMagnitude = 7 + cMagnitudes[iDelta>>7];
            iDelta = iBitMasks[cMagnitude] - iDelta;
            }
         else
            {
            if (iDelta < 128)
               cMagnitude = cMagnitudes[iDelta];
            else
               cMagnitude = 7 + cMagnitudes[iDelta>>7];
            }
         ucCode = (unsigned char)((iZeroCount << 4) | cMagnitude); // combine zero count and 'extra' size
         // store the huffman code
         ulCode = (unsigned long)pHuff[1024+ucCode];
         iLen = pHuff[1536+ucCode];
         ulCode = (ulCode << cMagnitude) | iDelta; // code followed by magnitude
         iLen += cMagnitude;
         PILStoreCode(pPC, ulCode, iLen);
         i++; // skip to next coefficient
         }
      }

encodemcuz:
   return iDCPred;

} /* JPEGEncodeMCU() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGGetMCU()                                               *
 *                                                                          *
 *  PURPOSE    : Get a 8x8 block of image data read for compression         *
 *                                                                          *
 ****************************************************************************/
static void JPEGGetMCU(unsigned char *pImage, PIL_PAGE *pPage, int lsize, int x, int y, signed short *pMCUData)
{
int iDelta, width, height, cx, cy;
signed short *pDest;
unsigned char *pSrc;

    if (pPage->cFlags & PIL_PAGEFLAGS_BOTTOMUP) // upside-down
       {
       pImage = pImage + (lsize * (pPage->iHeight-1));
       lsize = -lsize;
       }
    pSrc = pImage + x*8 + (y * 8 * lsize);
    pDest = pMCUData;
    width = height = 8;
    iDelta = 0;
    if (x*8 + width > pPage->iWidth)
       width = 4;
    if (y*8 + height > pPage->iHeight)
       height = pPage->iHeight & 7;
    if (width != 8 || height != 8)
       memset(pMCUData, 0, 64*sizeof(short)); // make sure untouched pixels are zero
    iDelta = 8 - width; // amount to skip in dest if at right edge
    for (cy=0; cy<height; cy++)
       {
       for (cx=0; cx<width; cx++)
          {
          *pDest++ = (signed short)((*pSrc++) - 128);
          }
       pDest += iDelta;
       pSrc += lsize - width; // skip to next line
       }
     
} /* JPEGGetMCU() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGSubSample24()                                          *
 *                                                                          *
 *  PURPOSE    : Subsample a 8x8 color block by 2:1                         *
 *                                                                          *
 ****************************************************************************/
static void JPEGSubSample24(unsigned char *pSrc, signed short *pLUM, signed short *pCb, signed short *pCr, int lsize, int cx, int cy)
{
int x, y;
unsigned char cRed, cGreen, cBlue;
int iY1, iY2, iY3, iY4, iCr1, iCr2, iCr3, iCr4, iCb1, iCb2, iCb3, iCb4;

   cx = (cx + 1)>>1; // do pixels in 2x2 blocks
   cy = (cy + 1)>>1;

   for (y=0; y<cy; y++)
      {
      for (x=0; x<cx; x++) // do 8x8 pixels in 2x2 blocks
         {
         cBlue = pSrc[0];
         cGreen = pSrc[1];
         cRed = pSrc[2];
         iY1 = (((cRed * 1225) + (cGreen * 2404) + (cBlue * 467)) >> 12) - 0x80;
         iCb1 = (cBlue << 11) + (cRed * -691) + (cGreen * -1357);
         iCr1 = (cRed << 11) + (cGreen * -1715) + (cBlue * -333);

         cBlue = pSrc[3];
         cGreen = pSrc[4];
         cRed = pSrc[5];
         iY2 = (((cRed * 1225) + (cGreen * 2404) + (cBlue * 467)) >> 12) - 0x80;
         iCb2 = (cBlue << 11) + (cRed * -691) + (cGreen * -1357);
         iCr2 = (cRed << 11) + (cGreen * -1715) + (cBlue * -333);

         cBlue = pSrc[lsize];
         cGreen = pSrc[lsize+1];
         cRed = pSrc[lsize+2];
         iY3 = (((cRed * 1225) + (cGreen * 2404) + (cBlue * 467)) >> 12) - 0x80;
         iCb3 = (cBlue << 11) + (cRed * -691) + (cGreen * -1357);
         iCr3 = (cRed << 11) + (cGreen * -1715) + (cBlue * -333);

         cBlue = pSrc[lsize+3];
         cGreen = pSrc[lsize+4];
         cRed = pSrc[lsize+5];
         iY4 = (((cRed * 1225) + (cGreen * 2404) + (cBlue * 467)) >> 12) - 0x80;
         iCb4 = (cBlue << 11) + (cRed * -691) + (cGreen * -1357);
         iCr4 = (cRed << 11) + (cGreen * -1715) + (cBlue * -333);

         // Average the chroma values together
         iCr1 = (iCr1 + iCr2 + iCr3 + iCr4) >> 14;
         iCb1 = (iCb1 + iCb2 + iCb3 + iCb4) >> 14;
 
         // store in the MCUs
         pLUM[0] = (signed short)iY1;
         pLUM[1] = (signed short)iY2;
         pLUM[8] = (signed short)iY3;
         pLUM[9] = (signed short)iY4;
         pLUM += 2;
         pCr[0] = (signed short)iCr1;
         pCb[0] = (signed short)iCb1;
         pCr++;
         pCb++;
         pSrc += 6; // skip 2 pixels to right
         } // for x
      pCr += 8 - cx; // skip to next row;
      pCb += 8 - cx;
      pLUM += 8 + (4-cx)*2; // skip down a row since 2 at a time
      pSrc += lsize*2 - cx*6; // skip 2 lines
      } // for y

} /* JPEGSubSample24() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGSubSample16()                                          *
 *                                                                          *
 *  PURPOSE    : Subsample a 8x8 color block by 2:1                         *
 *                                                                          *
 ****************************************************************************/
static void JPEGSubSample16(unsigned char *pSrc, signed short *pLUM, signed short *pCb, signed short *pCr, int lsize, int cx, int cy)
{
int x, y;
unsigned short us;
unsigned short *pUS = (unsigned short *)pSrc;
unsigned char cRed, cGreen, cBlue;
int iY1, iY2, iY3, iY4, iCr1, iCr2, iCr3, iCr4, iCb1, iCb2, iCb3, iCb4;

   cx = (cx + 1)>>1; // do pixels in 2x2 blocks
   cy = (cy + 1)>>1;

   for (y=0; y<cy; y++)
      {
      for (x=0; x<cx; x++) // do 8x8 pixels in 2x2 blocks
         {
         us = pUS[0];
         cBlue = (unsigned char)(((us & 0x1f)<<3) | (us & 7));
         cGreen = (unsigned char)(((us & 0x7e0)>>3) | ((us & 0x60)>>5));
         cRed = (unsigned char)(((us & 0xf800)>>8) | ((us & 0x3800)>>11));
         iY1 = (((cRed * 1225) + (cGreen * 2404) + (cBlue * 467)) >> 12) - 0x80;
         iCb1 = (cBlue << 11) + (cRed * -691) + (cGreen * -1357);
         iCr1 = (cRed << 11) + (cGreen * -1715) + (cBlue * -333);

         us = pUS[1];
         cBlue = (unsigned char)(((us & 0x1f)<<3) | (us & 7));
         cGreen = (unsigned char)(((us & 0x7e0)>>3) | ((us & 0x60)>>5));
         cRed = (unsigned char)(((us & 0xf800)>>8) | ((us & 0x3800)>>11));
         iY2 = (((cRed * 1225) + (cGreen * 2404) + (cBlue * 467)) >> 12) - 0x80;
         iCb2 = (cBlue << 11) + (cRed * -691) + (cGreen * -1357);
         iCr2 = (cRed << 11) + (cGreen * -1715) + (cBlue * -333);

         us = pUS[lsize>>1];
         cBlue = (unsigned char)(((us & 0x1f)<<3) | (us & 7));
         cGreen = (unsigned char)(((us & 0x7e0)>>3) | ((us & 0x60)>>5));
         cRed = (unsigned char)(((us & 0xf800)>>8) | ((us & 0x3800)>>11));
         iY3 = (((cRed * 1225) + (cGreen * 2404) + (cBlue * 467)) >> 12) - 0x80;
         iCb3 = (cBlue << 11) + (cRed * -691) + (cGreen * -1357);
         iCr3 = (cRed << 11) + (cGreen * -1715) + (cBlue * -333);

         us = pUS[(lsize>>1)+1];
         cBlue = (unsigned char)(((us & 0x1f)<<3) | (us & 7));
         cGreen = (unsigned char)(((us & 0x7e0)>>3) | ((us & 0x60)>>5));
         cRed = (unsigned char)(((us & 0xf800)>>8) | ((us & 0x3800)>>11));
         iY4 = (((cRed * 1225) + (cGreen * 2404) + (cBlue * 467)) >> 12) - 0x80;
         iCb4 = (cBlue << 11) + (cRed * -691) + (cGreen * -1357);
         iCr4 = (cRed << 11) + (cGreen * -1715) + (cBlue * -333);

         // Average the chroma values together
         iCr1 = (iCr1 + iCr2 + iCr3 + iCr4) >> 14;
         iCb1 = (iCb1 + iCb2 + iCb3 + iCb4) >> 14;
 
         // store in the MCUs
         pLUM[0] = (signed short)iY1;
         pLUM[1] = (signed short)iY2;
         pLUM[8] = (signed short)iY3;
         pLUM[9] = (signed short)iY4;
         pLUM += 2;
         pCr[0] = (signed short)iCr1;
         pCb[0] = (signed short)iCb1;
         pCr++;
         pCb++;
         pUS += 2; // skip 2 pixels to right
         } // for x
      pCr += 8 - cx; // skip to next row;
      pCb += 8 - cx;
      pLUM += 8 + (4-cx)*2; // skip down a row since 2 at a time
      pUS += lsize - cx*2; // skip 2 lines
      } // for y

} /* JPEGSubSample16() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGGetMCU22()                                             *
 *                                                                          *
 *  PURPOSE    : Get a 16x16 block of color image data read for compression *
 *                                                                          *
 ****************************************************************************/
static void JPEGGetMCU22(unsigned char *pImage, PIL_PAGE *pPage, int lsize, int x, int y, signed short *pMCUData)
{
int cx, cy, width, height;
unsigned char *pSrc=NULL;

    if (pPage->cFlags & PIL_PAGEFLAGS_BOTTOMUP)
       {
       pImage += lsize * (pPage->iHeight-1);
       lsize = -lsize;
       }
    if (pPage->cBitsperpixel == 16)
       pSrc = pImage + x*16*2 + (y * 16 * lsize);
    else if (pPage->cBitsperpixel == 24)
       pSrc = pImage + x*16*3 + (y * 16 * lsize);
    else if (pPage->cBitsperpixel == 17)
       pSrc = pImage + x*8*4 + (y * 16 * lsize);
    width = height = 16;
    if (x*16 + width > pPage->iWidth)
       width = pPage->iWidth & 15;
    if (y*16 + height > pPage->iHeight)
       height = pPage->iHeight & 15;
    if (width < 8)
       cx = width;
    else
       cx = 8;
    if (height < 8)
       cy = height;
    else
       cy = 8;
    if (cy != 8 || cx != 8) // for edge MCUs, make sure all unused slots are 0
      memset(pMCUData, 0, 6*64*sizeof(short));
    if (pPage->cBitsperpixel == 16)
       {
       // upper left
       JPEGSubSample16(pSrc, pMCUData, &pMCUData[DCTSIZE2*4], &pMCUData[DCTSIZE2*5], lsize, cx, cy);
       // upper right
       if (width > 8)
          JPEGSubSample16(pSrc+8*2, &pMCUData[DCTSIZE2*1], &pMCUData[4+DCTSIZE2*4], &pMCUData[4+DCTSIZE2*5], lsize, width-8, cy);
       if (height > 8)
          {
          // lower left
          JPEGSubSample16(pSrc+8*lsize, &pMCUData[DCTSIZE2*2], &pMCUData[32+DCTSIZE2*4], &pMCUData[32+DCTSIZE2*5], lsize, cx, height - 8);
          // lower right
          if (width > 8)
             JPEGSubSample16(pSrc+8*lsize + 8*2, &pMCUData[DCTSIZE2*3], &pMCUData[36+DCTSIZE2*4], &pMCUData[36+DCTSIZE2*5], lsize, width - 8, height - 8);
          }
       }
    else if (pPage->cBitsperpixel == 24)
       {
       // upper left
       JPEGSubSample24(pSrc, pMCUData, &pMCUData[DCTSIZE2*4], &pMCUData[DCTSIZE2*5], lsize, cx, cy);
       // upper right
       if (width > 8)
          JPEGSubSample24(pSrc+8*3, &pMCUData[DCTSIZE2*1], &pMCUData[4+DCTSIZE2*4], &pMCUData[4+DCTSIZE2*5], lsize, width-8, cy);
       if (height > 8)
          {
          // lower left
          JPEGSubSample24(pSrc+8*lsize, &pMCUData[DCTSIZE2*2], &pMCUData[32+DCTSIZE2*4], &pMCUData[32+DCTSIZE2*5], lsize, cx, height - 8);
          // lower right
          if (width > 8)
             JPEGSubSample24(pSrc+8*lsize + 8*3, &pMCUData[DCTSIZE2*3], &pMCUData[36+DCTSIZE2*4], &pMCUData[36+DCTSIZE2*5], lsize, width - 8, height - 8);
          }
       }     
} /* JPEGGetMCU22() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGSample16()                                             *
 *                                                                          *
 *  PURPOSE    : Sample a 8x8 color block                                   *
 *                                                                          *
 ****************************************************************************/
static void JPEGSample16(unsigned char *pSrc, signed short *pMCU, int lsize, int cx, int cy)
{
int x, y;
unsigned short us;
unsigned short *pUS = (unsigned short *)pSrc;
unsigned char cRed, cGreen, cBlue;
int iY, iCr, iCb;

   for (y=0; y<cy; y++)
      {
      for (x=0; x<cx; x++) // do 8x8 pixels
         {
         us = *pUS++;
         cBlue = (unsigned char)(((us & 0x1f)<<3) | (us & 7));
         cGreen = (unsigned char)(((us & 0x7e0)>>3) | ((us & 0x60)>>5));
         cRed = (unsigned char)(((us & 0xf800)>>8) | ((us & 0x3800)>>11));
         iY = (((cRed * 1225) + (cGreen * 2404) + (cBlue * 467)) >> 12) - 0x80;
         iCb = (cBlue << 11) + (cRed * -691) + (cGreen * -1357);
         iCr = (cRed << 11) + (cGreen * -1715) + (cBlue * -333);

         // store in the MCUs
         pMCU[64]  = (signed short)(iCb >> 12);
         pMCU[128]  = (signed short)(iCr >> 12);
         *pMCU++ = (signed short)iY;
         } // for x
      pMCU += 8 - cx;
      pSrc += (lsize>>1) - cx;
      } // for y

} /* JPEGSample16() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGSample24()                                             *
 *                                                                          *
 *  PURPOSE    : Sample a 8x8 color block                                   *
 *                                                                          *
 ****************************************************************************/
static void JPEGSample24(unsigned char *pSrc, signed short *pMCU, int lsize, int cx, int cy)
{
int x, y;
unsigned char cRed, cGreen, cBlue;
int iY, iCr, iCb;

   for (y=0; y<cy; y++)
      {
      for (x=0; x<cx; x++) // do 8x8 pixels
         {
         cBlue = *pSrc++;
         cGreen = *pSrc++;
         cRed = *pSrc++;
         iY = (((cRed * 1225) + (cGreen * 2404) + (cBlue * 467)) >> 12) - 0x80;
         iCb = (cBlue << 11) + (cRed * -691) + (cGreen * -1357);
         iCr = (cRed << 11) + (cGreen * -1715) + (cBlue * -333);

         // store in the MCUs
         pMCU[64]  = (signed short)(iCb >> 12);
         pMCU[128]  = (signed short)(iCr >> 12);
         *pMCU++ = (signed short)iY;
         } // for x
      pMCU += 8 - cx;
      pSrc += lsize - cx*3;
      } // for y

} /* JPEGSample24() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGGetMCU11()                                             *
 *                                                                          *
 *  PURPOSE    : Get a 8x8 block of color image data read for compression   *
 *                                                                          *
 ****************************************************************************/
static void JPEGGetMCU11(unsigned char *pImage, PIL_PAGE *pPage, int lsize, int x, int y, signed short *pMCUData)
{
int cx, cy;
unsigned char *pSrc;

    if (pPage->cFlags & PIL_PAGEFLAGS_BOTTOMUP)
       {
       pImage += lsize * (pPage->iHeight-1);
       lsize = -lsize;
       }
    if (pPage->cBitsperpixel == 24)
       pSrc = pImage + x*8*3 + (y * 8 * lsize);
    else
       pSrc = pImage + x*8*2 + (y * 8 * lsize);
    if (x*8 + 8 > pPage->iWidth)
       cx = pPage->iWidth & 7;
    else
       cx = 8;
    if (y*8 + 8 > pPage->iHeight)
       cy = pPage->iHeight & 7;
    else
       cy = 8;
    if (cy != 8 || cx != 8)
       memset(pMCUData, 0, 3*64*sizeof(short)); // make sure unused pixels are 0
    if (pPage->cBitsperpixel == 24)
       JPEGSample24(pSrc, pMCUData, lsize, cx, cy);
    else
       JPEGSample16(pSrc, pMCUData, lsize, cx, cy);
     
} /* JPEGGetMCU11() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGMakeHuffE(JPEGDATA *)                                  *
 *                                                                          *
 *  PURPOSE    : Create the Huffman tables for encoding an MCU.             *
 *                                                                          *
 ****************************************************************************/
static void JPEGMakeHuffE(JPEGDATA *pJPEG)
{
int code, iLen, iTable;
unsigned short *pTable;
int iBitNum; // current code bit length
int n_bits; // number of bits to do
int cc; // code
unsigned char *p, *pBits;
int iTableCount;

   if (pJPEG->ucNumComponents == 1)
      iTableCount = 1;
   else
      iTableCount = 2;

// first do DC components (up to 12-bit codes)
   for (iTable = 0; iTable < iTableCount; iTable++)
      {
      pJPEG->huffdc[iTable] = (int *)&pJPEG->ucHuffACDCBuf[iTable*0x1000]; // each table gets 4K
      pTable = (unsigned short *)pJPEG->huffdc[iTable];
      if (iTable == 0)
         pBits = huffl_dc;
      else
         pBits = huffcr_dc;
      p = (unsigned char *)pBits;
      p += 16; // point to bit data
      iBitNum = 1;
      cc = 0; // start with a code of 0
      for (n_bits = 0; n_bits < 16; n_bits++)
         {
         iLen = *pBits++; // get number of codes for this bit length
         while (iLen)
            {
            code = *p++;  // get actual huffman code
            pTable[code] = (unsigned short)cc;
            pTable[code+512] = (unsigned short)iBitNum; // store the length here
            cc++;
            iLen--;
            }
         iBitNum++;
         cc <<= 1;
         }
      }
// now do AC components (up to 16-bit codes)
   for (iTable = 0; iTable < iTableCount; iTable++)
      {
      pTable = (unsigned short *)pJPEG->huffdc[iTable];
      if (iTable == 0)
         pBits = huffl_ac;
      else
         pBits = huffcr_ac;
      p = (unsigned char *)pBits;
      p += 16; // point to bit data
      iBitNum = 1;
      cc = 0; // start with a code of 0
      for (n_bits = 0; n_bits < 16; n_bits++)
         {
         iLen = *pBits++; // get number of codes for this bit length
         while (iLen)
            {
            code = *p++;  // get actual huffman code
            pTable[1024+code] = (unsigned short)cc;
            pTable[1536+code] = (unsigned short)iBitNum;
            cc++;
            iLen--;
            }
         iBitNum++;
         cc <<= 1;
         }
      }
} /* JPEGMakeHuffE() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGQuantize(JPEGDATA, int *)                              *
 *                                                                          *
 *  PURPOSE    : Quantize and re-order the MCU data.                        *
 *                                                                          *
 ****************************************************************************/
static void JPEGQuantize(JPEGDATA *pJPEG, signed short *pMCUSrc, int iTable)
{
int i;
signed short d, sQ1, sQ2;
signed short *pQuant;
//unsigned char *pZigZag = cZigZag;

   pQuant = (signed short *)&pJPEG->sQuantTable[iTable * 64];
   for (i=0; i<64; i++)
      {
      sQ1 = *pQuant++;
      sQ2 = sQ1 >> 1;
      d = *pMCUSrc;
      if (d < 0)
         {
         *pMCUSrc++ = 0 - ((sQ2 - d)/sQ1);
         }
      else
         {
         *pMCUSrc++ = (sQ2 + d)/sQ1;
         }
      } // for

} /* JPEGQuantize() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : MakeJPEG8()                                                *
 *                                                                          *
 *  PURPOSE    : Encode an image as grayscale JPEG.                         *
 *                                                                          *
 ****************************************************************************/
static void MakeJPEG8(PIL_PAGE *pPage, JPEGDATA *pJPEG, int *iOffset, unsigned char *pBuf, int iQFactor)
{
unsigned char *pImage;
signed short *pMCUData;
int i, x, y, cx, cy, iDCPred;
int lsize;
PIL_CODE pc;

   pc.iLen = 0;
   pc.ulAcc = 0;
   pc.pOut = &pBuf[*iOffset];
   
   pImage = pPage->pData;
   lsize = mPILCalcSize(pPage->iWidth, pPage->cBitsperpixel);
   pMCUData = pJPEG->pMCUs;
   cx = (pPage->iWidth + 7)>>3; // number of MCU blocks
   cy = (pPage->iHeight + 7)>>3;
   // prepare the quantization table
   for (i=0; i<64; i++)
      {
      switch (iQFactor)
         {
         case 0:
            pJPEG->sQuantTable[i] = (quant_lum[i] >> 2);
            break;
         case 1:
            pJPEG->sQuantTable[i] = (quant_lum[i] >> 1);
            break;
         case 2:
            pJPEG->sQuantTable[i] = quant_lum[i];
            break;
         case 3:
            pJPEG->sQuantTable[i] = (quant_lum[i] << 1);
            break;
         }
      }
   JPEGFixQuantE(pJPEG); // reorder and scale quant table(s)
   JPEGMakeHuffE(pJPEG); // create the Huffman tables to encode
   pJPEG->pHuffDC = pJPEG->huffdc[0];
   iDCPred = 0;
   for (y=0; y<cy; y++)
      {
      for (x=0; x<cx; x++)
         {
         JPEGGetMCU(pImage, pPage, lsize, x, y, pMCUData);
         JPEGFDCT(pJPEG, pMCUData);
         JPEGQuantize(pJPEG, pMCUData, 0);
         iDCPred = JPEGEncodeMCU(pJPEG, pMCUData, &pc, iDCPred);
         }
      }
   PILFlushCode(&pc);
   *iOffset = (int)(pc.pOut - pBuf);

} /* MakeJPEG8() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : MakeJPEG24()                                               *
 *                                                                          *
 *  PURPOSE    : Encode an image as RGB color JPEG.                         *
 *                                                                          *
 ****************************************************************************/
static void MakeJPEG24(PIL_PAGE *pPage, JPEGDATA *pJPEG, int *iOffset, unsigned char *pBuf, int iQFactor)
{
unsigned char *pImage;
signed short *pMCUData;
int i, x, y, cx, cy, iDCPred0, iDCPred1, iDCPred2;
int lsize;
PIL_CODE pc;

// init code stream structure
   pc.pOut = &pBuf[*iOffset];
   pc.ulAcc = 0;
   pc.iLen = 0;
   
   pImage = pPage->pData;
   lsize = mPILCalcSize(pPage->iWidth, pPage->cBitsperpixel);
   pMCUData = pJPEG->pMCUs;
   // prepare the luma & chroma quantization tables
   for (i=0; i<64; i++)
      {
      switch (iQFactor & 3)
         {
         case 0:
            pJPEG->sQuantTable[i] = (quant_lum[i] >> 2);
            pJPEG->sQuantTable[i+64] = (quant_color[i] >> 2);
            break;
         case 1:
            pJPEG->sQuantTable[i] = (quant_lum[i] >> 1);
            pJPEG->sQuantTable[i+64] = (quant_color[i] >> 1);
            break;
         case 2:
            pJPEG->sQuantTable[i] = quant_lum[i];
            pJPEG->sQuantTable[i+64] = quant_color[i];
            break;
         case 3:
            pJPEG->sQuantTable[i] = (quant_lum[i] << 1);
            pJPEG->sQuantTable[i+64] = (quant_color[i] << 1);
            break;
         }
      }
   JPEGFixQuantE(pJPEG); // reorder and scale quant table(s)
   JPEGMakeHuffE(pJPEG); // create the Huffman tables to encode
   iDCPred0 = iDCPred1 = iDCPred2 = 0;
   if (iQFactor & PIL_CONVERT_QUALITY_SUBSAMPLE)
      {
      cx = (pPage->iWidth + 15)>>4; // number of MCU blocks
      cy = (pPage->iHeight + 15)>>4;
      for (y=0; y<cy; y++)
         {
         for (x=0; x<cx; x++)
            {
            JPEGGetMCU22(pImage, pPage, lsize, x, y, pMCUData);
            /*--- First, encode the 4 luma blocks ---*/
            pJPEG->pHuffDC = pJPEG->huffdc[0];
            JPEGFDCT(pJPEG, pMCUData);
            JPEGQuantize(pJPEG, pMCUData, 0);
            iDCPred0 = JPEGEncodeMCU(pJPEG, pMCUData, &pc, iDCPred0);

            JPEGFDCT(pJPEG, &pMCUData[1*DCTSIZE2]);
            JPEGQuantize(pJPEG, &pMCUData[1*DCTSIZE2], 0);
            iDCPred0 = JPEGEncodeMCU(pJPEG, &pMCUData[1*DCTSIZE2], &pc, iDCPred0);

            JPEGFDCT(pJPEG, &pMCUData[2*DCTSIZE2]);
            JPEGQuantize(pJPEG, &pMCUData[2*DCTSIZE2], 0);
            iDCPred0 = JPEGEncodeMCU(pJPEG, &pMCUData[2*DCTSIZE2], &pc, iDCPred0);

            JPEGFDCT(pJPEG, &pMCUData[3*DCTSIZE2]);
            JPEGQuantize(pJPEG, &pMCUData[3*DCTSIZE2], 0);
            iDCPred0 = JPEGEncodeMCU(pJPEG, &pMCUData[3*DCTSIZE2], &pc, iDCPred0);

            /*--- Now do the chroma blocks ---*/
            pJPEG->pHuffDC = pJPEG->huffdc[1];
            JPEGFDCT(pJPEG, &pMCUData[4*DCTSIZE2]);
            JPEGQuantize(pJPEG, &pMCUData[4*DCTSIZE2], 1);
            iDCPred1 = JPEGEncodeMCU(pJPEG, &pMCUData[4*DCTSIZE2], &pc, iDCPred1);

            JPEGFDCT(pJPEG, &pMCUData[5*DCTSIZE2]);
            JPEGQuantize(pJPEG, &pMCUData[5*DCTSIZE2], 1);
            iDCPred2 = JPEGEncodeMCU(pJPEG, &pMCUData[5*DCTSIZE2], &pc, iDCPred2);
            }
         }
      }
   else
      { // interleaved, but no subsampling
      cx = (pPage->iWidth + 7)>>3; // number of MCU blocks
      cy = (pPage->iHeight + 7)>>3;
      for (y=0; y<cy; y++)
         {
         for (x=0; x<cx; x++)
            {
            JPEGGetMCU11(pImage, pPage, lsize, x, y, pMCUData);
            /*--- First, encode the luma block ---*/
            pJPEG->pHuffDC = pJPEG->huffdc[0];
            JPEGFDCT(pJPEG, pMCUData);
            JPEGQuantize(pJPEG, pMCUData, 0);
            iDCPred0 = JPEGEncodeMCU(pJPEG, pMCUData, &pc, iDCPred0);

            /*--- Now do the chroma blocks ---*/
            pJPEG->pHuffDC = pJPEG->huffdc[1];
            JPEGFDCT(pJPEG, &pMCUData[1*DCTSIZE2]);
            JPEGQuantize(pJPEG, &pMCUData[1*DCTSIZE2], 1);
            iDCPred1 = JPEGEncodeMCU(pJPEG, &pMCUData[1*DCTSIZE2], &pc, iDCPred1);

            JPEGFDCT(pJPEG, &pMCUData[2*DCTSIZE2]);
            JPEGQuantize(pJPEG, &pMCUData[2*DCTSIZE2], 1);
            iDCPred2 = JPEGEncodeMCU(pJPEG, &pMCUData[2*DCTSIZE2], &pc, iDCPred2);
            }
         }
      }
   PILFlushCode(&pc);
   *iOffset = (int)(pc.pOut - pBuf);

} /* MakeJPEG24() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILMakeJPEG(PIL_PAGE *, PIL_PAGE *, int)                   *
 *                                                                          *
 *  PURPOSE    : Convert an image into JPEG compressed data (JFIF format).  *
 *                                                                          *
 ****************************************************************************/
static int PILMakeJPEG(PIL_PAGE *pPage, PIL_PAGE *pOutPage, int iQFactor)
{
int i = 0, iOffset, iErr;
JPEGDATA *pJPEG;
unsigned char c, *pBuf;

   iErr = 0;
   if (iQFactor & PIL_CONVERT_NOALLOC)
      pBuf = pOutPage->pData;
   else
      {
      pOutPage->pData = pBuf = MiniIOAlloc(pPage->iDataSize / 3); // min compression is usually 8:1
#ifdef _WIN32
      i = GetLastError();
#endif
      if (!pBuf) /* Allocate the output buffer structure */
         return PIL_ERROR_MEMORY;
      }
   if (iQFactor & PIL_CONVERT_NOALLOC)
      pJPEG =  pOutPage->pJPEG;
   else
      pJPEG = MiniIOAlloc(sizeof(JPEGDATA));
   iOffset = 0;

   if (pPage->cBitsperpixel != 8 && pPage->cBitsperpixel != 24 && pPage->cBitsperpixel != 16 && pPage->cBitsperpixel != 17 && pPage->cBitsperpixel != 18)
      {
      iErr = PIL_ERROR_INVPARAM;
      goto makejpegz;
      }
   if (pPage->cBitsperpixel == 8)
      pJPEG->ucNumComponents = 1;
   else
      pJPEG->ucNumComponents = 3;
   WRITEMOTO32(pBuf, iOffset, 0xffd8ffe0); // write app0 marker
   iOffset += 4;
   if (pOutPage->cCompression == PIL_COMP_JPEG)
      {
      WRITEMOTO32(pBuf, iOffset, 0x00104a46); // JFIF
      iOffset += 4;
      WRITEMOTO32(pBuf, iOffset, 0x49460001);
      iOffset += 4;
      }
   else // motion jpeg uses another identifier
      {
      {
      WRITEMOTO32(pBuf, iOffset, 0x00104156); // AVI1
      iOffset += 4;
      WRITEMOTO32(pBuf, iOffset, 0x49310001);
      iOffset += 4;
      }
      }
   WRITEMOTO16(pBuf, iOffset, 0x0101); // resolution units = dots per inche
   iOffset += 2;
   if (pPage->iXres > 0xffff || pPage->iXres < 0) // invalid value
      pPage->iXres = 0;
   if (pPage->iYres > 0xffff || pPage->iYres < 0) // invalid value
      pPage->iYres = 0;
   WRITEMOTO16(pBuf, iOffset, pPage->iXres); // store spacial resolution
   iOffset += 2;
   WRITEMOTO16(pBuf, iOffset, pPage->iYres);
   iOffset += 2;
   WRITEMOTO16(pBuf, iOffset, 0); // add 2 zeros
   iOffset += 2;
   // define quantization tables
   WRITEMOTO16(pBuf, iOffset, 0xffdb); // huff table marker
   iOffset += 2;
   WRITEMOTO16(pBuf, iOffset, 0x0043); // table size
   iOffset += 2;
   pBuf[iOffset++] = 0; // table type and number 0,8 bit
   for (i=0; i<64; i++)
      {
      c = quant_lum[i];
      switch (iQFactor & 3) // adjust table depending on quality factor
         {
         default:
         case 0: // best quality, divide by 4
            pBuf[iOffset++] = c >> 2;
            break;
         case 1: // high quality, divide by 2
            pBuf[iOffset++] = c >> 1;
            break;
         case 2: // medium quality factor, use values unchanged
            pBuf[iOffset++] = c;
            break;
         case 3: // low quality, use values * 2
            pBuf[iOffset++] = c << 1;
            break;
         }
      }
   if (pPage->cBitsperpixel > 8) // add color quant tables
      {
      WRITEMOTO16(pBuf, iOffset, 0xffdb); // huff table
      iOffset += 2;
      WRITEMOTO16(pBuf, iOffset, 0x0043); // table size
      iOffset += 2;
      pBuf[iOffset++] = 1;  // table 1, 8 bit
      for (i=0; i<64; i++)
         {
         c = quant_color[i];
         switch (iQFactor & 3) // adjust table depending on quality factor
            {
            case 0: // best quality, divide by 4
               pBuf[iOffset++] = c >> 2;
               break;
            case 1: // high quality, divide by 2
               pBuf[iOffset++] = c >> 1;
               break;
            case 2: // medium quality factor, use values unchanged
               pBuf[iOffset++] = c;
               break;
            case 3: // low quality, use values * 2
               pBuf[iOffset++] = c << 1;
               break;
            }
         }
      }
   // store the frame header
   WRITEMOTO16(pBuf, iOffset, 0xffc0); // SOF0 marker
   iOffset += 2;
   if (pPage->cBitsperpixel == 8)
      {
      pBuf[iOffset++] = 0;
      pBuf[iOffset++] = 11; // length = 11
      pBuf[iOffset++] = 8;   // sample precision
      WRITEMOTO16(pBuf, iOffset, pPage->iHeight); // image height
      iOffset += 2;
      WRITEMOTO16(pBuf, iOffset, pPage->iWidth); // image width
      iOffset += 2;
      pBuf[iOffset++] = 1; // number of components = 1 (grayscale)
      pBuf[iOffset++] = 0; // component number
      WRITEMOTO16(pBuf, iOffset, 0x1100); // subsampling and quant table selector
      iOffset += 2;
      }
   else  // set up color stuff
      {
      pBuf[iOffset++] = 0;
      pBuf[iOffset++] = 17; // length = 17
      pBuf[iOffset++] = 8;   // sample precision
      WRITEMOTO16(pBuf, iOffset, pPage->iHeight); // image height
      iOffset += 2;
      WRITEMOTO16(pBuf, iOffset, pPage->iWidth); // image width
      iOffset += 2;
      pBuf[iOffset++] = 3; // number of components = 3 (Ycc)
      pBuf[iOffset++] = 0; // component number 0 (Y)
      if (iQFactor & PIL_CONVERT_QUALITY_SUBSAMPLE)
         {
         WRITEMOTO16(pBuf, iOffset, 0x2200); // 2:1 subsampling and quant table selector
         }
      else
         {
         WRITEMOTO16(pBuf, iOffset, 0x1100); // no subsampling and quant table selector
         }
      iOffset += 2;
      pBuf[iOffset++] = 1; // component number 1 (Cb)
      WRITEMOTO16(pBuf, iOffset, 0x1101); // subsampling and quant table selector
      iOffset += 2;
      pBuf[iOffset++] = 2; // component number 2 (Cr)
      WRITEMOTO16(pBuf, iOffset, 0x1101); // subsampling and quant table selector
      iOffset += 2;
      }
  // define Huffman tables
   WRITEMOTO16(pBuf, iOffset, 0xffc4); // Huffman DC table
   iOffset += 2;
   WRITEMOTO16(pBuf, iOffset, 0x1f); // Table length = 31
   iOffset += 2;
   pBuf[iOffset++] = 0; // table class = 0 (DC), id = 0
   memcpy(&pBuf[iOffset], huffl_dc, 28); // copy DC table
   iOffset += 28;
// now the AC table
   WRITEMOTO16(pBuf, iOffset, 0xffc4); // Huffman AC table
   iOffset += 2;
   WRITEMOTO16(pBuf, iOffset, 0xb5); // Table length = 181
   iOffset += 2;
   pBuf[iOffset++] = 0x10; // table class = 1 (AC), id = 0
   memcpy(&pBuf[iOffset], huffl_ac, 178); // copy AC table
   iOffset += 178;
   if (pPage->cBitsperpixel > 8) // define a second set of tables for color 
      {
      WRITEMOTO16(pBuf, iOffset, 0xffc4); // Huffman DC table
      iOffset += 2;
      WRITEMOTO16(pBuf, iOffset, 0x1f); // Table length = 31
      iOffset += 2;
      pBuf[iOffset++] = 1; // table class = 0 (DC), id = 1
      memcpy(&pBuf[iOffset], huffcr_dc, 28); // copy DC table
      iOffset += 28;
      // now the AC table
      WRITEMOTO16(pBuf, iOffset, 0xffc4); // Huffman AC table
      iOffset += 2;
      WRITEMOTO16(pBuf, iOffset, 0xb5); // Table length = 181
      iOffset += 2;
      pBuf[iOffset++] = 0x11; // table class = 1 (AC), id = 1
      memcpy(&pBuf[iOffset], huffcr_ac, 178); // copy AC table
      iOffset += 178;
      }
 // Define the start of scan header (SOS)
   WRITEMOTO16(pBuf, iOffset, 0xffda); // SOS
   iOffset += 2;
   if (pPage->cBitsperpixel == 8)
      {
      WRITEMOTO16(pBuf, iOffset, 0x8); // Table length = 8
      iOffset += 2;
      pBuf[iOffset++] = 1;            // number of components in scan = 1 (grayscale)
      pBuf[iOffset++] = 0; // component id = 0
      pBuf[iOffset++] = 0; // dc/ac huffman table = 0/0
      }
   else // color
      {
      WRITEMOTO16(pBuf, iOffset, 0xc); // Table length = 12
      iOffset += 2;
      pBuf[iOffset++] = 3;            // number of components in scan = 3 (color)
      pBuf[iOffset++] = 0; // component id = 0
      pBuf[iOffset++] = 0; // dc/ac huffman table = 0/0
      pBuf[iOffset++] = 1; // component id = 1
      pBuf[iOffset++] = 0x11; // dc/ac huffman table = 1/1
      pBuf[iOffset++] = 2; // component id = 2
      pBuf[iOffset++] = 0x11; // dc/ac huffman table = 1/1
      } 
   pBuf[iOffset++] = 0; // start of spectral selection
   pBuf[iOffset++] = 63; // end of spectral selection
   pBuf[iOffset++] = 0; // successive approximation bit
   //lsize = mPILCalcSize(pPage->iWidth, pPage->cBitsperpixel);
   //iStartData = iOffset; // start of compressed data
   if (pPage->cBitsperpixel == 8)
      MakeJPEG8(pPage, pJPEG, &iOffset, pBuf, iQFactor);
   else
      MakeJPEG24(pPage, pJPEG, &iOffset, pBuf, iQFactor);
makejpegz:
//   iOffset++; // allow for last partial-byte
   if (!(iQFactor & PIL_CONVERT_NOALLOC))
      MiniIOFree(pJPEG);
   if (iErr == 0)
      {
      pBuf[iOffset++] = 0xff;
      pBuf[iOffset++] = 0xd9;  // end of image (EOI)
      pOutPage->iDataSize = iOffset;
      }
   pOutPage->cState = PIL_PAGE_STATE_LOADED;
   // DEBUG Free huffman tables
   return iErr;

} /* PILMakeJPEG() */
#endif // PIL_IMAGE_WRITE

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILCalcSize(int, int)                                      *
 *                                                                          *
 *  PURPOSE    : Calculate the cache-aligned line size in bytes.            *
 *                                                                          *
 ****************************************************************************/
int mPILCalcSize(int x, int bpp)
{
int i;

   switch(bpp)
      {
      case 1:
         i = (x + 31) >> 3;
         i &= 0xfffc;
         break;
      case 2:
         i = (x + 3) >> 2;
         i = (i + 3) & 0xfffc;
         break;
      case 4:
         i = (x + 1) >> 1;
         i = (i+3) & 0xfffc;
         break;
      case 8:
         i = (x + 3) & 0xfffc;
         break;
      case 16:
      case 17:
         i = x * 2;
         i = (i+3) & 0xfffc;
//         i = (i + 31) & 0xffe0;
         break;
      case 24:
         i = x * 3;
         i = (i+3) & 0xfffc;
//         i = (i + 31) & 0xffe0;
         break;
      case 32:
         i = x * 4;
         break;
      default: /* to suppress compiler warning */
         i = 0;
         break;
      }
   return i;

} /* mPILCalcSize() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILCalcBSize(int, int)                                     *
 *                                                                          *
 *  PURPOSE    : Calculate the byte-aligned line size in bytes.             *
 *                                                                          *
 ****************************************************************************/
static int PILCalcBSize(int x, int bpp)
{
int i;

   switch(bpp)
      {
      case 1:
         i = (x + 7) >> 3;
         break;
      case 2:
         i = (x + 3) >> 2;
         break;
      case 4:
         i = (x + 1) >> 1;
         break;
      case 8:
         i = x;
         break;
      case 16:
         i = x * 2;
         break;
      case 24:
         i = x * 3;
         break;
      case 32:
         i = x * 4;
         break;
      default: /* to suppress compiler warning */
         i = 0;
         break;
      }
   return i;

} /* PILCalcBSize() */

#ifdef PIL_IMAGE_WRITE
/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILLZWOutput(short, int, IRLCBUF *, int *, int *, BOOL)    *
 *                                                                          *
 *  PURPOSE    : Add a LZW code to the output buffer.                       *
 *                                                                          *
 ****************************************************************************/
static void PILLZWOutput(unsigned short code, int nbits, unsigned char *pOutBuf, int *bitnum, int *bitoff, BOOL bGIF)
{
unsigned long l, lc;
unsigned char *p;

p = pOutBuf + *bitoff;
if (bGIF)
   {
   l = INTELLONG(p);
   l |= (long)(code << *bitnum);
/* Need to do this instead of storing a long because some CPUs */
/* like Motorolas don't allow mis-aligned writes to memory */
   p[0] = (unsigned char)(l & 0xff);
   p[1] = (unsigned char)((l >> 8) & 0xff);
   p[2] = (unsigned char)((l >> 16) & 0xff);
   p[3] = (unsigned char)(l >> 24);
   *bitnum += nbits;
   *bitoff += *bitnum >> 3;
   *bitnum &= 7;
   }
else
   {
   l = MOTOLONG(p);
   lc = (long)code << (32 - *bitnum - nbits);
   l |= lc;
/* Need to do this instead of storing a long because some CPUs */
/* like Motorolas don't allow mis-aligned writes to memory */
   p[0] = (unsigned char)(l >> 24);
   p[1] = (unsigned char)((l >> 16) & 0xff);
   p[2] = (unsigned char)((l >> 8) & 0xff);
   p[3] = (unsigned char)(l & 0xff);
   *bitnum += nbits;
   *bitoff += *bitnum >> 3;
   *bitnum &= 7;
   }
} /* PILLZWOutput() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILGIFPrepLine(SPVOBJ *, int, char *, BOOL)                *
 *                                                                          *
 *  PURPOSE    : Convert a line of the image into 1 byte per pixel.         *
 *                                                                          *
 ****************************************************************************/
static void PILGIFPrepLine(PIL_PAGE *inpage, int y, unsigned char *curline, BOOL bGIF)
{
int x, lsize;
unsigned char *p;
int i, *indexbuf;

   if (inpage->iPitch)
      lsize = inpage->iPitch;
   else
      lsize = mPILCalcSize(inpage->iWidth, inpage->cBitsperpixel);
   indexbuf = (int *)inpage->pData;
   i = inpage->cBitsperpixel;
   if (!bGIF && inpage->cBitsperpixel > 1)
      i = 8; /* All bit depths of TIFF LZW use 8bpp code words */
   switch (i)
      {
      case 4:
         if (inpage->cFlags & PIL_PAGEFLAGS_BOTTOMUP)
            p = (unsigned char *)indexbuf + (((inpage->iHeight-1)-y) * lsize);
         else
            p = (unsigned char *)indexbuf + (y * lsize);
         x = (inpage->iWidth + 1) >> 1; /* Number of bytes */
         while (x)
            {
            *curline++ = *p >> 4; /* even pixel */
            *curline++ = *p & 0xf; /* odd pixel */
            x--;
            p++;
            }
         break;

      case 8:
         if (inpage->cFlags & PIL_PAGEFLAGS_BOTTOMUP)
            p = (unsigned char *)indexbuf + (((inpage->iHeight-1)-y) * lsize);
         else
            p = (unsigned char *)indexbuf + (y * lsize);
         memcpy(curline, p, lsize); /* Copy the line as-is */
         break;
      } /* switch */
   if (!bGIF && inpage->cBitsperpixel == 24) /* Need to reverse red/blue for TIFF output */
      PILFixTIFFRGB(curline, inpage->iWidth);

} /* PILGIFPrepLine() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILMakeLZW(PIL_PAGE *, PIL_PAGE *, int)                    *
 *                                                                          *
 *  PURPOSE    : Convert an image into LZW compressed data (run length)     *
 *                                                                          *
 ****************************************************************************/
static int PILMakeLZW(PIL_PAGE *pInPage, PIL_PAGE *pOutPage, BOOL bGIF)
{
int lsize, y;
int i, xcount, iMAXMAX;
int init_bits, nbits, bitoff, bitnum;
unsigned char *curline, *p, *pOutput;
short *codetab, disp, code, maxcode, cc, lastentry, free_ent, eoi;
int x;
long hashcode, cvar, *hashtab;

   pOutput = MiniIOAlloc((pInPage->iWidth * pInPage->iHeight * pInPage->cBitsperpixel)/8);
   if (pOutput == NULL)
      return PIL_ERROR_MEMORY;

   lsize = mPILCalcSize(pInPage->iWidth, pInPage->cBitsperpixel);
   curline = MiniIOAlloc(lsize);
   bitoff = bitnum = 0;
   init_bits = pInPage->cBitsperpixel + 1;
   if (bGIF)
      { /* GIF buffer has two special bytes to start */
      unsigned char *p;
      p = (unsigned char *)pOutput;
      *p++ = 0; /* Flags byte, nothing special here */
      *p++ = (unsigned char)init_bits; /* Starting code size stored here */
      bitoff = 2;
      xcount = pInPage->iWidth;
      iMAXMAX = MAXMAXCODE;
      }
   else
      {
      init_bits = 9; /* TIFF always uses 9 bits per codeword to start */
      xcount = PILCalcBSize(pInPage->iWidth, pInPage->cBitsperpixel);
      iMAXMAX = MAXMAXCODE - 1;
      }
   nbits = init_bits;
   hashtab = MiniIOAlloc(MAX_HASH * sizeof(long));
   codetab = MiniIOAlloc(MAX_HASH * sizeof(short));
   cc = 1 << (nbits - 1);
   eoi = cc + 1;
   lastentry = 0; /* To suppress compiler warning */
   free_ent = eoi + 1;
   maxcode = (1 << nbits) - 1;

   /* Clear the hash table */
   for (i=0; i<MAX_HASH; i++)
      hashtab[i] = -1;
   PILLZWOutput(cc, nbits, pOutput, &bitnum, &bitoff, bGIF); /* Start by encoding a cc */
   for (y=0; y < pInPage->iHeight; y++)
      {
/* Prepare a row of pixels as bytes for fast access */
      PILGIFPrepLine(pInPage, y, curline, bGIF);
      p = curline;
      x = xcount;
      if (y == 0) /* First time through, need lastentry to be valid */
         {
         lastentry = *p++; /* Get first pixel to start */
         x--;
         }
      while (x)
         {
         cvar = *p++; /* Grab a character to compress */
         x--;
         hashcode = (cvar << 12) + lastentry;
         code = (short)((cvar << 4) ^ lastentry);
         if (hashcode == hashtab[code])
            {
            lastentry = codetab[code];
            if (x != 0)
               continue;
            else
               break;
            }
         else
            {
            if (hashtab[code] < 0)
               goto gif_nomatch;
            disp = MAX_HASH - code;
            if (code == 0)
               disp = 1;
gif_probe:
            code -= disp;
            if (code < 0)
               code += MAX_HASH;
            if (hashtab[code] == hashcode)
               {
               lastentry = codetab[code];
               if (x!= 0)
                  continue;
               else
                  break;
               }
            if (hashtab[code] > 0)
               goto gif_probe;
gif_nomatch:
            PILLZWOutput(lastentry, nbits, pOutput, &bitnum, &bitoff, bGIF); /* encode this one */
            lastentry = (short)cvar;
            /* Check for code size increase/clear flag */
            if (bGIF)
               {
               if (free_ent > maxcode)
                  {
                  maxcode = (maxcode * 2) + 1;
                  nbits++;
                  }
               }
            else
               {
               if (free_ent >= maxcode) /* Need to increment code size */
                  {
                  maxcode = (maxcode * 2) + 1;
                  nbits++;
                  }
               }
            if (free_ent < iMAXMAX)
               {
               codetab[code] = free_ent++;
               hashtab[code] = hashcode;
               if (x!= 0)
                  continue;
               else
                  break;
               }
            else /* reset all tables */
               {
               free_ent = cc + 2;
               for (i=0; i<MAX_HASH; i++)
                   hashtab[i] = -1;
               nbits--; /* Bit count is wrong */
               PILLZWOutput(cc, nbits, pOutput, &bitnum, &bitoff, bGIF); /* encode this one */
            /* Check for code size increase/clear flag */
               nbits = init_bits;
               maxcode = (1 << nbits) - 1;
               }
            }
         }  /* while x */
      } /* for y */
/* Output the final code */
   PILLZWOutput(lastentry, nbits, pOutput, &bitnum, &bitoff, bGIF); /* encode this one */
   PILLZWOutput(eoi, nbits, pOutput, &bitnum, &bitoff, bGIF); /* encode this one */
   MiniIOFree(hashtab);
   MiniIOFree(codetab);
   MiniIOFree(curline);

   pOutPage->iDataSize = (int)(bitoff+1);
   pOutPage->pData = pOutput;
   pOutPage->cState = PIL_PAGE_STATE_LOADED;
   return 0;

} /* PILMakeLZW() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILEncodeRLE8(char *, char *, int)                         *
 *                                                                          *
 *  PURPOSE    : Encode a line as Windows BMP RLE format.                   *
 *                                                                          *
 ****************************************************************************/
static unsigned char * PILEncodeRLE8(unsigned char *pSrc, unsigned char *pDest, int iLen)
{
unsigned char *pStart, o, c;
int iCount;

encpb0:
//      if (iLen < 2) /* can't have repeats with less than 2 */
//         goto encpb1;
      pStart = pSrc; /* Preserve line pointer */
      iCount = 1; /* Repeat count */
      o = *pSrc++;  /* Set the compare byte */
enc0:
      c = *pSrc++;
      if (c == o)
         {
         iCount++;
         if (iCount != iLen) /* End of line reached? */
            goto enc0; /* Keep finding repeats */
         }
      pSrc = pStart; /* Restore pointer to start of section */
      if (iCount == 1) /* Any repeats? */
         goto encpb1; /* Look for consecutive, non-repeats */
      pSrc += iCount;
      iLen -= iCount;
      while (iCount > 255)
         {
         *pDest++ = 0xff; /* Store max count */
         *pDest++ = o; /* Store the repeating pixel */
         iCount -= 255;
         }
      if (iCount) /* We may have had exactly 255 */
         {
         *pDest++ = (unsigned char)iCount;
         *pDest++ = o;
         }
encpb1:
/* Find # of consec non-repeats */
      if (iLen == 0) /* Done with the line? */
         return pDest;
      if (iLen == 1) /* Special case */
         goto encpb2;
    /* Count non-repeats */
      pStart = pSrc;
      o = *pSrc++; /* Get the compare byte */
      iCount = 1; /* Assume 1 non-repeat to start */
enc1:
      c = *pSrc++;
      if (c == o)
         goto enc2;
      o = c;
      iCount++;
      if (iCount != iLen)
         goto enc1;
enc2:
      pSrc = pStart; /* Restore section pointer */
      if (iCount == 1) /* No repeats  */
         goto encpb2;
/* Store the count of non-repeats */
      iCount--; /* Get rid of extra one */
      iLen -= iCount;
      while (iCount > 254)
         {
         *pDest++ = 0; // command for non-repeats
         *pDest++ = 254; /* Store max non-repeat count of 254 */
         memcpy(pDest, pSrc, 254);
         pSrc += 254;
         pDest += 254;
         iCount -= 254;
         }
      if (iCount)
         {
         if (iCount <= 2) // special cases that would be misinterpreted as commands
            {
            while (iCount)
               {
               *pDest++ = 1; // repeat count of 1
               *pDest++ = *pSrc++; // pixel to repeat
               iCount--;
               }
            }
         else
            {
            *pDest++ = 0; // command for sequence of non-repeats
            *pDest++ = (char)iCount;
            memcpy(pDest, pSrc, iCount);
            pSrc += iCount;
            pDest += iCount;
            if (iCount & 1) // must remain on even byte boundary
               pDest++; // add a pad byte
            }
         }
encpb2:
      if (iLen == 1) /* Nothing useful left to do */
         {
         *pDest++ = 1;  // store a repeat count of 1
         *pDest++ = *pSrc++;
//         return pDest; // suppress compiler warning
         }
      else
         goto encpb0; /* Look for repeats */

   return pDest;

} /* PILEncodeRLE8() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILEncodeRLE4(char *, char *, int)                         *
 *                                                                          *
 *  PURPOSE    : Encode a line as Windows BMP RLE format.                   *
 *                                                                          *
 ****************************************************************************/
static unsigned char * PILEncodeRLE4(unsigned char *pSrc, unsigned char *pDest, int iLen)
{
unsigned char *pStart, o, c;
int iCount;

encpb0:
//      if (iLen < 2) /* can't have repeats with less than 2 */
//         goto encpb1;
      pStart = pSrc; /* Preserve line pointer */
      iCount = 1; /* Repeat count */
      o = *pSrc++;  /* Set the compare byte */
enc0:
      c = *pSrc++;
      if (c == o)
         {
         iCount++;
         if (iCount != iLen) /* End of line reached? */
            goto enc0; /* Keep finding repeats */
         }
      pSrc = pStart; /* Restore pointer to start of section */
      if (iCount == 1) /* Any repeats? */
         goto encpb1; /* Look for consecutive, non-repeats */
      pSrc += iCount;
      iLen -= iCount;
      while (iCount > 127)
         {
         *pDest++ = 0xfe; /* Store max count of 254 pixels */
         *pDest++ = o; /* Store the repeating pixel */
         iCount -= 127;
         }
      if (iCount) /* We may have had exactly 254 */
         {
         *pDest++ = (unsigned char)(iCount*2);
         *pDest++ = o;
         }
encpb1:
/* Find # of consec non-repeats */
      if (iLen == 0) /* Done with the line? */
         return pDest;
      if (iLen == 1) /* Special case */
         goto encpb2;
    /* Count non-repeats */
      pStart = pSrc;
      o = *pSrc++; /* Get the compare byte */
      iCount = 1; /* Assume 1 non-repeat to start */
enc1:
      c = *pSrc++;
      if (c == o)
         goto enc2;
      o = c;
      iCount++;
      if (iCount != iLen)
         goto enc1;
enc2:
      pSrc = pStart; /* Restore section pointer */
      if (iCount == 1) /* No repeats  */
         goto encpb2;
/* Store the count of non-repeats */
      iCount--; /* Get rid of extra one */
      iLen -= iCount;
      while (iCount > 126)
         {
         *pDest++ = 0; // command for non-repeats
         *pDest++ = 252; /* Store max non-repeat count of 252 */
         memcpy(pDest, pSrc, 126);
         pSrc += 126;
         pDest += 126;
         iCount -= 126;
         }
      if (iCount)
         {
         if (iCount == 1) // special cases that would be misinterpreted as commands
            {
            *pDest++ = 2; // repeat count of 2
            *pDest++ = *pSrc++; // pixel to repeat
            }
         else
            {
            *pDest++ = 0; // command for sequence of non-repeats
            *pDest++ = (char)iCount*2;
            memcpy(pDest, pSrc, iCount);
            pSrc += iCount;
            pDest += iCount;
            if (iCount & 1) // must remain on even byte boundary
               pDest++; // add a pad byte
            }
         }
encpb2:
      if (iLen == 1) /* Nothing useful left to do */
         {
         *pDest++ = 2;  // store a repeat count of 2
         *pDest++ = *pSrc++;
//         return pDest;
         }
      else
         goto encpb0; /* Look for repeats */

   return pDest;

} /* PILEncodeRLE4() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILMakeRLE(PIL_PAGE *, PIL_PAGE *, int)                    *
 *                                                                          *
 *  PURPOSE    : Convert an image into Windows BMP RLE format.              *
 *                                                                          *
 ****************************************************************************/
static int PILMakeRLE(PIL_PAGE *pInPage, PIL_PAGE *pOutPage, int iOptions)
{
unsigned char *buf, *pDest, *pSrc;
int y, lsize;

   if (pInPage->cBitsperpixel != 4 && pInPage->cBitsperpixel != 8)
      return PIL_ERROR_INVPARAM;

   lsize = mPILCalcSize(pInPage->iWidth, pInPage->cBitsperpixel);
   // need to encode the bitmap upside down, so if rightside-up, reverse it
   if (pInPage->cFlags & PIL_PAGEFLAGS_TOPDOWN)
      {
      pSrc = &pInPage->pData[pInPage->iOffset] + lsize * (pInPage->iHeight-1);
      lsize = -lsize;
      }
   else
      {
      pSrc = &pInPage->pData[pInPage->iOffset];
      }
   y = (pInPage->iWidth*pInPage->iHeight*pInPage->cBitsperpixel)/8;
   y = (y*3)/2; // give it some breathing room in case it expands
   pDest = buf = MiniIOAlloc(y); /* Allocate the output buffer structure */
   if (pDest == NULL)
      return PIL_ERROR_MEMORY;

   switch (pInPage->cBitsperpixel)
      {
      case 4:
         for (y=0; y<pInPage->iHeight; y++)
            {
            pDest = PILEncodeRLE4(pSrc, pDest, (pInPage->iWidth+1)/2);
            pDest[0] = 0;  // store an end of line command
            pDest[1] = 0;
            pDest += 2; 
            pSrc += lsize;
            }
         break;
      case 8:
         for (y=0; y<pInPage->iHeight; y++)
            {
            pDest = PILEncodeRLE8(pSrc, pDest, pInPage->iWidth);
            pDest[0] = 0;  // store an end of line command
            pDest[1] = 0;
            pDest += 2; 
            pSrc += lsize;
            }
         break;
      }
   pDest[0] = 0; // cmd for end of bitmap = 0,1
   pDest[1] = 1;
   pDest +=2 ;
   pOutPage->pData = buf;
   pOutPage->iDataSize = (int)pDest - (int)buf;
   pOutPage->cState = PIL_PAGE_STATE_LOADED;
   return 0;

} /* PILMakeRLE() */
#endif // PIL_IMAGE_WRITE

#ifdef PIL_IMAGE_READ
/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILCountGIFPages()                                         *
 *                                                                          *
 *  PURPOSE    : Determine the number of pages in a GIF file.               *
 *                                                                          *
 ****************************************************************************/
static void PILCountGIFPages(void * iHandle, PIL_FILE *pFile)
{
int iOff, iNumPages;
long lFileOff = 0;
BOOL bDone = FALSE;
BOOL bExt;
unsigned char c, *cBuf;

   iNumPages = 0;
   pFile->pPageList = MiniIOAlloc(MAX_PAGES * sizeof(int));
   pFile->pPageList[iNumPages++] = 0; /* First page starts at 0 */
   if (pFile->cState == PIL_FILE_STATE_LOADED) // use provided pointer
      {
      cBuf = pFile->pData;
      }
   else
      {
      cBuf = MiniIOAlloc(0x10000);
      MiniIOSeek(iHandle, 0, 0); // seek to beginning again
      MiniIORead(iHandle, cBuf, 0x10000); /* Read 64k to start */
      }
   iOff = 6;
   pFile->iX = cBuf[iOff] + (cBuf[iOff+1]<<8); // get width
   iOff += 2;
   pFile->iY = cBuf[iOff] + (cBuf[iOff+1]<<8); // get height
   iOff += 2;
   c = cBuf[iOff]; // get info bits
   pFile->cBpp = cGIFBits[(c & 7)+1]; // bits per pixel (converted to supported values)
   iOff += 3;   /* Skip flags, background color & aspect ratio */
   if (c & 0x80) /* Deal with global color table */
      {
      c &= 7;  /* Get the number of colors defined */
      iOff += (2<<c)*3; /* skip color table */
      }
   while (!bDone)
      {
      bExt = TRUE; /* skip extension blocks */
      while (bExt)
         {
         switch(cBuf[iOff])
            {
            case 0x3b: /* End of file */
               /* we were fooled into thinking there were more pages */
               iNumPages--;
               goto gifpagesz;
            case 0x21: /* Extension block */
               iOff += 2; /* skip to length */
               iOff += (int)cBuf[iOff]; /* Skip the data block */
               iOff++;
               c = cBuf[iOff++]; /* Skip any sub-blocks */
               while (c)
                  {
                  iOff += (int)c;
                  c = cBuf[iOff++];
                  }
               break;
            case 0x2c: /* Start of image data */
               bExt = FALSE; /* Stop doing extension blocks */
               break;
            default:
               /* Corrupt data, stop here */
               iNumPages--;
               goto gifpagesz;
            }
         }
      /* Start of image data */
      c = cBuf[iOff+9]; /* Get the flags byte */
      iOff += 10; /* Skip image position and size */
      if (c & 0x80) /* Local color table */
         {
         c &= 7;
         iOff += (2<<c)*3;
         }
      iOff++; /* Skip LZW code size byte */
      c = cBuf[iOff++];
      while (c) /* While there are more data blocks */
         {
         if (pFile->cState != PIL_FILE_STATE_LOADED && iOff > 65000) /* Near end of buffer, re-align */
            {
            lFileOff += iOff; /* adjust total file pointer */
            MiniIOSeek(iHandle, lFileOff, 0); /* Re-seek to current spot */
            MiniIORead(iHandle, cBuf, 65536); /* Read a new block */
            iOff = 0; /* Start at beginning of buffer */
            }
         iOff += (int)c;  /* Skip this data block */
         c = cBuf[iOff++]; /* Get length of next */
         }
      /* End of image data, check for more pages... */
      if ((lFileOff + iOff > pFile->iFileSize) || cBuf[iOff] == 0x3b)
         {
         bDone = TRUE; /* End of file has been reached */
         }
      else /* More pages to scan */
         {
         pFile->pPageList[iNumPages++] = lFileOff + iOff;
         }
      } /* while !bDone */
gifpagesz:
      pFile->pPageList[iNumPages] = pFile->iFileSize; /* Store end of file for length calc */
      pFile->iPageTotal = iNumPages;
      if (iNumPages == 1) // no need for pagelist structure
         {
         MiniIOFree(pFile->pPageList);
         pFile->pPageList = NULL;
         }
      if (pFile->cState != PIL_FILE_STATE_LOADED)
         MiniIOFree(cBuf); // free the temp buffer

} /* PILCountGIFPages() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILTest()                                                  *
 *                                                                          *
 *  PURPOSE    : See if the file is a supported type.                       *
 *                                                                          *
 ****************************************************************************/
BOOL mPILTest(char *szFileName)
{
int i, j, iNumBytes;
void *iHandle;
unsigned char cBuf[1024]; // small buffer to load header info

   iHandle = MiniIOOpenRO(szFileName);
   if (iHandle == (void *)-1)
      {
      return FALSE;
      }

   iNumBytes = MiniIORead(iHandle, cBuf, 1024); /* Read a decent sized chunk */
   MiniIOClose(iHandle);

   if (iNumBytes < 50) /* Valid files must be at least 50 bytes long */
      {
      return FALSE;
      }

   /*--- Check for PNG ---*/
   if (MOTOLONG(cBuf) == 0x89504e47)
      {
      return TRUE;
      }

   /*--- Check for Adobe PDF ---*/
   if (MOTOLONG(cBuf) == 0x25504446 /*'%PDF'*/)
      {
      return TRUE;
      }

   /*--- Check for Canon RAW (CRW) ---*/
   if (MOTOLONG(cBuf) == 0x49491a00)
      {
      return TRUE;
      }

   /*--- Check for CALS ---*/
   if (INTELLONG(&cBuf[0]) == 0x64637273)
      {
      if (cBuf[775] != '1') /* If not type 1, we can't do it */
         {
         return FALSE;
         }
      return TRUE;
      }

   /*--- Check for WIN & OS2 bitmap ---*/
   if (cBuf[0] == 'B' && cBuf[1] == 'M')
      {
      if (cBuf[14] == 0x28) /* Windows */
         {
         return TRUE;
         }
      else /* OS/2 BMP */
         {
         if (INTELSHORT(&cBuf[14]) != 12 && INTELSHORT(&cBuf[14]) < 20)
            {
            return FALSE; /* Unknown file type */
            }
         return TRUE; /* Done, leave */
         }
      } /* WIN + OS2 BMP */

  /*--- Check for JPEG JFIF ---*/
   if ((MOTOLONG(cBuf) & 0xffffff00) == 0xffd8ff00)
      {
      return TRUE;
      } /* JPEG */

  /*--- Check for DCX ---*/
   if (INTELLONG(cBuf) == 0x3ade68b1)
      {
      return TRUE;
      } /* DCX */

  /*--- Check for PCX ---*/
   if (cBuf[0] == 0x0a && cBuf[1] < 0x6 && cBuf[2] == 0x01)
      {
      return TRUE;
      }  /* PCX */

  /*--- Check for Microsoft FAX (*.AWD) ---*/
   if (INTELLONG(cBuf) == 0xe011cfd0 && INTELLONG(&cBuf[4]) == 0xe11ab1a1)
      {
      long l;

      l = 0x6f0052;
      /* Look for 'root entry' marker */
      for (i=0; i<4096; i++)
         {
         if (memcmp(&cBuf[i], &l, sizeof(long)) == 0) // this looks ugly, but is necessary
            break;
         }
      if (i == 4096)
         goto checkgif;  /* Not an AWD file */

      return TRUE;
      } /* AWD */

checkgif:
   /*--- Check for GIF ---*/
   if (MOTOLONG(cBuf) == 0x47494638 /*'GIF8'*/)
      {
      return TRUE;
      }

   /*--- Check for TIFF ---*/
   if ((cBuf[0] == 'I' && cBuf[1] == 'I') || (cBuf[0] == 'M' && cBuf[1] == 'M'))
      {
      return TRUE;
      } /* TIFF */

   /*--- Check for IBM PSEG ---*/
   if (cBuf[0] == 'Z' && cBuf[MOTOSHORT(&cBuf[1])+1] == 'Z')
      {
      return TRUE;
      } /* PSEG */

   // Check for MPEG
   if (MOTOLONG(cBuf) == 0x1ba || MOTOLONG(cBuf) == 0x1b3)
      return TRUE;

   /*--- Check for True Vision Targa --- */ // This should be last since it is too vague
   i = cBuf[1] & 0xfe;
   j = cBuf[2];
   if (i == 0 && (j == 1 || j == 2 || j == 3 || j == 9 || j == 10 || j == 11))
      {
      i = INTELSHORT(&cBuf[12]); // width
      j = INTELSHORT(&cBuf[14]); // height
      if (i == 0 || j == 0 || i > 32767 || j > 32767)
         goto no_good;
      i = cBuf[16]; // bits per pixel
      if (i != 1 && i != 4 && i != 8 && i != 16 && i != 24 && i != 32)
         goto no_good;
      return TRUE;
      }
   /* File type is not known */
no_good:
   return FALSE;

} /* mPILTest() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILOpen()                                                  *
 *                                                                          *
 *  PURPOSE    : Open an image file, determine if it's a supported type     *
 *                                                                          *
 ****************************************************************************/
int mPILOpen(char *szFileName, PIL_FILE *pFile, int iOptions)
{
int i, j, iNumBytes, iErr;
void *iHandle=NULL;
unsigned char cBuf[4096]; // small buffer to load header info

   iErr = 0;
   if (pFile->cState == PIL_FILE_STATE_LOADED) // skip the file handling if the data is already in memory
      {
      memcpy(cBuf, pFile->pData, 128);
      iNumBytes = 128; // fool it into seeing a good "read"
      }
   else // need to read it from a file
      {
      if (!MiniIOExists(szFileName)) // see if the file exists
         return PIL_ERROR_FILENF; // not found
      iHandle = MiniIOOpenRO(szFileName);
      if (iHandle == (void *)-1)
         {
         iErr = PIL_ERROR_IO;
         goto pilopenz;
         }
      memset(pFile, 0, sizeof(PIL_FILE)); // start with a clean structure
      pFile->iFileSize = MiniIOSize(iHandle);
      if (pFile->iFileSize == 0) // huge or empty file, we don't support it
         {
         iErr = PIL_ERROR_IO;
         goto pilopenz;
         }
      iNumBytes = MiniIORead(iHandle, cBuf, 128); /* Read a little chunk */
      if (iNumBytes < 32) /* Valid files must be at least 32 bytes long */
         {
         iErr = PIL_ERROR_UNKNOWN;
         goto pilopenz;
         }
      pFile->cState = PIL_FILE_STATE_OPEN; // successfully opened (so far)
      MiniIOSeek(iHandle, 0, 0);
      }

   pFile->iPageTotal = 1; /* Assume 1 page */
      
   /*--- Check for PNG ---*/
   if (MOTOLONG(cBuf) == 0x89504e47)
      {
      pFile->cFileType = PIL_FILE_PNG;
      goto pilopenz;
      }

   /*--- Check for WIN & OS2 bitmap ---*/
   if (cBuf[0] == 'B' && cBuf[1] == 'M')
      {
      if (cBuf[14] == 0x28) /* Windows */
         {
         pFile->cFileType = PIL_FILE_WINBMP;
         goto pilopenz; /* Done, leave */
         }
      else /* OS/2 BMP */
         {
         if (INTELSHORT(&cBuf[14]) != 12 && INTELSHORT(&cBuf[14]) < 20)
            {
            iErr = PIL_ERROR_UNKNOWN;
            goto pilopenz; /* Unknown file type */
            }
         pFile->cFileType = PIL_FILE_OS2BMP;
         goto pilopenz; /* Done, leave */
         }
      } /* WIN + OS2 BMP */

  /*--- Check for JPEG JFIF ---*/
   if ((MOTOLONG(cBuf) & 0xffffff00) == 0xffd8ff00)
      {
      pFile->cFileType = PIL_FILE_JFIF;
      goto pilopenz; /* done */
      } /* JPEG */

   /*--- Check for GIF ---*/
   if (MOTOLONG(cBuf) == 0x47494638 /*'GIF8'*/)
      {
      pFile->cFileType = PIL_FILE_GIF;
      PILCountGIFPages(iHandle, pFile);
      goto pilopenz;
      }

   /*--- Check for True Vision Targa --- */ // This should be last since it is too vague
   i = cBuf[1] & 0xfe;
   j = cBuf[2];
   // make sure it is not a MPEG file (starts with 00 00 01 BA)
   if (MOTOLONG(cBuf) != 0x1ba && MOTOLONG(cBuf) != 0x1b3 && i == 0 && (j == 1 || j == 2 || j == 3 || j == 9 || j == 10 || j == 11))
      {
      pFile->cFileType = PIL_FILE_TARGA;
      goto pilopenz; /* done */
      }

   /* File type is not known */
   iErr = PIL_ERROR_UNKNOWN;

   pilopenz:
      if (iErr == 0) /* Success */
         {
         if (pFile->cState != PIL_FILE_STATE_LOADED)
            {
            // retain this open file handle
            pFile->iFile = iHandle;
            pFile->pData = NULL;
            }
         }
      else
         {
         if (iHandle != (void *)-1)
            MiniIOClose(iHandle);    // close the file handle used for sequential access
         pFile->cState = PIL_FILE_STATE_CLOSED;
         }
      return iErr;

} /* mPILOpen() */
#endif // PIL_IMAGE_READ

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILFixGIFRGB()                                             *
 *                                                                          *
 *  PURPOSE    : Swap RED and BLUE palette order for GIF file.              *
 *                                                                          *
 ****************************************************************************/
static void PILFixGIFRGB(unsigned char *p)
{
int i;
unsigned char c;

   for(i=0; i<256; i++)
      {
      c = p[0];  /* Swap red and blue */
      p[0] = p[2];
      p[2] = c;
      p += 3;
      }

} /* PILFixGIFRGB() */

#ifdef PIL_IMAGE_WRITE
/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILWrite()                                                 *
 *                                                                          *
 *  PURPOSE    : Write a file into a file.                                  *
 *                                                                          *
 ****************************************************************************/
int mPILWrite(PIL_FILE *pFile, PIL_PAGE *pPage, int iOptions)
{
int i, j, iSize, iErr, lsize, iHeaderSize;
unsigned char *p, *pBuf, *pSrc;
unsigned short *usSrc, *usDest;

      iErr = 0;

      pBuf = MiniIOAlloc(0x10000);
      switch (pFile->cFileType)
         {
         case PIL_FILE_WINBMP:
            if (pFile->iPageTotal > 0)
               iErr = PIL_ERROR_INVPARAM; // WinBMPs can only have 1 page
            else
               { // DEBUG - only works for 16 and 24 bpp
               lsize = mPILCalcSize(pPage->iWidth, pPage->cBitsperpixel);
               iHeaderSize = 54;
               if (pPage->cBitsperpixel < 16)
                  {
                  iHeaderSize += (1<<(pPage->cBitsperpixel+2));
                  }
               i = (lsize * pPage->iHeight) + iHeaderSize; // datasize

               memset(pBuf, 0, 54);
               pBuf[0] = 'B';
               pBuf[1] = 'M';
               pBuf[2] = i & 0xff;     // 4 bytes of file size
               pBuf[3] = (i >> 8) & 0xff;
               pBuf[4] = (i >> 16) & 0xff;
               pBuf[5] = (i >> 24) & 0xff;
               /* Offset to data bits */
               pBuf[10] = iHeaderSize & 0xff;
               pBuf[11] = (unsigned char)(iHeaderSize >> 8);
               pBuf[14] = 0x28;
               pBuf[18] = pPage->iWidth & 0xff; // xsize low
               pBuf[19] = (unsigned char)(pPage->iWidth >> 8); // xsize high
               pBuf[22] = pPage->iHeight & 0xff; // ysize low
               pBuf[23] = (unsigned char)(pPage->iHeight >> 8); // ysize high
               pBuf[26] = 1; // number of planes
               pBuf[28] = pPage->cBitsperpixel;
               if (pPage->cCompression == PIL_COMP_WINRLE)
                  {
                  if (pPage->cBitsperpixel == 4)
                     pBuf[30] = 2; // 4-bit RLE
                  else
                     pBuf[30] = 1; // 8-bit RLE
                  i = pPage->iDataSize;
                  pBuf[34] = i & 0xff;  // data size
                  pBuf[35] = (i >> 8) & 0xff;
                  pBuf[36] = (i >> 16) & 0xff;
                  pBuf[37] = (i >> 24) & 0xff;
                  }
               else
                  {
                  pBuf[30] = 0; // uncompressed
                  i = lsize * pPage->iHeight;
                  pBuf[34] = i & 0xff;  // data size
                  pBuf[35] = (i >> 8) & 0xff;
                  pBuf[36] = (i >> 16) & 0xff;
                  pBuf[37] = (i >> 24) & 0xff;
                  }
               MiniIOWrite(pFile->iFile, pBuf, 54);
               if (pPage->cBitsperpixel < 16)
                  {
                  if (pPage->cBitsperpixel == 1)
                     {
                     pBuf[0] = pBuf[1] = pBuf[2] = pBuf[3] = pBuf[7] = 0;
                     pBuf[4] = pBuf[5] = pBuf[6] = 0xff;
                     MiniIOWrite(pFile->iFile, pBuf, 8);
                     }
                  else
                     {
                     p = pPage->pPalette;
                     j = 1 << pPage->cBitsperpixel;
                     pBuf[3] = 0;
                     for (i=0; i<j; i++)
                        {
                        pBuf[0] = p[0];    // swap red and blue order
                        pBuf[1] = p[1];
                        pBuf[2] = p[2];
                        p += 3;
                        MiniIOWrite(pFile->iFile, pBuf, 4);
                        }
                     }
                  }
               if (pPage->cCompression == PIL_COMP_NONE)
                  {
                  if (pPage->cFlags & PIL_PAGEFLAGS_TOPDOWN)
                     {
                    // bitmap is upside-down, so copy from bottom line up
                     pSrc = pPage->pData + (pPage->iHeight-1)*pPage->iPitch;
                     }
                  else
                     pSrc = pPage->pData;

                  for (i=0; i<pPage->iHeight; i++)
                     {
                     if (pPage->cBitsperpixel == 16) // special case, need to make it RGB555
                        {
                        usSrc = (unsigned short *)pSrc;
                        usDest = (unsigned short *)pBuf;
                        for (j=0; j<abs(lsize)/2; j++)
                           {
                           usDest[0] = ((usSrc[0] & 0xffc0) >> 1) | (usSrc[0] & 0x1f);
                           usSrc++;
                           usDest++;
                           }
                        if (lsize < 0)
                           MiniIOWrite(pFile->iFile, pBuf, -lsize);
                        else
                           MiniIOWrite(pFile->iFile, pBuf, lsize);
                        }
                     else
                        {
                        if (lsize < 0)
                           MiniIOWrite(pFile->iFile, pSrc, -lsize);
                        else
                           MiniIOWrite(pFile->iFile, pSrc, lsize);
                        }
                  if (pPage->cFlags & PIL_PAGEFLAGS_TOPDOWN)
                     pSrc -= pPage->iPitch;
                  else
                     pSrc += pPage->iPitch;
                     }
                  }
               else // RLE compressed
                  MiniIOWrite(pFile->iFile, pPage->pData, pPage->iDataSize);
               }
            break;
         case PIL_FILE_GIF:
            if (pPage->cCompression != PIL_COMP_GIF)
               {
               iErr = PIL_ERROR_INVPARAM;
               break;
               }
            MiniIOWrite(pFile->iFile, "GIF87a", 6);
            pBuf[0] = pPage->iWidth & 0xff;
            pBuf[1] = (unsigned char)(pPage->iWidth >> 8);
            pBuf[2] = pPage->iHeight & 0xff;
            pBuf[3] = (unsigned char)(pPage->iHeight >> 8);
            pBuf[4] = (pPage->cBitsperpixel - 1) | 0x80;
            pBuf[5] = 0; /* Background color */
            pBuf[6] = 0; /* Future expansion */
            MiniIOWrite(pFile->iFile, pBuf, 7);
            /* Write the color palette */
            if (pPage->cBitsperpixel == 1)
               {
               i = 0;
               MiniIOWrite(pFile->iFile, &i, 3);
               i = -1;
               MiniIOWrite(pFile->iFile, &i, 3);
               }
            else
               {
               PILFixGIFRGB(pPage->pPalette);
               MiniIOWrite(pFile->iFile, pPage->pPalette, 3*(1<<pPage->cBitsperpixel));
               PILFixGIFRGB(pPage->pPalette);
               }
            pBuf[0] = '!'; // add copyright info in an app extension
            pBuf[1] = 0xff; // app extension
            pBuf[2] = 33;
            strcpy((char *)&pBuf[3],"PIL - Portable Imaging Libary 1.0");
            pBuf[36] = 0; // length terminator
            MiniIOWrite(pFile->iFile, pBuf, 37);
            if (pPage->szComment[0]) // write the comment to the file
               {
               pBuf[0] = '!';
               pBuf[1] = 0xfe; // comment
               i = strlen(pPage->szComment);
               pBuf[2] = (unsigned char)i;
               strcpy((char *)&pBuf[3], pPage->szComment);
               pBuf[3+i] = 0;
               MiniIOWrite(pFile->iFile, pBuf, 4+i);
               }
            MiniIOWrite(pFile->iFile, ",", 1);
            i = 0;
            MiniIOWrite(pFile->iFile, &i, 4); /* Image position */
            pBuf[0] = pPage->iWidth & 0xff;
            pBuf[1] = (unsigned char)(pPage->iWidth >> 8);
            pBuf[2] = pPage->iHeight & 0xff;
            pBuf[3] = (unsigned char)(pPage->iHeight >> 8);
            MiniIOWrite(pFile->iFile, pBuf, 4); /* Image size again */
           /* Initial code size */
            pBuf[0] = pPage->cBitsperpixel - 1;
            if (pPage->cBitsperpixel == 1)
               pBuf[1] = 2;
            else
               pBuf[1] = pPage->cBitsperpixel;
            MiniIOWrite(pFile->iFile, pBuf, 2);
            iSize = pPage->iDataSize - 2;
            i = 2;
            /* re-packetize the image data */
            pBuf[0] = 0xff;
            while (iSize > 255)
               {
               MiniIOWrite(pFile->iFile, pBuf, 1); /* packet length */
               MiniIOWrite(pFile->iFile, &pPage->pData[i], 255);
               iSize -= 255;
               i += 255;
               }
            pBuf[0] = (char)iSize;
            MiniIOWrite(pFile->iFile, pBuf, 1);
            MiniIOWrite(pFile->iFile, &pPage->pData[i], iSize);
            pBuf[0] = '\0';
            pBuf[1] = ';';
            MiniIOWrite(pFile->iFile, pBuf, 2); /* Write the GIF terminator */
            break;
         case PIL_FILE_JFIF:
            if (pFile->iPageTotal > 0)
               iErr = PIL_ERROR_INVPARAM; // JFIFs can only have 1 page
            else
               {
               MiniIOWrite(pFile->iFile, pPage->pData, pPage->iDataSize);
               }
            break;
         default:
            iErr = PIL_ERROR_INVPARAM;
            break;
         }

   if (!iErr)
      pFile->iPageTotal++;
   MiniIOFree(pBuf);
   
   return iErr;

} /* mPILWrite() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILCreate()                                                *
 *                                                                          *
 *  PURPOSE    : Create an image file ready to receive pages.               *
 *                                                                          *
 ****************************************************************************/
int mPILCreate(char *szFileName, PIL_FILE *pFile, int iOptions, int iFileType)
{
int iErr;
void *iHandle;

   iErr = 0;

      if (iOptions & PIL_OPTION_APPEND) // try to open an existing file
         {
         iHandle = MiniIOOpen(szFileName);
         if (iHandle == (void *)-1) // does not exist, create it
            iHandle = MiniIOCreate(szFileName);
         }
      else
         iHandle = MiniIOCreate(szFileName);
   if (iHandle == (void *)-1)
      {
      iErr = PIL_ERROR_IO;
      goto pilcreatez;
      }
   memset(pFile, 0, sizeof(PIL_FILE)); // start with a clean structure
   pFile->iFile = iHandle;
   pFile->cState = PIL_FILE_STATE_OPEN;
   pFile->cFileType = (char)iFileType;

   pilcreatez:
   if (iErr != 0 && iHandle != (void *)-1)
      {
      MiniIOClose(iHandle);
      pFile->iFile = 0;
      }
   return iErr;

} /* mPILCreate() */
#endif // PIL_IMAGE_WRITE

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILFixGifRGB()                                             *
 *                                                                          *
 *  PURPOSE    : Convert the palette entries to be in Windows format.       *
 *                                                                          *
 ****************************************************************************/
static void PILFixGifRGB(unsigned char *pPal)
{
int i, j;
unsigned char c;

   j = 0;
   for(i=0; i<256; i++)
      {
      c = pPal[j];  /* Swap red and blue */
      pPal[j] = pPal[j+2];
      pPal[j+2] = c;
      j += 3;
      }

} /* PILFixGifRGB() */

#ifdef PIL_IMAGE_READ

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILGetTargaMap()                                           *
 *                                                                          *
 *  PURPOSE    : Read the color map into our palette structure.             *
 *                                                                          *
 ****************************************************************************/
static int PILGetTargaMap(PIL_PAGE *pPage, unsigned char *pData)
{
unsigned char *pDest, *pSrc;
int i, iLen, iOrigin, iMapLen;
unsigned short us, *pus;

   iLen = pData[0] + 18;  // ID Field + length
   pDest = pPage->pPalette;
   iOrigin = INTELSHORT(&pData[3]) * 3;
   iMapLen = INTELSHORT(&pData[5]) * 3;
   if ((iOrigin + iMapLen) > 768)
      return PIL_ERROR_BADHEADER; // bad header info
   pDest += iOrigin;
   pSrc = pData + iLen;
   switch (pData[7]) // color map entry size
      {
      case 16:
         pus = (unsigned short *)pSrc;
         for (i=0; i<iMapLen; i++)
            {
            us = *pus++;
            pDest[0] = (unsigned char)((us << 3));   // blue
            pDest[1] = (us >> 2) & 0xf8; // green
            pDest[2] = (us >> 7) & 0xf8; // red
            pDest += 3;
            }
         break;
      case 24:
         memcpy(pDest, pSrc, iMapLen*3); // just copy as-is
         break;
      case 32:
         for (i=0; i<iMapLen; i++)
            {
            pDest[0] = pSrc[0];
            pDest[1] = pSrc[1];
            pDest[2] = pSrc[2];
            pDest += 3;
            pSrc += 4; // skip the attribute byte
            }
         break;
      }
   return 0;

} /* PILGetTargaMap() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILConvertTargaStrange(char *, int)                        *
 *                                                                          *
 *  PURPOSE    : Convert 8-bit samples to true bilevel image.               *
 *                                                                          *
 ****************************************************************************/
static void PILConvertTargaStrange(unsigned char *pSrc, int iWidth)
{
int i, j, iCount;
unsigned char c, cMask, *d;

   d = pSrc;
   iCount = (iWidth + 7)>>3; // number of destination bytes
   for (i=0; i<iCount; i++)
      {
      cMask = 0x80;
      c = 0;
      for (j = 0; j<8; j++) // bits in the byte
         {
         if (*pSrc++)
            c |= cMask;
         cMask >>= 1;
         }
      *d++ = c;
      }

} /* PILConvertTargaStrange() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILGetTargaC()                                             *
 *                                                                          *
 *  PURPOSE    : Read a compressed Targa image.                             *
 *                                                                          *
 ****************************************************************************/
static int PILGetTargaC(PIL_PAGE *pPage, PIL_FILE *pFile)
{
unsigned char *s, *d, *sEnd, c, p, p2, p3, p4, *irlcptr, *pTemp;
int *indexbuf;
unsigned short *ud, up;
int i, x, y;

   pPage->cState = PIL_PAGE_STATE_LOADED; // all pages will be loaded
   switch (pPage->cBitsperpixel)
      {
      case 16:
         s = &pFile->pData[pPage->iOffset];
         sEnd = &pFile->pData[pFile->iFileSize];
         pPage->pData = MiniIOAlloc(pPage->iPitch * pPage->iHeight);
         ud = (unsigned short *)pPage->pData;
         if (pPage->pData == NULL)
            return PIL_ERROR_MEMORY;
         pPage->iOffset = 0;
         y = 0;
         x = pPage->iWidth;
         while (y < pPage->iHeight && s < sEnd)
            {
            c = *s++; // get the count
            if (c >=128) // negative = repeating byte
               {
               c &= 0x7f;
               c++;
               up = INTELSHORT(s); // pixel to repeat
               up = ((up & 0x7fe0) << 1) | (up & 0x1f);
               s += 2;
               while (c > x && s < sEnd)
                  {
                  for (i=0; i<x; i++)
                     *ud++ = up;
                  c -= (unsigned char)x;
                  y++;
                  x = pPage->iWidth;
                  }
               for (i=0; i<c; i++)
                  *ud++ = up;
               x -= c;
               }
            else // raw data
               {
               c++;
               while (c > x && s < sEnd)
                  {
                  for (i=0; i<x; i++)
                     {
                     up = INTELSHORT(s); // pixel to repeat
                     up = ((up & 0x7fe0) << 1) | (up & 0x1f);
                     s += 2;
                     *ud++ = up;
                     }
                  c -= (unsigned char)x;
                  y++;
                  x = pPage->iWidth;
                  }
               for (i=0; i<c; i++)
                  {
                  up = INTELSHORT(s); // pixel to repeat
                  up = ((up & 0x7fe0) << 1) | (up & 0x1f);
                  s += 2;
                  *ud++ = up;
                  }
               x -= c;
               }
            }
         break;
      case 32:
         s = &pFile->pData[pPage->iOffset];
         sEnd = pFile->pData + pFile->iFileSize;
         d = pPage->pData = MiniIOAlloc(pPage->iPitch * pPage->iHeight);
         if (pPage->pData == NULL)
            return PIL_ERROR_MEMORY;
         pPage->iOffset = 0;
         y = 0;
         x = pPage->iWidth;
         while (y < pPage->iHeight && s < sEnd)
            {
            c = *s++; // get the count
            if (c >=128) // negative = repeating byte
               {
               c &= 0x7f;
               c++;
               p = *s++; // pixel to repeat
               p2 = *s++;
               p3 = *s++;
               p4 = *s++;
               while (c > x && s < sEnd)
                  {
                  for (i=0; i<x; i++)
                     {
                     d[0] = p;
                     d[1] = p2;
                     d[2] = p3;
                     d[3] = p4;
                     d += 4;
                     }
                  c -= (unsigned char)x;
                  y++;
                  x = pPage->iWidth;
                  }
               for (i=0; i<c; i++)
                  {
                  d[0] = p;
                  d[1] = p2;
                  d[2] = p3;
                  d += 3;
                  }
               x -= c;
               }
            else // raw data
               {
               c++;
               while (c > x && s < sEnd)
                  {
                  for (i=0; i<x; i++)
                     {
                     d[0] = s[0];
                     d[1] = s[1];
                     d[2] = s[2];
                     d[3] = s[3];
                     d += 4;
                     s += 4;
                     }
                  c -= (unsigned char)x;
                  y++;
                  x = pPage->iWidth;
                  }
               for (i=0; i<c; i++)
                  {
                  d[0] = s[0];
                  d[1] = s[1];
                  d[2] = s[2];
                  d[3] = s[3];
                  d += 4;
                  s += 4;
                  }
               x -= c;
               }
            }
         pPage->cBitsperpixel = 32;
         break;
      case 24:
         s = &pFile->pData[pPage->iOffset];
         sEnd = pFile->pData + pFile->iFileSize;
         d = pPage->pData = MiniIOAlloc(pPage->iPitch * pPage->iHeight);
         if (pPage->pData == NULL)
            return PIL_ERROR_MEMORY;
         pPage->iOffset = 0;
         y = 0;
         x = pPage->iWidth;
         while (y < pPage->iHeight && s < sEnd)
            {
            c = *s++; // get the count
            if (c >=128) // negative = repeating byte
               {
               c &= 0x7f;
               c++;
               p = *s++; // pixel to repeat
               p2 = *s++;
               p3 = *s++;
               while (c > x && s < sEnd)
                  {
                  for (i=0; i<x; i++)
                     {
                     d[0] = p;
                     d[1] = p2;
                     d[2] = p3;
                     d += 3;
                     }
                  c -= (unsigned char)x;
                  y++;
                  x = pPage->iWidth;
                  }
               for (i=0; i<c; i++)
                  {
                  d[0] = p;
                  d[1] = p2;
                  d[2] = p3;
                  d += 3;
                  }
               x -= c;
               }
            else // raw data
               {
               c++;
               while (c > x && s < sEnd)
                  {
                  memcpy(d, s, x*3);
                  d += x*3;
                  s += x*3;
                  c -= (unsigned char)x;
                  y++;
                  x = pPage->iWidth;
                  }
               memcpy(d, s, c*3);
               x -= c;
               d += c*3;
               s += c*3;
               }
            }
         break;

      case 8:
         s = &pFile->pData[pPage->iOffset];
         sEnd = pFile->pData + pFile->iFileSize;
         d = pPage->pData = MiniIOAlloc(pPage->iPitch * pPage->iHeight);
         if (pPage->pData == NULL)
            return PIL_ERROR_MEMORY;
         pPage->iOffset = 0;
         y = 0;
         x = pPage->iWidth;
         while (y < pPage->iHeight && s < sEnd)
            {
            c = *s++; // get the count
            if (c >=128) // negative = repeating byte
               {
               c &= 0x7f;
               c++;
               p = *s++; // pixel to repeat
               while (c > x && s < sEnd)
                  {
                  memset(d, p, x);
                  d += x;
                  c -= (unsigned char)x;
                  y++;
                  x = pPage->iWidth;
                  }
               memset(d, p, c);
               x -= c;
               d += c;
               }
            else // raw data
               {
               c++;
               while (c > x && s < sEnd)
                  {
                  memcpy(d, s, x);
                  d += x;
                  s += x;
                  c -= (unsigned char)x;
                  y++;
                  x = pPage->iWidth;
                  }
               memcpy(d, s, c);
               x -= c;
               d += c;
               s += c;
               }
            }
         break;

      case 1:
         indexbuf = MiniIOAlloc(((pPage->iWidth*pPage->iHeight)/2) + pPage->iHeight*sizeof(int)); // make a reasonable estimate of the max data size
         if (!indexbuf) /* Allocate the irlc buffer structure */
            return PIL_ERROR_MEMORY;
         pPage->pData = (unsigned char *)indexbuf;
         pPage->cState = PIL_PAGE_STATE_LOADED;
         pPage->cCompression = PIL_COMP_RLE; // we are generating RLE encoded data
         irlcptr = (unsigned char *)&indexbuf[pPage->iHeight];
         s = &pFile->pData[pPage->iOffset];
         sEnd = pFile->pData + pFile->iFileSize;
         pTemp = d = MiniIOAlloc(pPage->iWidth); // temporary line
         if (d == NULL)
            return PIL_ERROR_MEMORY;
         pPage->iOffset = 0;
         y = 0;
         x = pPage->iWidth;
         while (y < pPage->iHeight && s < sEnd)
            {
            c = *s++; // get the count
            if (c >=128) // negative = repeating byte
               {
               c &= 0x7f;
               c++;
               p = *s++; // pixel to repeat
               while (c > x && s < sEnd)
                  {
                  memset(d, p, x);
                  d += x;
                  c -= (unsigned char)x;
                  y++;
                  x = pPage->iWidth;
                  PILConvertTargaStrange(pTemp, pPage->iWidth);
                  indexbuf[y] = (int)irlcptr; /* Pointer to the start of this line */
// DEBUG
//                  irlcptr = PILEncodeLine(pPage->iWidth, irlcptr, pTemp); /* Encode this line */
                  d = pTemp;
                  }
               memset(d, p, c);
               x -= c;
               d += c;
               }
            else // raw data
               {
               c++;
               while (c > x && s < sEnd)
                  {
                  memcpy(d, s, x);
                  d += x;
                  s += x;
                  c -= (unsigned char)x;
                  y++;
                  x = pPage->iWidth;
                  PILConvertTargaStrange(pTemp, pPage->iWidth);
                  indexbuf[y] = (int)irlcptr; /* Pointer to the start of this line */
// DEBUG
//                  irlcptr = PILEncodeLine(pPage->iWidth, irlcptr, pTemp); /* Encode this line */
                  d = pTemp;
                  }
               memcpy(d, s, c);
               x -= c;
               d += c;
               s += c;
               }
            }
         MiniIOFree(pTemp);
         if (pPage->cFlags & PIL_PAGEFLAGS_BOTTOMUP)
            {
            mPILFlipv(pPage); /* Flip it vertically since BMP files are sometimes upside-down */
            pPage->cFlags &= ~PIL_PAGEFLAGS_BOTTOMUP;
            pPage->cFlags |= PIL_PAGEFLAGS_TOPDOWN;
            }
         if (pPage->cPhotometric == PIL_PHOTOMETRIC_WHITEISZERO)
            {
            PILInvert(pPage); // invert the colors
            pPage->cPhotometric = PIL_PHOTOMETRIC_BLACKISZERO;
            }
         pPage->iDataSize = (int)irlcptr - (int)pPage->pData;
         break;
      default:
         return PIL_ERROR_UNSUPPORTED;
      }

   return 0;
} /* PILGetTargaC() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILGetTarga()                                              *
 *                                                                          *
 *  PURPOSE    : Read an uncompressed Targa image.                          *
 *                                                                          *
 ****************************************************************************/
static int PILGetTarga(PIL_PAGE *pPage, PIL_FILE *pFile)
{

   if (pPage->cBitsperpixel == 16) // special case for RGB555->RGB565 files
      {
      unsigned short us, *s, *d;
      int x, y;
      pPage->pData = MiniIOAlloc(pPage->iPitch * pPage->iHeight);
      if (pPage->pData == NULL)
         return PIL_ERROR_MEMORY;
      pPage->cState = PIL_PAGE_STATE_LOADED;
      s = (unsigned short *)&pFile->pData[pPage->iOffset];
      d = (unsigned short *)pPage->pData;
      for (y=0; y<pPage->iHeight; y++)
         {
         for (x=0; x<pPage->iWidth; x++)
            {
            us = s[x];
            d[x] = ((us & 0x7fe0) << 1) | (us & 0x1f);
            }
         s += pPage->iPitch/2;
         d += pPage->iPitch/2;
         }
      pPage->iOffset = 0;
      }
   if (pPage->cBitsperpixel == 32) // special case for 32-bit images
      {
      unsigned char *s, *d;
      int y;
      pPage->pData = MiniIOAlloc(pPage->iPitch * pPage->iHeight);
      if (pPage->pData == NULL)
         return PIL_ERROR_MEMORY;
      pPage->cState = PIL_PAGE_STATE_LOADED;
      s = (unsigned char *)&pFile->pData[pPage->iOffset];
      d = (unsigned char *)pPage->pData;
      for (y=0; y<pPage->iHeight; y++)
         {
         memcpy(d, s, pPage->iWidth*4);
         s += pPage->iWidth*4;
         d += pPage->iPitch;
         }
      pPage->iOffset = 0;
      }
   if (pPage->cBitsperpixel == 1) // need to convert to true 1bpp image
      {
      unsigned char c, *s, *d;
      int x, y;
      pPage->pData = MiniIOAlloc(pPage->iPitch * pPage->iHeight);
      if (pPage->pData == NULL)
         return PIL_ERROR_MEMORY;
      pPage->cState = PIL_PAGE_STATE_LOADED;
      s = (unsigned char *)&pFile->pData[pPage->iOffset];
      d = (unsigned char *)pPage->pData;
      for (y=0; y<pPage->iHeight; y++)
         {
         for (x=0; x<pPage->iWidth; x+=8)
            {
            c = 0;
            if (s[x])
               c |= 0x80;
            if (s[x+1])
               c |= 0x40;
            if (s[x+2])
               c |= 0x20;
            if (s[x+3])
               c |= 0x10;
            if (s[x+4])
               c |= 0x8;
            if (s[x+5])
               c |= 0x4;
            if (s[x+6])
               c |= 0x2;
            if (s[x+7])
               c |= 0x1;
            d[x/8] = c;
            }
         s += pPage->iWidth;
         d += pPage->iPitch;
         }
      pPage->iOffset = 0;
      }
   return 0;

} /* PILGetTarga() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILRead()                                                  *
 *                                                                          *
 *  PURPOSE    : Set up the page structure to read the requested file page. *
 *                                                                          
 ****************************************************************************/
int mPILRead(PIL_FILE *pFile, PIL_PAGE *pPage, int iRequestedPage, int iOptions)
{
unsigned char c, *pData=NULL, *p, *d, *pOldPage;
int i, j, iLen, codestart, iMap, lsize, iErr, iOffset, iDataLen;
int iPixelType;
int iMarker;
BOOL bDone;

   if (pFile->cState != PIL_FILE_STATE_OPEN && pFile->cState != PIL_FILE_STATE_LOADED) // can't work with a closed file
      return PIL_ERROR_INVPARAM; // can't read or load it
      
   iErr = 0;
   if (!(iOptions & PIL_CONVERT_NOALLOC))
      pPage->pData = NULL; // make sure any old junk is cleared out
   pPage->plStrips = NULL;
   pPage->plStripSize = NULL;
   pPage->pPalette = NULL;
   pPage->lUser = 0;
   pPage->szComment[0] = '\0';
   pPage->szInfo1[0] = '\0';
   pPage->szInfo2[0] = '\0';
   // Assume no EXIF data
   pPage->iExposure = -1;
   pPage->iExposureProgram = -1;
   pPage->iFStop = -1;
   pPage->iShutter = -1;
   pPage->iMetering = -1;
   pPage->iISO = -1;
   pPage->iWhiteBalance = -1;
   pPage->iFocalLength = -1;
   pPage->iOriginalHeight = -1;
   pPage->iOriginalWidth = -1;
   pPage->iOrientation = -1; // default to normal orientation

   if (iRequestedPage > pFile->iPageTotal-1)
      {
      iErr = PIL_ERROR_PAGENF;
      goto pilreadz;
      }
//   if (pFile->cFileType == PIL_FILE_JFIF && (iOptions & PIL_CONVERT_THUMBNAIL)) // see if we can extract the JPEG EXIF thumbnail quicker
//      {
//      i = PILReadEXIFThumb(pFile, pPage, iOptions); // try to read just the thumbnail image (it will show up as a small TIFF file)
//      return i;
//      }

   if (pFile->cState == PIL_FILE_STATE_OPEN)
      {
      if (pFile->cFileType == PIL_FILE_JFIF || pFile->cFileType == PIL_FILE_MPEG) // special case for JPEG+MPEG files (for now)
         { // for these type of files, just get it started by reading a "buffer's" worth
         i = PIL_BUFFER_SIZE;
         iOffset = 0; // start at the beginning of the file
         }
      else
         {
         if (pFile->iPageTotal > 1 && pFile->pPageList)
            {
            i = pFile->pPageList[iRequestedPage+1] - pFile->pPageList[iRequestedPage];
            iOffset = pFile->pPageList[iRequestedPage];
            }
         else
            {
            iOffset = 0; // start at the beginning of the file
            i = pFile->iFileSize; // load the whole file
            }
         }
      if (!(iOptions & PIL_CONVERT_NOALLOC))
         pPage->pData = MiniIOAlloc(i);
      pPage->iFilePos = i;
      pPage->iHandle = pFile->iFile;
      pPage->cState = PIL_PAGE_STATE_OPEN;
      MiniIOSeek(pPage->iHandle, iOffset, 0);
      MiniIORead(pPage->iHandle, pPage->pData, i);
      pData = pPage->pData;
      }
   else if (pFile->cState == PIL_FILE_STATE_LOADED)
      {
      pPage->pData = pData = pFile->pData; // we already have the data
      pPage->cState = PIL_PAGE_STATE_LOADED;
      }

   switch (pFile->cFileType)
      {
      case PIL_FILE_PNG:
         pPage->iStripCount = 0; // no strips
         pPage->cCompression = PIL_COMP_FLATE;
         if (pFile->pData) // data already loaded
            {
            pPage->pData = pFile->pData;
            }
         else
            {
            pPage->pData = MiniIOAlloc(pFile->iFileSize);
            if (pPage->pData)
               {
               MiniIOSeek(pFile->iFile, 0, 0); // make sure we seek to the start of the file
               MiniIORead(pFile->iFile, pPage->pData, pFile->iFileSize);
               }
            else
               {
               iErr = PIL_ERROR_MEMORY;
               goto pilreadz;
               }
            }
         // IHDR chunk must be first (except in the case of Apple PNGs)
         iOffset = 8;
         iLen = MOTOLONG(&pPage->pData[iOffset]); // get the length of the IHDR chunk
         while (iOffset < pFile->iFileSize && MOTOLONG(&pPage->pData[iOffset+4]) != 0x49484452 /*'IHDR'*/) // must be Apple PNG
            {
            iOffset += (iLen + 0xc);
            }
         if (MOTOLONG(&pPage->pData[iOffset+4]) != 0x49484452/*'IHDR'*/) // not a valid PNG
            {
            MiniIOFree(pPage->pData);
            return PIL_ERROR_BADHEADER;
            }
         pPage->iOffset = 0;
         pPage->iDataSize = pFile->iFileSize;
         pPage->iWidth = MOTOLONG(&pPage->pData[iOffset+8]);
         pPage->iHeight = MOTOLONG(&pPage->pData[iOffset+0xc]);
         i = pPage->pData[iOffset+0x10]; // bits per pixel
         iPixelType = pPage->pData[iOffset+0x11]; // pixel type
         if (pPage->pData[iOffset+0x14]) // interlace flag - we don't handle it yet
            {
            MiniIOFree(pPage->pData);
            return PIL_ERROR_UNSUPPORTED;
            }
         switch (iPixelType)
            {
            case 0: // grayscale
               pPage->cBitsperpixel = (char)i;
               break;
            case 2: // each pixel is an RGB triple
               pPage->cBitsperpixel = (char)i*3;
               break;
            case 3: // palette image
               pPage->cBitsperpixel = (char)i;
               break;
            case 4: // grayscale + alpha channel
               pPage->cBitsperpixel = (char)i*2;
               break;
            case 6: // RGB + alpha channel
               pPage->cBitsperpixel = (char)i*4;
               break;
            }
         // walk through chunks until we find a data chunk
         iOffset = 8;
         bDone = FALSE;
         pPage->pPalette = NULL;
         while (!bDone && iOffset < pFile->iFileSize-8)
            {
            iLen = MOTOLONG(&pPage->pData[iOffset]); // chunk length
            iMarker = MOTOLONG(&pPage->pData[iOffset+4]);
            iOffset += 8; // point to the data
            switch (iMarker)
               {
               case 0x67414d41 /*'gAMA'*/:
                  break;
               case 0x504c5445 /*'PLTE'*/: // palette colors
                  pPage->pPalette = MiniIOAlloc(768+256); // space for colors and transparency
                  memset(&pPage->pPalette[768], 0xff, 256); // assume all colors are opaque unless specified
                  memcpy(pPage->pPalette, &pPage->pData[iOffset], iLen);
                  // swap R/B order
                  for (i=0; i<iLen; i+=3)
                     {
                     unsigned char t;
                     t = pPage->pPalette[i+2];
                     pPage->pPalette[i+2] = pPage->pPalette[i];
                     pPage->pPalette[i] = t;
                     }
                  break;
               case 0x74524e53/*'tRNS'*/: // transparency info
				   // DEBUG - we currently don't support single color transparency for grayscale/fullcolor images
				  if (iPixelType == 3 && pPage->pPalette) // if indexed color and a palette has been defined
				  {
					memcpy(&pPage->pPalette[768], &pPage->pData[iOffset], iLen);
				  }
                  break;
               case 0x49444154 /*'IDAT'*/: // image data block
                  bDone = TRUE;
                  break;
//               case 'iTXt':
               case 0x74455874/*'tEXt'*/:
//               case 'zTXt':
                  if (iLen < 128)
                     memcpy(pPage->szComment, &pPage->pData[iOffset], iLen);
                  else
                     {
                     memcpy(pPage->szComment, &pPage->pData[iOffset], 127);
                     pPage->szComment[127] = '\0';
                     }
                  break;
               }
            iOffset += iLen + 4; // skip data + CRC
            }
         // if we're 2/4/8 bpp and no palete defined, then it's a grayscale image
         if (pPage->pPalette == NULL && (pPage->cBitsperpixel ==2 || pPage->cBitsperpixel == 4 || pPage->cBitsperpixel == 8))
            {
            pPage->pPalette = MiniIOAlloc(768+256); // space for colors and transparency
            memset(&pPage->pPalette[768], 0xff, 256); // assume all colors are opaque unless specified
            if (pPage->cBitsperpixel == 4)
               {
               for (i=0; i<16; i++)
                  {
                  pPage->pPalette[i*3+0] = (unsigned char)i*0x11;
                  pPage->pPalette[i*3+1] = (unsigned char)i*0x11;
                  pPage->pPalette[i*3+2] = (unsigned char)i*0x11;
                  }
               }
            else if (pPage->cBitsperpixel == 8)
               {
               for (i=0; i<256; i++)
                  {
                  pPage->pPalette[i*3+0] = (unsigned char)i;
                  pPage->pPalette[i*3+1] = (unsigned char)i;
                  pPage->pPalette[i*3+2] = (unsigned char)i;
                  }
               }
            else // 2bpp
               {
               for (i=0; i<4; i++)
                  {
                  pPage->pPalette[i*3+0] = (unsigned char)i*0x55;
                  pPage->pPalette[i*3+1] = (unsigned char)i*0x55;
                  pPage->pPalette[i*3+2] = (unsigned char)i*0x55;
                  }
               }
            }
         pPage->cState = PIL_PAGE_STATE_LOADED;
         pPage->cFlags = PIL_PAGEFLAGS_TOPDOWN;
         break;

      case PIL_FILE_GIF:
         pPage->iStripCount = 0; // no strips
         iOffset = pPage->iOffset = 0;
         if (pFile->iPageTotal > 1)
            {
            if (pFile->cState == PIL_FILE_STATE_LOADED)
               pPage->iOffset = pFile->pPageList[iRequestedPage];
            pPage->iDataSize = pFile->pPageList[iRequestedPage+1] - pFile->pPageList[iRequestedPage];
            }
         else
            {
            pPage->iDataSize = pFile->iFileSize;
            }
         pPage->cCompression = PIL_COMP_GIF;
         pPage->cFlags = PIL_PAGEFLAGS_TOPDOWN;
         if (pFile->cState == PIL_FILE_STATE_LOADED)
            p = pOldPage = &pFile->pData[pPage->iOffset];
         else
            p = pOldPage = &pPage->pData[pPage->iOffset];
         if (iRequestedPage == 0)
            {
            pPage->iPageWidth = INTELSHORT(&p[6]);
            pPage->iPageHeight = INTELSHORT(&p[8]);
            pPage->cBitsperpixel = (p[10] & 7) + 1;
            pPage->cBackground = p[11]; // background color
            iOffset = 13;
            if (p[10] & 0x80) // global color table?
               {
               if (pPage->cBitsperpixel == 1)
                  {
                  if (p[13])
                     pPage->cPhotometric = PIL_PHOTOMETRIC_WHITEISZERO;
                  else
                     pPage->cPhotometric = PIL_PHOTOMETRIC_BLACKISZERO;
                  iOffset += 6; /* Skip the color table */
                  }
               else
                  {
                  pPage->pPalette = MiniIOAlloc(768); /* Allocate fixed size color palette */
                  memcpy(pPage->pPalette, &p[iOffset], 3*(1 << pPage->cBitsperpixel));
                  PILFixGifRGB(pPage->pPalette); /* Fix RGB byte order */
                  iOffset += 3 * (1 << pPage->cBitsperpixel);
                  }
               }
            }
         else
            {
            // need a re-design to not have to do this
             // get the global color table again
            if (pPage->cBitsperpixel != 1)
               {
               pPage->pPalette = MiniIOAlloc(768); /* Allocate fixed size color palette */
               if (pFile->cState == PIL_FILE_STATE_LOADED)
                  {
                  memcpy(pPage->pPalette, &pFile->pData[13], 3*(1 << pPage->cBitsperpixel));
                  }
               else
                  {
                  MiniIOSeek(pPage->iHandle, 13, 0);
                  MiniIORead(pPage->iHandle, pPage->pPalette, 3*(1<<pPage->cBitsperpixel));
                  }
               PILFixGifRGB(pPage->pPalette); /* Fix RGB byte order */
               }
            }
         while (p[iOffset] != ',') /* Wait for image separator */
            {
            if (p[iOffset] == '!') /* Extension block */
               {
               iOffset++;
               switch(p[iOffset++]) /* Block type */
                  {
                  case 0xf9: /* Graphic extension */
                     if (p[iOffset] == 4) // correct length
                        {
                        pPage->cGIFBits = p[iOffset+1]; // packed fields
                        pPage->iGIFDelay = INTELSHORT(&p[iOffset+2]);
                        pPage->cTransparent = p[iOffset+4]; // transparent color index
                        iOffset += 6;
                        }
//                     else   // error
                     break;
                  case 0xff: /* App extension */
                     c = 1;
                     j = 0;
                     while (c) /* Skip all data sub-blocks */
                        {
                        c = p[iOffset++]; /* Block length */
                        if (j == 0) // use only first block
                           {
                           j = c;
                           if (j > 127)   // max comment length = 127
                              j = 127;
                           memcpy(pPage->szInfo1, &p[iOffset], j);
                           pPage->szInfo1[j] = '\0';
                           j = 1;
                           }
                        iOffset += (int)c; /* Skip this sub-block */
                        }
                     break;
                  case 0x01: /* Text extension */
                     c = 1;
                     j = 0;
                     while (c) /* Skip all data sub-blocks */
                        {
                        c = p[iOffset++]; /* Block length */
                        if (j == 0) // use only first block
                           {
                           j = c;
                           if (j > 127)   // max comment length = 127
                              j = 127;
                           memcpy(pPage->szInfo2, &p[iOffset], j);
                           pPage->szInfo2[j] = '\0';
                           j = 1;
                           }
                        iOffset += (int)c; /* Skip this sub-block */
                        }
                     break;
                  case 0xfe: /* Comment */
                     c = 1;
                     j = 0;
                     while (c) /* Skip all data sub-blocks */
                        {
                        c = p[iOffset++]; /* Block length */
                        if (j == 0) // use only first block
                           {
                           j = c;
                           if (j > 127)   // max comment length = 127
                              j = 127;
                           memcpy(pPage->szComment, &p[iOffset], j);
                           pPage->szComment[j] = '\0';
                           j = 1;
                           }
                        iOffset += (int)c; /* Skip this sub-block */
                        }
                     break;
                  default:
                     iErr = PIL_ERROR_BADHEADER; /* Bad header info */
                     if (pPage->cBitsperpixel != 1)
                        MiniIOFree(pPage->pPalette);
                     goto pilreadz;
                  } /* switch */
               }
            else // invalid byte, stop decoding
               {
               iErr = PIL_ERROR_BADHEADER; /* Bad header info */
               if (pPage->cBitsperpixel != 1)
                  MiniIOFree(pPage->pPalette);
               goto pilreadz;
               }
            } /* while */
         if (p[iOffset] == ',')
            iOffset++;
         pPage->iX = INTELSHORT(&p[iOffset]);
         pPage->iY = INTELSHORT(&p[iOffset+2]);
         pPage->iWidth = INTELSHORT(&p[iOffset+4]);
         pPage->iHeight = INTELSHORT(&p[iOffset+6]);
         iOffset += 8;

   /* Image descriptor
     7 6 5 4 3 2 1 0    M=0 - use global color map, ignore pixel
     M I 0 0 0 pixel    M=1 - local color map follows, use pixel
                        I=0 - Image in sequential order
                        I=1 - Image in interlaced order
                        pixel+1 = # bits per pixel for this image
   */
         iMap = p[iOffset++];
         if (iMap & 0x80) // local color table?
            {
            if (pPage->pPalette == NULL) // no global table, need to allocate it
               pPage->pPalette = MiniIOAlloc(768);
            i = 3 * (1 << ((iMap & 7)+1)); // get the size of the color table specified
            if (((iMap & 7)+1) > pPage->cBitsperpixel) // more colors than we thought, use this info
               pPage->cBitsperpixel = (iMap & 7) + 1;
            memcpy(pPage->pPalette, &p[iOffset], i);
            PILFixGifRGB(pPage->pPalette); /* Fix RGB byte order */
            iOffset += i;
            }
         codestart = p[iOffset++]; /* initial code size */
         /* Since GIF can be 1-8 bpp, we only allow 1,4,8 */
         pPage->cBitsperpixel = cGIFBits[pPage->cBitsperpixel-1];
         pPage->pData = MiniIOAlloc(pPage->iDataSize+8); /* Allocate a large enough buffer */
         if (pPage->pData == NULL)
            {
            if (pPage->pPalette)
               MiniIOFree(pPage->pPalette);
            iErr = PIL_ERROR_MEMORY;
            goto pilreadz;
            }
         d = pPage->pData;
         *d++ = (unsigned char)iMap; /* Store the map attributes */
         *d++ = (unsigned char)codestart; /* And the starting code */
         iDataLen = 2; /* Length of GIF data */
         c = p[iOffset++]; /* This block length */
         while (c && (iOffset+c) <= pPage->iDataSize) /* Collect all of the data packets together */
            {
            memcpy(d, &p[iOffset], c);
            iOffset += c;
            d += c;
            iDataLen += c; /* Overall data length */
            c = p[iOffset++];
            }
         if ((iOffset+c) > pPage->iDataSize) /* Error, we went beyond end of buffer */
            {
            MiniIOFree(pPage->pData);
            if (pPage->pPalette)
               MiniIOFree(pPage->pPalette);
            iErr = PIL_ERROR_DECOMP;
            goto pilreadz;
            }
         pPage->iDataSize = iDataLen; /* Total bytes of GIF data */
         pPage->cState = PIL_PAGE_STATE_LOADED; // since we have to repack the data
         pPage->iOffset = 0; // since we repacked the data
         pPage->iPitch = mPILCalcSize(pPage->iWidth, pPage->cBitsperpixel);
         if (pFile->cState != PIL_FILE_STATE_LOADED)
            MiniIOFree(pData); // free the temp data buffer (had raw GIF page)
         break;

      case PIL_FILE_JFIF:
         pPage->iStripCount = 0; // no strips
         pPage->cCompression = PIL_COMP_JPEG;
         i = 2; /* Start at offset of first marker */
         iMarker = 0; /* Search for SOF (start of frame) marker */
         while (i < PIL_BUFFER_SIZE) //pFile->iFileSize)
            {
            iMarker = MOTOSHORT(&pData[i]) & 0xfffc;
            i += 2;
            if (iMarker < 0xff00) // invalid marker, could be generated by "Arles Image Web Page Creator" or Accusoft
               continue; // skip 2 bytes and try to resync
            if (iMarker == 0xffc0)
               break;
            i += MOTOSHORT(&pData[i]); /* Skip to next marker */
            }
         if (iMarker != 0xffc0)
            iErr = PIL_ERROR_BADHEADER;
         else
            {
            pPage->iHeight = MOTOSHORT(&pData[i+3]);
            pPage->iWidth = MOTOSHORT(&pData[i+5]);
            if (pData[i+7] == 1) /* number of components */
               {
               pPage->cBitsperpixel = 8; /* grayscale */
               // Create a gray palette
               pPage->pPalette = PILGrayPalette(8);
               }
            else
               pPage->cBitsperpixel = 24;
            pPage->iDataSize = pFile->iFileSize;
            pPage->iOffset = 0;  /* Start of data */
            pPage->cFlags = PIL_PAGEFLAGS_TOPDOWN;
            }
         break;

      case PIL_FILE_OS2BMP:
         pPage->iStripCount = 0; // no strips
         if (pData[30] == 1 || pData[30] == 2 || pData[30] == 4) // if biCompression is non-zero (1=4bit rle, 2=8bit rle)
            pPage->cCompression = PIL_COMP_WINRLE; // windows run-length
         else
            pPage->cCompression = PIL_COMP_NONE;
         if (pData[14] == 12) // version 1.2
            {

            pPage->iWidth = INTELSHORT(&pData[18]);
            pPage->iHeight = INTELSHORT(&pData[20]);
            pPage->cBitsperpixel = pData[22]; /* Number of planes */
            pPage->cBitsperpixel *= pData[24]; /* Number of bits */
            }
         else
            {
            pPage->cBitsperpixel = pData[28]; /* Number of bits per plane */
            pPage->cBitsperpixel *= pData[26]; /* Number of planes */
            pPage->iWidth = INTELSHORT(&pData[18]);
            pPage->iHeight = INTELSHORT(&pData[22]);
            }
         if (pPage->cCompression == PIL_COMP_WINRLE && pPage->cBitsperpixel != 4 && pPage->cBitsperpixel != 8 && pPage->cBitsperpixel != 24) // we only support 4, 8 and 24-bit RLE
            {
            iErr = PIL_ERROR_UNSUPPORTED;
            goto pilreadz;
            }
         lsize = INTELLONG(&pData[10]); /* Offset to data bits */
         pPage->iDataSize = pFile->iFileSize - lsize;
         pPage->iPitch = mPILCalcSize(pPage->iWidth, pPage->cBitsperpixel);
         if (pPage->cCompression == PIL_COMP_NONE && pFile->iFileSize < (pPage->iPitch * pPage->iHeight)) // incomplete file
            {
            iErr = PIL_ERROR_IO;
            goto pilreadz;
            }
         pPage->iOffset = 0;   // starting point of data
         pPage->pData = &pFile->pData[lsize]; // pointer to bits
         pPage->cFlags = PIL_PAGEFLAGS_BOTTOMUP;
         if (pPage->cBitsperpixel == 1)
            {
            pPage->cPhotometric = pData[26]; // get inversion flag
            }
         if (pPage->cBitsperpixel == 4 || pPage->cBitsperpixel == 8)
            { /* Get color palette */
            pPage->pPalette = MiniIOAlloc(768);
            p = pPage->pPalette;
            for (i=0; i<(1<<pPage->cBitsperpixel); i++)
               {
               *p++ = pData[26+i*3];
               *p++ = pData[27+i*3];
               *p++ = pData[28+i*3];
               }
            }
         pPage->cFlags = PIL_PAGEFLAGS_DWORD | PIL_PAGEFLAGS_BOTTOMUP;
         break;
         
      case PIL_FILE_TARGA:
         pPage->pData = pFile->pData;
         pPage->iWidth = INTELSHORT(&pData[12]);
         pPage->iHeight = INTELSHORT(&pData[14]);
         pPage->cBitsperpixel = pData[16];
         pPage->iStripCount = 0;
         i = pData[16]; // bits per pixel
         lsize = pPage->iPitch = PILCalcBSize(pPage->iWidth, i);
         c = pData[2]; // get the file type
         if (c == 3 || c == 11) // monochrome
            {
            pPage->cBitsperpixel = 1;
            pPage->cPhotometric = PIL_PHOTOMETRIC_BLACKISZERO;
            }
         j = 0; // color map length
         if (pPage->cBitsperpixel == 4 || pPage->cBitsperpixel == 8)
            {
            pPage->pPalette = MiniIOAlloc(768);
            PILGetTargaMap(pPage, pData);
            j = INTELSHORT(&pData[5]); // length
            j *= pData[7]; // bits per entry
            j /= 8;
            }
         if (pPage->cBitsperpixel == 1)
            pPage->iOffset = pData[0] + 18;
         else
            pPage->iOffset = 0;
         pPage->pData = &pPage->pData[j + pData[0] + 18];
         pPage->cCompression = PIL_COMP_NONE;
         pPage->iDataSize = lsize * pPage->iHeight;
         if (pFile->pData[17] & 32)
            pPage->cFlags = PIL_PAGEFLAGS_TOPDOWN;
         else
            pPage->cFlags = PIL_PAGEFLAGS_BOTTOMUP;
         if (c < 9) // Uncompressed
            iErr = PILGetTarga(pPage, pFile);
         else       // Compressed
            iErr = PILGetTargaC(pPage, pFile);
         pPage->iXres = pPage->iYres = 0; // no resolution info
         if (iErr && pPage->pPalette)
            {
            MiniIOFree(pPage->pPalette);
            pPage->pPalette = NULL;
            }
         break;

      case PIL_FILE_WINBMP:
         pPage->iStripCount = 0; // no strips
         pPage->cBitsperpixel = pData[28]; /* Number of bits per plane */
         pPage->cBitsperpixel *= pData[26]; /* Number of planes */
         if (pData[30] && (pPage->cBitsperpixel == 4 || pPage->cBitsperpixel == 8)) // if biCompression is non-zero (2=4bit rle, 1=8bit rle,4=24bit rle)
            pPage->cCompression = PIL_COMP_WINRLE; // windows run-length
         else
            pPage->cCompression = PIL_COMP_NONE;
         pPage->iWidth = INTELSHORT(&pData[18]);
         pPage->iHeight = INTELSHORT(&pData[22]);
//         if (pPage->cCompression == PIL_COMP_WINRLE && pPage->cBitsperpixel != 4 && pPage->cBitsperpixel != 8) // we only support 4 and 8-bit RLE
//            {
//            iErr = PIL_ERROR_UNSUPPORTED;
//            goto pilreadz;
//            }
         lsize = INTELLONG(&pData[10]); /* Offset to data bits */
         pPage->iDataSize = pFile->iFileSize - lsize;
         pPage->iPitch = mPILCalcSize(pPage->iWidth, pPage->cBitsperpixel);
         if (pPage->cCompression == PIL_COMP_NONE && pFile->iFileSize < (pPage->iPitch * pPage->iHeight)) // incomplete file
            {
            iErr = PIL_ERROR_IO;
            goto pilreadz;
            }
         pPage->iOffset = 0;   // starting point of data
         if (pFile->cState == PIL_FILE_STATE_LOADED)
            pPage->pData = &pFile->pData[lsize]; // pointer to bits
         else // we have to read the data
            {
            // see if we have the whole thing in memory already
            if (pPage->iDataSize <= PIL_BUFFER_SIZE) // we have the whole thing in memory
               {
               pPage->iOffset = lsize; // start of data
               pPage->pData = pData;
               pPage->cState = PIL_PAGE_STATE_LOADED;
               }
            }
         if (pPage->cBitsperpixel == 1)
            {
            pPage->cPhotometric = pData[54]; // get inversion flag
            }
         if (pPage->cBitsperpixel == 4 || pPage->cBitsperpixel == 8)
            { /* Get color palette */
            pPage->pPalette = MiniIOAlloc(768);
            p = pPage->pPalette;
            for (i=0; i<(1<<pPage->cBitsperpixel); i++)
               {
               *p++ = pData[54+i*4];
               *p++ = pData[55+i*4];
               *p++ = pData[56+i*4];
               }
            }
         pPage->cFlags = PIL_PAGEFLAGS_DWORD | PIL_PAGEFLAGS_BOTTOMUP;
         if (pPage->cBitsperpixel == 16 && !(pData[30] == 3 && pData[58] == 0xe0)) // special case for RGB555->RGB565 files
            {
            unsigned short us, *s, *d;
            int x, y;
            pPage->pData = MiniIOAlloc(pPage->iPitch * pPage->iHeight);
            if (pPage->pData == NULL)
               {
               iErr = PIL_ERROR_MEMORY;
               goto pilreadz;
               }
            pPage->cState = PIL_PAGE_STATE_LOADED;
            if (pFile->cState == PIL_FILE_STATE_LOADED)
               s = (unsigned short *)&pFile->pData[lsize];
            else
               s = (unsigned short *)&pData[lsize];
            d = (unsigned short *)pPage->pData;
            for (y=0; y<pPage->iHeight; y++)
               {
               for (x=0; x<pPage->iWidth; x++)
                  {
                  us = s[x];
                  d[x] = ((us & 0x7fe0) << 1) | (us & 0x1f);
                  }
               s += pPage->iPitch/2;
               d += pPage->iPitch/2;
               }
            pPage->iOffset = 0;
            if (pFile->cState != PIL_FILE_STATE_LOADED)
               MiniIOFree(pData); // free temp buffer
            }
         if (pPage->cBitsperpixel == 32) // special case for 32-bit bitmaps, fix it
            { // DEBUG
            unsigned char *s, *d;
            int y;
            pPage->iPitch = mPILCalcSize(pPage->iWidth, pPage->cBitsperpixel);
            pPage->pData = MiniIOAlloc(pPage->iPitch * pPage->iHeight);
            pPage->iDataSize = pPage->iPitch * pPage->iHeight;
            if (pPage->pData == NULL)
               {
               iErr = PIL_ERROR_MEMORY;
               goto pilreadz;
               }
            pPage->cState = PIL_PAGE_STATE_LOADED;
            s = (unsigned char *)&pFile->pData[lsize];
            d = (unsigned char *)&pPage->pData[0];
            for (y=0; y<pPage->iHeight; y++)
               {
               memcpy(d, s, pPage->iWidth*4);
               s += pPage->iWidth*4;
               d += pPage->iPitch;
               }
            pPage->iOffset = 0;
            }
         if (pPage->cBitsperpixel == 2) // special case of a ".2BP", fix it
            {
            unsigned char c, *s, *d;
            int x, y, iDelta;
            iDelta = mPILCalcSize(pPage->iWidth, 2);
            pPage->cBitsperpixel = 4; // convert to 4bpp gray
            pPage->iPitch = mPILCalcSize(pPage->iWidth, 4);
            pPage->pData = MiniIOAlloc(pPage->iPitch * pPage->iHeight);
            if (pPage->pData == NULL)
               {
               iErr = PIL_ERROR_MEMORY;
               goto pilreadz;
               }
            pPage->cState = PIL_PAGE_STATE_LOADED;
            s = &pFile->pData[lsize];
            d = pPage->pData;
            for (y=0; y<pPage->iHeight; y++)
               {
               for (x=0; x<(pPage->iWidth+3)/4; x++)
                  {
                  c = s[x];   // get 4 pixels
                  d[x*2] = ((c & 0xc0)>>2) | ((c & 0x30) >> 4);
                  d[x*2+1] = ((c & 0xc)<<2) | (c & 0x3);
                  }
               s += iDelta;
               d += pPage->iPitch;
               }
            // now create a simple gray palette
            pPage->pPalette = MiniIOAlloc(768);
            memcpy(pPage->pPalette, c2bpppal, 12);
            pPage->iOffset = 0;
            }
         break;
      default:
         iErr = PIL_ERROR_UNKNOWN;
         break;
      }
pilreadz:
   if (iErr == 0)
      {
      pFile->iPage = iRequestedPage; /* Set page number to one loaded */
      }
   return iErr;
} /* mPILRead() */
#endif // PIL_IMAGE_READ

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILClose()                                                 *
 *                                                                          *
 *  PURPOSE    : Close an image file.                                       *
 *                                                                          *
 ****************************************************************************/
int mPILClose(PIL_FILE *pFile)
{

   if (pFile->cState == PIL_FILE_STATE_OPEN)
      {
      MiniIOClose(pFile->iFile);
      }
   if (pFile->pJPEG)
      {
      PILFreeHuffTables(pFile->pJPEG);
      MiniIOFree(pFile->pJPEG);
      pFile->pJPEG = NULL;
      }
   if (pFile->pSoundList)
      {
      MiniIOFree(pFile->pSoundList);
      pFile->pSoundList = NULL;
      }
   if (pFile->pKeyFlags)
      {
      MiniIOFree(pFile->pKeyFlags);
      pFile->pKeyFlags = NULL;
      }
   if (pFile->pSoundLens)
      {
      MiniIOFree(pFile->pSoundLens);
      pFile->pSoundLens = NULL;
      }
   if (pFile->pPageList)
      {
      MiniIOFree(pFile->pPageList);
      pFile->pPageList = NULL;
      }
   if (pFile->pPageLens)
      {
      MiniIOFree(pFile->pPageLens);
      pFile->pPageLens = NULL;
      }
   pFile->cState = PIL_FILE_STATE_CLOSED; // reset state flags
   return 0;
} /* mPILClose() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILDraw4(PIL_PAGE *, PIL_VIEW *, BOOL, BOOL)                *
 *                                                                          *
 *  PURPOSE    : Draw a view of an 4bpp SPV object into a bitmap.           *
 *                                                                          *
 ****************************************************************************/
static int PILDraw4(PIL_PAGE *inpage, PIL_VIEW *sv, int bTop)
{
int curx, cury, i, lsize, width;
unsigned long ysum, xsum, iScaleX;
int x, xoff;
unsigned char *s, *d, *temp;

   if (sv->iOrientation == 0 || sv->iOrientation == 180)
      {
      /* Pick the smaller area to draw */
      i = ((inpage->iHeight - sv->iWinY) << 8) / sv->iScaleY;
      if (i < sv->iHeight)
         cury = i;
      else
         cury = sv->iHeight;

      /* Pick the smaller area to draw */
      i = ((inpage->iWidth - sv->iWinX) << 8) / sv->iScaleX;
      if (i < sv->iWidth)
         curx = i;
      else
         curx = sv->iWidth;
      }
   else // rotated 90/270
      {
      /* Pick the smaller area to draw */
      i = ((inpage->iWidth - sv->iWinY) << 8) / sv->iScaleY;
      if (i < sv->iHeight)
         cury = i;
      else
         cury = sv->iHeight;

      /* Pick the smaller area to draw */
      i = ((inpage->iHeight - sv->iWinX) << 8) / sv->iScaleX;
      if (i < sv->iWidth)
         curx = i;
      else
         curx = sv->iWidth;
      }

   lsize = inpage->iPitch;
   width = mPILCalcSize(sv->iWidth, 4);
   if (bTop)
      d = sv->pBitmap; /* Destination pointer */
   else
      d = sv->pBitmap + (sv->iHeight-1) * width; /* Destination pointer */
   iScaleX = sv->iScaleX; /* Speed up access to this value */
   ysum = 0;
   switch (sv->iOrientation)
      {
      case 0:
         while (cury)  /* loop through the whole image */
            { /* Calculate source pointer */
            unsigned char c1, c2, shift;
            s = inpage->pData + inpage->iOffset + (sv->iWinX >> 1) + ((ysum >> 8) + sv->iWinY) * lsize;
            xsum = 0;
            x = curx;
            temp = d;
            while (x)
               {
               shift = 0;
               if (xsum & 0x100) /* If odd pixel */
                  shift = 4;
               c1 = s[xsum >> 9]; /* source pixel */
               c1 <<= shift;
               c1 &= 0xf0; /* Make sure odd pixel is black */
               xsum += iScaleX;
               x--;
               if (!x)
                  {
                  *d++ = c1;
                  break; /* Stop looping */
                  }
               shift = 4;
               if (xsum & 0x100) /* If odd pixel */
                  shift = 0;
               c2 = s[xsum >> 9]; /* get source pixel */
               c2 >>= shift;
               c1 |= (c2 & 0x0f); /* Make sure even pixel is black */
               *d++ = c1;
               xsum += iScaleX;
               x--;
               }
            d = temp; /* Get start of line pointer again */
            if (bTop)
               d += width; /* Move down to next line */
            else
               d -= width; /* Move up to next line */
            ysum += sv->iScaleY;
            cury--;
            }
         break; // 0

      case 90:
         while (cury)  /* loop through the whole image */
            { /* Calculate source pointer */
            unsigned char c1, shift;
            xoff = (sv->iWinY +(ysum>>8))/2 + (inpage->iHeight-1-sv->iWinX) * lsize;
            s = inpage->pData + inpage->iOffset + xoff;
            xsum = 0;
            x = 0;
            temp = d;
            shift = 0;
            if (ysum & 0x100) /* If odd pixel */
               shift = 4;
            if (sv->iWinY & 1)
               shift = 4-shift;
            while (x < curx)
               {
               xoff = (xsum>>8)*lsize;
               c1 = s[-xoff]; /* source pixel */
               c1 <<= shift;
               c1 &= 0xf0; /* Make sure odd pixel is black */
               xsum += iScaleX;
               if (x & 1)
                  *d++ |= (c1 >> 4);
               else
                  *d = c1;
               x++;
               }
            d = temp; /* Get start of line pointer again */
            if (bTop)
               d += width; /* Move down to next line */
            else
               d -= width; /* Move up to next line */
            ysum += sv->iScaleY;
            cury--;
            }
         break; // 90

      case 180:
         while (cury)  /* loop through the whole image */
            { /* Calculate source pointer */
            unsigned char c1, shift;
            xoff = (inpage->iWidth-1-sv->iWinX)/2 + (inpage->iHeight-1-sv->iWinY-(ysum>>8)) * lsize;
            s = inpage->pData + inpage->iOffset + xoff;
            xsum = 0;
            x = 0;
            temp = d;
            while (x < curx)
               {
               shift = 4;
               if (xsum & 0x100) /* If odd pixel */
                  shift = 0;
               c1 = s[-(int)(xsum >> 9)]; /* source pixel */
               c1 <<= shift;
               c1 &= 0xf0; /* Make sure odd pixel is black */
               xsum += iScaleX;
               if (x & 1)
                  *d++ |= (c1 >> 4);
               else
                  *d = c1;
               x++;
               }
            d = temp; /* Get start of line pointer again */
            if (bTop)
               d += width; /* Move down to next line */
            else
               d -= width; /* Move up to next line */
            ysum += sv->iScaleY;
            cury--;
            }
         break; // 180

      case 270:
         while (cury)  /* loop through the whole image */
            { /* Calculate source pointer */
            unsigned char c1, shift;
            xoff = (inpage->iWidth - 1 - sv->iWinY -(ysum>>8))/2 + sv->iWinX * lsize;
            s = inpage->pData + inpage->iOffset + xoff;
            xsum = 0;
            x = 0;
            temp = d;
            shift = 4;
            if (ysum & 0x100) /* If odd pixel */
               shift = 0;
            if (sv->iWinY & 1)
               shift = 4-shift;
            while (x < curx)
               {
               xoff = (xsum>>8)*lsize;
               c1 = s[xoff]; /* source pixel */
               c1 <<= shift;
               c1 &= 0xf0; /* Make sure odd pixel is black */
               xsum += iScaleX;
               if (x & 1)
                  *d++ |= (c1 >> 4);
               else
                  *d = c1;
               x++;
               }
            d = temp; /* Get start of line pointer again */
            if (bTop)
               d += width; /* Move down to next line */
            else
               d -= width; /* Move up to next line */
            ysum += sv->iScaleY;
            cury--;
            }
         break; // 270

      default:
         return PIL_ERROR_INVPARAM;
      }

   return 0; /* No errors */

} /* PILDraw4() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILDraw8(PIL_PAGE *, PIL_VIEW *, BOOL, BOOL)               *
 *                                                                          *
 *  PURPOSE    : Draw a view of an 8bpp SPV object into a bitmap.           *
 *                                                                          *
 ****************************************************************************/
static int PILDraw8(PIL_PAGE *inpage, PIL_VIEW *sv, int bTop)
{
int curx, cury, i, lsize, width;
unsigned long ysum, xsum, iScaleX;
int x;
unsigned char *s, *d, *dataptr;

   if (sv->iOrientation == 0 || sv->iOrientation == 180)
      {
      /* Pick the smaller area to draw */
      i = ((inpage->iHeight - sv->iWinY) << 8) / sv->iScaleY;
      if (i < sv->iHeight)
         cury = i;
      else
         cury = sv->iHeight;

      /* Pick the smaller area to draw */
      i = ((inpage->iWidth - sv->iWinX) << 8) / sv->iScaleX;
      if (i < sv->iWidth)
         curx = i;
      else
         curx = sv->iWidth;
      }
   else // rotated 90/270
      {
      /* Pick the smaller area to draw */
      i = ((inpage->iWidth - sv->iWinY) << 8) / sv->iScaleY;
      if (i < sv->iHeight)
         cury = i;
      else
         cury = sv->iHeight;

      /* Pick the smaller area to draw */
      i = ((inpage->iHeight - sv->iWinX) << 8) / sv->iScaleX;
      if (i < sv->iWidth)
         curx = i;
      else
         curx = sv->iWidth;
      }

//   lsize = (inpage->iWidth + 3) & 0xfffc;
   lsize = inpage->iPitch;
   width = mPILCalcSize(sv->iWidth, 8);
   if (bTop)
      d = (unsigned char *)sv->pBitmap; /* Destination pointer */
   else
      d = (unsigned char *)sv->pBitmap + (sv->iHeight-1)*width; /* Destination pointer */
   
   switch (sv->iOrientation)
      {
      case 0: // normal view
         dataptr = (unsigned char *)&inpage->pData[inpage->iOffset];
         iScaleX = sv->iScaleX; /* Speed up access to this value */
         ysum = 0;
         if (iScaleX == 256 && sv->iScaleY == 256) // 1:1 image, draw it faster way
            {
            s = &dataptr[sv->iWinX + ((ysum >> 8) + sv->iWinY) * lsize];
            while (cury)
               {
               memcpy(d, s, curx);
               if (bTop)
                  d += width;
               else
                  d -= width;
               s += lsize;
               cury--;
               }
            } // 1:1 view
         else
            {
            while (cury)  /* loop through the whole image */
               { /* Calculate source pointer */
               s = &dataptr[sv->iWinX + ((ysum >> 8) + sv->iWinY) * lsize];
               xsum = 0;
               for (x = 0; x < curx; x++)
                  {
                  *d++ = s[xsum >> 8];
                  xsum += iScaleX;
                  }
               d -= curx;  /* Move back over stuff drawn */
               if (bTop)
                  d += width; /* Move down to next line */
               else
                  d -= width; /* Move up to next line */
               ysum += sv->iScaleY;
               cury--;
               }
            } // scaled view
         break; // 0

      case 90:
         dataptr = (unsigned char *)&inpage->pData[inpage->iOffset + inpage->iPitch*(inpage->iHeight-1)];
         iScaleX = sv->iScaleX; /* Speed up access to this value */
         ysum = 0;
         if (iScaleX == 256 && sv->iScaleY == 256) // 1:1 image, draw it faster way
            {
            s = &dataptr[sv->iWinY - (sv->iWinX * lsize)];
            while (cury)
               {
               for (x=0; x<curx; x++)
                  {
                  d[x] = s[-x*lsize];
                  }
               if (bTop)
                  d += width;
               else
                  d -= width;
               s++;
               cury--;
               }
            } // 1:1 view
         else
            {
            while (cury)  /* loop through the whole image */
               { /* Calculate source pointer */
               s = &dataptr[sv->iWinY +(ysum>>8) - (sv->iWinX) * lsize];
               xsum = 0;
               for (x = 0; x < curx; x++)
                  {
                  *d++ = s[-lsize*(xsum >> 8)];
                  xsum += iScaleX;
                  }
               d -= curx;  /* Move back over stuff drawn */
               if (bTop)
                  d += width; /* Move down to next line */
               else
                  d -= width; /* Move up to next line */
               ysum += sv->iScaleY;
               cury--;
               }
            } // scaled view
         break; // 90

      case 180:
         dataptr = (unsigned char *)&inpage->pData[inpage->iOffset + (inpage->iHeight-1)*inpage->iPitch + (inpage->iWidth-1)];
         iScaleX = sv->iScaleX; /* Speed up access to this value */
         ysum = 0;
         if (iScaleX == 256 && sv->iScaleY == 256) // 1:1 image, draw it faster way
            {
            s = &dataptr[-sv->iWinX - (sv->iWinY * lsize)];
            while (cury)
               {
               for (x=0; x<curx; x++)
                  {
                  d[x] = s[-x];
                  }
               if (bTop)
                  d += width;
               else
                  d -= width;
               s -= lsize;
               cury--;
               }
            } // 1:1 view
         else
            {
            while (cury)  /* loop through the whole image */
               { /* Calculate source pointer */
               s = &dataptr[-sv->iWinX - ((ysum >> 8) + sv->iWinY) * lsize];
               xsum = 0;
               for (x = 0; x < curx; x++)
                  {
                  *d++ = s[-(signed int)(xsum >> 8)];
                  xsum += iScaleX;
                  }
               d -= curx;  /* Move back over stuff drawn */
               if (bTop)
                  d += width; /* Move down to next line */
               else
                  d -= width; /* Move up to next line */
               ysum += sv->iScaleY;
               cury--;
               }
            } // scaled view
         break; // 180

      case 270:
         dataptr = (unsigned char *)&inpage->pData[inpage->iOffset + (inpage->iWidth-1)];
         iScaleX = sv->iScaleX; /* Speed up access to this value */
         ysum = 0;
         if (iScaleX == 256 && sv->iScaleY == 256) // 1:1 image, draw it faster way
            {
            s = &dataptr[-sv->iWinY + sv->iWinX * lsize];
            while (cury)
               {
               for (x=0; x<curx; x++)
                  {
                  d[x] = s[x*lsize];
                  }
               if (bTop)
                  d += width;
               else
                  d -= width;
               s--;
               cury--;
               }
            } // 1:1 view
         else
            {
            while (cury)  /* loop through the whole image */
               { /* Calculate source pointer */
               s = &dataptr[-(signed int)(sv->iWinY + (ysum >> 8)) + sv->iWinX * lsize];
               xsum = 0;
               for (x = 0; x < curx; x++)
                  {
                  *d++ = s[(xsum >> 8)*lsize];
                  xsum += iScaleX;
                  }
               d -= curx;  /* Move back over stuff drawn */
               if (bTop)
                  d += width; /* Move down to next line */
               else
                  d -= width; /* Move up to next line */
               ysum += sv->iScaleY;
               cury--;
               }
            } // scaled view
         break; // 270

      default: // invalid angle
         return PIL_ERROR_INVPARAM;
      } // switch

   return 0; /* No errors */

} /* PILDraw8() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILDraw16(PIL_PAGE *, PIL_VIEW *, BOOL, BOOL)              *
 *                                                                          *
 *  PURPOSE    : Draw a view of a 16bpp SPV object into a bitmap.           *
 *                                                                          *
 ****************************************************************************/
static int PILDraw16(PIL_PAGE *inpage, PIL_VIEW *sv, int bTop, unsigned char *pGammaBrightness)
{
int curx, cury, i, lsize, width;
unsigned long ysum, xsum, iScaleX;
int x;
unsigned short *s, *d, *dataptr;
BOOL bSmooth = FALSE; // special anti-alias code
int iShift=0, iApertureX=0, iApertureY=0; // For color averaging
unsigned long xmin=0, xmax=0, ymin=0, ymax=0;
//unsigned short us;
unsigned long ul, ulPixel, ulShiftMask;
int j, k, xoff;

   ulShiftMask = 0x07e0f81f;
   /* Pick the smaller areas to draw so we don't go beyond the edges of the image */
   if (sv->iOrientation == 0 || sv->iOrientation == 180)
      {
      i = ((inpage->iHeight - sv->iWinY) << 8) / sv->iScaleY;
      if (i < sv->iHeight)
         cury = i;
      else
         cury = sv->iHeight;
      i = ((inpage->iWidth - sv->iWinX) << 8) / sv->iScaleX;
      if (i < sv->iWidth)
         curx = i;
      else
         curx = sv->iWidth;
      }
   else
      { // 90 / 270
      i = ((inpage->iWidth - sv->iWinY) << 8) / sv->iScaleY;
      if (i < sv->iHeight)
         cury = i;
      else
         cury = sv->iHeight;

      i = ((inpage->iHeight - sv->iWinX) << 8) / sv->iScaleX;
      if (i < sv->iWidth)
         curx = i;
      else
         curx = sv->iWidth;
      } // switch

//   lsize = (inpage->iWidth + 1) & 0xfffe;
   lsize = inpage->iPitch/2; // get as # of words per line
   width = mPILCalcSize(sv->iWidth, 16);
   width >>= 1; // we are calculating with shorts
   if (bTop)
      d = (unsigned short *)sv->pBitmap; /* Destination pointer */
   else
      d = (unsigned short *)sv->pBitmap + (sv->iHeight-1)*width; /* Destination pointer */

   switch (sv->iOrientation)
      {
      case 0: // normal view
         dataptr = (unsigned short *)&inpage->pData[inpage->iOffset];
         iScaleX = sv->iScaleX; /* Speed up access to this value */
         ysum = 0;
         if (iScaleX == 256 && sv->iScaleY == 256) // 1:1 image, draw it faster way
            {
            s = &dataptr[sv->iWinX + (sv->iWinY * lsize)];
            while (cury)
               {
               memcpy(d, s, curx*sizeof(*d));
               if (bTop)
                  d += width;
               else
                  d -= width;
               s += lsize;
               cury--;
               }
            } // 1:1 view
         else
            {
            if ((sv->iScaleX >= 512 || sv->iScaleY >=512) && (sv->cFilter & PIL_VIEWFLAGS_AVERAGE)) // if scaling down more than 2:1
               {
               bSmooth = TRUE;
               iApertureX = (sv->iScaleX >> 8); // get the integer amount of "shrinkage"
               if (iApertureX > 4)
                  iApertureX = 4; // no need to go beyond 4:1
               if (iApertureX == 3)
                  iApertureX = 2; // only allow 2 or 4 for easy shifting
               iApertureY = (sv->iScaleY >> 8); // get the integer amount of "shrinkage"
               if (iApertureY > 4)
                  iApertureY = 4; // no need to go beyond 4:1
               if (iApertureY == 3)
                  iApertureY = 2; // only allow 2 or 4 for easy shifting
               if (iApertureX == 2)
                  iShift = 2;
               else
                  iShift = 4;
               // Set up limits to make sure we don't read past edges of source image
               if (sv->iWinX >= iApertureX)
                  xmin = 0;
               else
                  xmin = iApertureX << 8;
               if (inpage->iWidth - sv->iWidth*iApertureX > iApertureX)
                  xmax = (sv->iWidth*sv->iScaleX) << 8;
               else
                  xmax = ((sv->iWidth - iApertureX)*sv->iScaleX) << 8;

               if (sv->iWinY >= iApertureY)
                  ymin = 0;
               else
                  ymin = iApertureY << 8;
               if (inpage->iHeight - sv->iHeight*iApertureY > iApertureY)
                  ymax = (sv->iHeight*sv->iScaleY) << 8;
               else
                  ymax = ((sv->iHeight - iApertureY)*sv->iScaleY) << 8;
               }
            while (cury)  /* loop through the whole image */
               { /* Calculate source pointer */
               s = &dataptr[sv->iWinX + ((ysum >> 8) + sv->iWinY) * lsize];
               xsum = 0;
               if (bSmooth)
                  {
                  for (x = 0; x < curx; x++)
                     {
                     xoff = xsum >> 8;
                     // don't go beyond source image size
                     if (xsum > xmin && xsum < xmax && ysum > ymin && ysum < ymax) // do pixel averaging
                        {
                        ulPixel = 0;
                        for (k=0; k<iApertureY; k++) // scan vertically
                           {
                           for (j=0; j<iApertureX; j++) // scan horizontally
                              {
                              ul = s[xoff + k*lsize + j];
                              ul |= ul << 16;
                              ul &= ulShiftMask;
                              ulPixel += ul;
                              }
                           }
                        ulPixel >>= iShift; // store averaged pixel
                        ulPixel &= ulShiftMask;
                        ulPixel |= (ulPixel >> 16); // combine G with R+B
                        *d++ = (unsigned short)ulPixel;
                        }
                     else // just copy the single pixel
                        {
                        *d++ = s[xoff];
                        }
                     xsum += iScaleX;
                     }
                  }
               else // quick and dirty image scaling
                  {
#ifdef USE_ARM_ASM    // use ARM asm
                  ARMDrawScaled16_0(s, d, curx, iScaleX);
                  d += curx;
#else
                  for (x = 0; x < curx; x++)
                     {
                     *d++ = s[xsum >> 8];
                     xsum += iScaleX;
                     }
#endif // _WIN32_WCE
                  }
               d -= curx;  /* Move back over stuff drawn */
               if (bTop)
                  d += width; /* Move down to next line */
               else
                  d -= width; /* Move up to next line */
               ysum += sv->iScaleY;
               cury--;
               }
            } // scaled view
         break; // 0

      case 90:
      // start in upper left corner of virtual display
         dataptr = (unsigned short *)&inpage->pData[inpage->iOffset + inpage->iPitch*(inpage->iHeight-1)];
         iScaleX = sv->iScaleX; /* Speed up access to this value */
         ysum = 0;
         if (iScaleX == 256 && sv->iScaleY == 256) // 1:1 image, draw it faster way
            {
            s = &dataptr[sv->iWinY - (sv->iWinX * lsize)];
            while (cury)
               {
               for (x=0; x<curx; x++)
                  {
                  d[x] = s[-x*lsize];
                  }
               if (bTop)
                  d += width;
               else
                  d -= width;
               s++;
               cury--;
               }
            } // 1:1 view
         else
            {
            if ((sv->iScaleX >= 512 || sv->iScaleY >=512) && (sv->cFilter & PIL_VIEWFLAGS_AVERAGE)) // if scaling down more than 2:1
               {
               bSmooth = TRUE;
               iApertureX = (sv->iScaleY >> 8); // get the integer amount of "shrinkage"
               if (iApertureX > 4)
                  iApertureX = 4; // no need to go beyond 4:1
               iApertureY = (sv->iScaleX >> 8); // get the integer amount of "shrinkage"
               if (iApertureY > 4)
                  iApertureY = 4; // no need to go beyond 5:1
//               iDivisor = iApertureX*iApertureY;
               if (iApertureX == 3)
                  iApertureX = 2; // only allow 2 and 4
               iShift = iApertureX;
               // Set up limits to make sure we don't read past edges of source image
               if (sv->iWinY >= iApertureX)
                  xmin = 0;
               else
                  xmin = iApertureX << 8;
               if (inpage->iHeight - sv->iWidth*iApertureX > iApertureX)
                  xmax = (sv->iWidth*sv->iScaleY) << 8;
               else
                  xmax = ((sv->iWidth - iApertureX)*sv->iScaleY) << 8;

               if (sv->iWinX >= iApertureY)
                  ymin = 0;
               else
                  ymin = iApertureY << 8;
               if (inpage->iWidth - sv->iHeight*iApertureY > iApertureY)
                  ymax = (sv->iHeight*sv->iScaleX) << 8;
               else
                  ymax = ((sv->iHeight - iApertureY)*sv->iScaleX) << 8;
               }
            while (cury)  /* loop through the whole image */
               { /* Calculate source pointer */
               s = &dataptr[(ysum>>8) + sv->iWinY - (sv->iWinX * lsize)];
               xsum = 0;
               if (bSmooth)
                  {
                  for (x = 0; x < curx; x++)
                     {
                     xoff = -lsize*(xsum >> 8);
                     // don't go beyond source image size
                     if (xsum > xmin && xsum < xmax && ysum > ymin && ysum < ymax) // do pixel averaging
                        {
                        ulPixel = 0;
                        for (j=0; j<iApertureX; j++) // scan horizontally
                           {
                           for (k=0; k<iApertureY; k++) // scan vertically
                              {
                              ul = s[xoff + k*lsize + j];
                              ul |= (ul<<16);
                              ul &= ulShiftMask;
                              ulPixel += ul;
                              }
                           }
                        ulPixel >>= iShift; // store averaged pixel
                        ulPixel &= ulShiftMask;
                        ulPixel |= (ulPixel >> 16);
                        *d++ = (unsigned short)ulPixel;
                        }
                     else // just copy the single pixel
                        {
                        *d++ = s[xoff];
                        }
                     xsum += iScaleX;
                     }
                  }
               else // quick and dirty image scaling
                  {
                  for (x = 0; x < curx; x++)
                     {
                     *d++ = s[-lsize*(xsum >> 8)];
                     xsum += iScaleX;
                     }
                  }
               d -= curx;  /* Move back over stuff drawn */
               if (bTop)
                  d += width; /* Move down to next line */
               else
                  d -= width; /* Move up to next line */
               ysum += sv->iScaleY;
               cury--;
               }
            } // scaled view
         break; // 90

      case 180:
         dataptr = (unsigned short *)&inpage->pData[inpage->iOffset + (inpage->iHeight-1)*inpage->iPitch + (inpage->iWidth-1)*2];
         iScaleX = sv->iScaleX; /* Speed up access to this value */
         ysum = 0;
         if (iScaleX == 256 && sv->iScaleY == 256) // 1:1 image, draw it faster way
            {
            s = &dataptr[-sv->iWinX - (sv->iWinY * lsize)];
            while (cury)
               {
               for (x=0; x<curx; x++)
                  {
                  d[x] = s[-x];
                  }
               if (bTop)
                  d += width;
               else
                  d -= width;
               s -= lsize;
               cury--;
               }
            } // 1:1 view
         else
            {
            if ((sv->iScaleX >= 512 || sv->iScaleY >=512) && (sv->cFilter & PIL_VIEWFLAGS_AVERAGE)) // if scaling down more than 2:1
               {
               bSmooth = TRUE;
               iApertureX = (sv->iScaleX >> 8); // get the integer amount of "shrinkage"
               if (iApertureX > 4)
                  iApertureX = 4; // no need to go beyond 4:1
               iApertureY = (sv->iScaleY >> 8); // get the integer amount of "shrinkage"
               if (iApertureY > 4)
                  iApertureY = 4; // no need to go beyond 4:1
               if (iApertureX == 3)
                  iApertureX = 2; // only allow 2 or 4
//               iDivisor = iApertureX*iApertureY;
               iShift = iApertureX;
               // Set up limits to make sure we don't read past edges of source image
               if (sv->iWinX >= iApertureX)
                  xmin = 0;
               else
                  xmin = iApertureX << 8;
               if (inpage->iWidth - sv->iWidth*iApertureX > iApertureX)
                  xmax = (sv->iWidth*sv->iScaleX) << 8;
               else
                  xmax = ((sv->iWidth - iApertureX)*sv->iScaleX) << 8;

               if (sv->iWinY >= iApertureY)
                  ymin = 0;
               else
                  ymin = iApertureY << 8;
               if (inpage->iHeight - sv->iHeight*iApertureY > iApertureY)
                  ymax = (sv->iHeight*sv->iScaleY) << 8;
               else
                  ymax = ((sv->iHeight - iApertureY)*sv->iScaleY) << 8;
               }
            while (cury)  /* loop through the whole image */
               { /* Calculate source pointer */
               s = &dataptr[-sv->iWinX - ((ysum >> 8) + sv->iWinY) * lsize];
               xsum = 0;
               for (x = 0; x < curx; x++)
                  {
                  xoff = -(signed int)(xsum >> 8);
                  // don't go beyond source image size
                  if (xsum > xmin && xsum < xmax && ysum > ymin && ysum < ymax) // do pixel averaging
                     {
                     ulPixel = 0;
                     for (j=0; j<iApertureX; j++) // scan horizontally
                        {
                        for (k=0; k<iApertureY; k++) // scan vertically
                           {
                           ul = s[xoff + k*lsize + j];
                           ul |= (ul<<16);
                           ul &= ulShiftMask;
                           ulPixel += ul;
                           }
                        }
                     ulPixel >>= iShift; // store averaged pixel
                     ulPixel &= ulShiftMask;
                     ulPixel |= (ulPixel >> 16);
                     *d++ = (unsigned short)ulPixel;
                     }
                  else // just copy the single pixel
                     {
                     *d++ = s[xoff];
                     }
                  xsum += iScaleX;
                  }
               d -= curx;  /* Move back over stuff drawn */
               if (bTop)
                  d += width; /* Move down to next line */
               else
                  d -= width; /* Move up to next line */
               ysum += sv->iScaleY;
               cury--;
               }
            } // scaled view
         break; // 180

      case 270:
         dataptr = (unsigned short *)&inpage->pData[inpage->iOffset + (inpage->iWidth-1)*2];
         iScaleX = sv->iScaleX; /* Speed up access to this value */
         ysum = 0;
         if (iScaleX == 256 && sv->iScaleY == 256) // 1:1 image, draw it faster way
            {
            s = &dataptr[-sv->iWinY + sv->iWinX * lsize];
            while (cury)
               {
               for (x=0; x<curx; x++)
                  {
                  d[x] = s[x*lsize];
                  }
               if (bTop)
                  d += width;
               else
                  d -= width;
               s--;
               cury--;
               }
            } // 1:1 view
         else
            {
            if ((sv->iScaleX >= 512 || sv->iScaleY >=512) && (sv->cFilter & PIL_VIEWFLAGS_AVERAGE)) // if scaling down more than 2:1
               {
               bSmooth = TRUE;
               iApertureX = (sv->iScaleY >> 8); // get the integer amount of "shrinkage"
               if (iApertureX > 4)
                  iApertureX = 4; // no need to go beyond 4:1
               iApertureY = (sv->iScaleX >> 8); // get the integer amount of "shrinkage"
               if (iApertureY > 4)
                  iApertureY = 4; // no need to go beyond 4:1
               if (iApertureX == 3)
                  iApertureX = 2; // only allow 2 or 4
//               iDivisor = iApertureX*iApertureY;
               iShift = iApertureX;
               // Set up limits to make sure we don't read past edges of source image
               if (sv->iWinY >= iApertureX)
                  xmin = 0;
               else
                  xmin = iApertureX << 8;
               if (inpage->iHeight - sv->iWidth*iApertureX > iApertureX)
                  xmax = (sv->iWidth*sv->iScaleY) << 8;
               else
                  xmax = ((sv->iWidth - iApertureX)*sv->iScaleY) << 8;

               if (sv->iWinX >= iApertureY)
                  ymin = 0;
               else
                  ymin = iApertureY << 8;
               if (inpage->iWidth - sv->iHeight*iApertureY > iApertureY)
                  ymax = (sv->iHeight*sv->iScaleX) << 8;
               else
                  ymax = ((sv->iHeight - iApertureY)*sv->iScaleX) << 8;
               }
            while (cury)  /* loop through the whole image */
               { /* Calculate source pointer */
               s = &dataptr[-(signed int)(sv->iWinY + (ysum >> 8)) + sv->iWinX * lsize];
               xsum = 0;
               if (bSmooth)
                  {
                  for (x = 0; x < curx; x++)
                     {
                     xoff = lsize*(xsum >> 8);
                     // don't go beyond source image size
                     if (xsum > xmin && xsum < xmax && ysum > ymin && ysum < ymax) // do pixel averaging
                        {
                        ulPixel = 0;
                        for (j=0; j<iApertureX; j++) // scan horizontally
                           {
                           for (k=0; k<iApertureY; k++) // scan vertically
                              {
                              ul = s[xoff + k*lsize + j];
                              ul |= (ul<<16);
                              ul &= ulShiftMask;
                              ulPixel += ul;
                              }
                           }
                        ulPixel >>= iShift; // store averaged pixel
                        ulPixel &= ulShiftMask;
                        ulPixel |= (ulPixel >> 16);
                        *d++ = (unsigned short)ulPixel;
                        }
                     else // just copy the single pixel
                        {
                        *d++ = s[xoff];
                        }
                     xsum += iScaleX;
                     }
                  }
               else // quick and dirty image scaling
                  {
                  for (x = 0; x < curx; x++)
                     {
                     *d++ = s[(xsum >> 8)*lsize];
                     xsum += iScaleX;
                     }
                  }
               d -= curx;  /* Move back over stuff drawn */
               if (bTop)
                  d += width; /* Move down to next line */
               else
                  d -= width; /* Move up to next line */
               ysum += sv->iScaleY;
               cury--;
               }
            } // scaled view
         break; // 270

      default: // invalid angle
         return PIL_ERROR_INVPARAM;
      } // switch

   if (pGammaBrightness) // adjust gamma/brightness based on lookup table
      {
      dataptr = (unsigned short *)&pGammaBrightness[0x100]; // point to 16-bit table
      s = (unsigned short *)sv->pBitmap;
      i = width * sv->iHeight; // number of words to convert
      d = &s[i]; // ending pointer
      while (s < d)
		  {
			*s = dataptr[s[0]]; // adjust each 16-bit pixel
			s++;
		  }
      }
   return 0; /* No errors */

} /* PILDraw16() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILDraw24(PIL_PAGE *, PIL_VIEW *, BOOL, BOOL)              *
 *                                                                          *
 *  PURPOSE    : Draw a view of an 24bpp SPV object into a bitmap.          *
 *                                                                          *
 ****************************************************************************/
static int PILDraw24(PIL_PAGE *inpage, PIL_VIEW *sv, int bTop, unsigned char *pGammaBrightness)
{
int curx, cury, i, lsize, width;
unsigned long ysum, xsum, iScaleX;
int x;
unsigned char *s, *d;
BOOL bSmooth = FALSE; // special anti-alias code
int iRed, iGreen, iBlue, iDivisor, iApertureX=0, iApertureY=0; // For color averaging
unsigned long xmin, xmax, ymin, ymax;
int j, k, iOffset;

   if (sv->iOrientation == 0 || sv->iOrientation == 180)
      {
      /* Pick the smaller area to draw */
      i = ((inpage->iHeight - sv->iWinY) << 8) / sv->iScaleY;
      if (i < sv->iHeight)
         {
         cury = i;
         if (cury < 0)
            cury |= 0;
         }
      else
         cury = sv->iHeight;

      /* Pick the smaller area to draw */
      i = ((inpage->iWidth - sv->iWinX) << 8) / sv->iScaleX;
      if (i < sv->iWidth)
         curx = i;
      else
         curx = sv->iWidth;
      }
   else // rotated 90/270
      {
      /* Pick the smaller area to draw */
      i = ((inpage->iWidth - sv->iWinY) << 8) / sv->iScaleY;
      if (i < sv->iHeight)
         cury = i;
      else
         cury = sv->iHeight;

      /* Pick the smaller area to draw */
      i = ((inpage->iHeight - sv->iWinX) << 8) / sv->iScaleX;
      if (i < sv->iWidth)
         curx = i;
      else
         curx = sv->iWidth;
      }

//   lsize = inpage->iWidth * 3;
//   lsize = (lsize + 3) & 0xfffc;
   lsize = inpage->iPitch;
   width = mPILCalcSize(sv->iWidth, 24);
   if (bTop)
      d = sv->pBitmap; /* Destination pointer */
   else
      d = sv->pBitmap + (sv->iHeight-1) * width; /* Destination pointer */
   iScaleX = sv->iScaleX; /* Speed up access to this value */
   ysum = 0;
   switch (sv->iOrientation)
      {
      case 0:
         if (iScaleX == 256 && sv->iScaleY == 256)
            {
            s = inpage->pData + inpage->iOffset + sv->iWinX*3 + ((ysum >> 8) + sv->iWinY) * lsize;
            while (cury)  /* loop through the whole image */
               {
               memcpy(d, s, curx * 3);
               if (bTop)
                  d += width; /* Move down to next line */
               else
                  d -= width; /* Move up to next line */
               s += lsize;
               cury--;
               }
            } // 1:1 view
         else
            {
            if ((sv->iScaleX >= 512 || sv->iScaleY >=512) && (sv->cFilter & PIL_VIEWFLAGS_AVERAGE)) // if scaling down more than 2:1
               {
               bSmooth = TRUE;
               iApertureX = (sv->iScaleX / 256); // get the integer amount of "shrinkage"
               if (iApertureX > 5)
                  iApertureX = 5; // no need to go beyond 5:1
               iApertureY = (sv->iScaleY / 256); // get the integer amount of "shrinkage"
               if (iApertureY > 5)
                  iApertureY = 5; // no need to go beyond 5:1
               iDivisor = iApertureX*iApertureY;
               // Set up limits to make sure we don't read past edges of source image
               if (sv->iWinX >= iApertureX)
                  xmin = 0;
               else
                  xmin = iApertureX << 8;
               if (inpage->iWidth - sv->iWidth*iApertureX > iApertureX)
                  xmax = (sv->iWidth*sv->iScaleX) << 8;
               else
                  xmax = ((sv->iWidth - iApertureX)*sv->iScaleX) << 8;

               if (sv->iWinY >= iApertureY)
                  ymin = 0;
               else
                  ymin = iApertureY << 8;
               if (inpage->iHeight - sv->iHeight*iApertureY > iApertureY)
                  ymax = (sv->iHeight*sv->iScaleY) << 8;
               else
                  ymax = ((sv->iHeight - iApertureY)*sv->iScaleY) << 8;
               }
            while (cury)  /* loop through the whole image */
               { /* Calculate source pointer */
               int xoff;
               s = inpage->pData + inpage->iOffset + sv->iWinX*3 + ((ysum >> 8) + sv->iWinY) * lsize;
               xsum = 0;
               if (bSmooth)
                  {
                  for (x = 0; x < curx; x++)
                     {
                     xoff = (xsum >> 8) * 3;
                     // don't go beyond source image size
                     if (xsum > xmin && xsum < xmax && ysum > ymin && ysum < ymax) // do pixel averaging
                        {
                        iRed = iGreen = iBlue = 0;
                        for (j=0; j<iApertureX; j++) // scan horizontally
                           {
                           for (k=0; k<iApertureY; k++) // scan vertically
                              {
                              iOffset = xoff + k*lsize + j*3;
                              iRed += s[iOffset+0];
                              iGreen += s[iOffset+1];
                              iBlue += s[iOffset+2];
                              }
                           }
                        d[0] = (unsigned char)(iRed / iDivisor); // store averaged pixel
                        d[1] = (unsigned char)(iGreen / iDivisor);
                        d[2] = (unsigned char)(iBlue / iDivisor);
                        }
                     else // just copy the single pixel
                        {
                        d[0] = s[xoff+0];
                        d[1] = s[xoff+1];
                        d[2] = s[xoff+2];
                        }
                     d += 3;
                     xsum += iScaleX;
                     }
                  }
               else // quick and dirty image scaling
                  {
                  for (x = 0; x < curx; x++)
                     {
                     xoff = (xsum >> 8) * 3;
                     d[0] = s[xoff+0];
                     d[1] = s[xoff+1];
                     d[2] = s[xoff+2];
                     d += 3;
                     xsum += iScaleX;
                     }
                  }
               d -= curx * 3;  /* Move back over stuff drawn */
               if (bTop)
                  d += width; /* Move down to next line */
               else
                  d -= width; /* Move up to next line */
               ysum += sv->iScaleY;
               cury--;
               }
            } // scaled view
         break; // 0

      case 90:
         if (iScaleX == 256 && sv->iScaleY == 256)
            {
            s = inpage->pData + inpage->iOffset + sv->iWinY*3 + (inpage->iHeight - 1 - sv->iWinX) * lsize;
            while (cury)  /* loop through the whole image */
               {
               for (x=0; x<curx; x++)
                  {
                  d[0] = s[0];
                  d[1] = s[1];
                  d[2] = s[2];
                  d += 3;
                  s -= lsize;
                  }
               d -= 3*curx;
               s += curx*lsize;
               s+=3;
               if (bTop)
                  d += width; /* Move down to next line */
               else
                  d -= width; /* Move up to next line */
               cury--;
               }
            } // 1:1 view
         else
            {
            if ((sv->iScaleX >= 512 || sv->iScaleY >=512) && (sv->cFilter & PIL_VIEWFLAGS_AVERAGE)) // if scaling down more than 2:1
               {
               bSmooth = TRUE;
               iApertureX = (sv->iScaleY / 256); // get the integer amount of "shrinkage"
               if (iApertureX > 5)
                  iApertureX = 5; // no need to go beyond 5:1
               iApertureY = (sv->iScaleX / 256); // get the integer amount of "shrinkage"
               if (iApertureY > 5)
                  iApertureY = 5; // no need to go beyond 5:1
               iDivisor = iApertureX*iApertureY;
               // Set up limits to make sure we don't read past edges of source image
               if (sv->iWinY >= iApertureX)
                  xmin = 0;
               else
                  xmin = iApertureX << 8;
               if (inpage->iHeight - sv->iWidth*iApertureX > iApertureX)
                  xmax = (sv->iWidth*sv->iScaleY) << 8;
               else
                  xmax = ((sv->iWidth - iApertureX)*sv->iScaleY) << 8;

               if (sv->iWinX >= iApertureY)
                  ymin = 0;
               else
                  ymin = iApertureY << 8;
               if (inpage->iWidth - sv->iHeight*iApertureY > iApertureY)
                  ymax = (sv->iHeight*sv->iScaleX) << 8;
               else
                  ymax = ((sv->iHeight - iApertureY)*sv->iScaleX) << 8;
               }
            while (cury)  /* loop through the whole image */
               { /* Calculate source pointer */
               int xoff;
               s = inpage->pData + inpage->iOffset + (sv->iWinY+(ysum>>8))*3 + (inpage->iHeight - 1 - sv->iWinX) * lsize;
               xsum = 0;
               if (bSmooth)
                  {
                  for (x = 0; x < curx; x++)
                     {
                     xoff = -lsize*(xsum >> 8);
                     // don't go beyond source image size
                     if (xsum > xmin && xsum < xmax && ysum > ymin && ysum < ymax) // do pixel averaging
                        {
                        iRed = iGreen = iBlue = 0;
                        for (j=0; j<iApertureX; j++) // scan horizontally
                           {
                           for (k=0; k<iApertureY; k++) // scan vertically
                              {
                              iOffset = xoff + k*lsize + j*3;
                              iRed += s[iOffset+0];
                              iGreen += s[iOffset+1];
                              iBlue += s[iOffset+2];
                              }
                           }
                        d[0] = (unsigned char)(iRed / iDivisor); // store averaged pixel
                        d[1] = (unsigned char)(iGreen / iDivisor);
                        d[2] = (unsigned char)(iBlue / iDivisor);
                        d += 3;
                        }
                     else // just copy the single pixel
                        {
                        d[0] = s[xoff+0];
                        d[1] = s[xoff+1];
                        d[2] = s[xoff+2];
                        d += 3;
                        }
                     xsum += iScaleX;
                     }
                  }
               else // quick and dirty image scaling
                  {
                  for (x = 0; x < curx; x++)
                     {
                     xoff = -(int)(xsum >> 8) * lsize;
                     d[0] = s[xoff+0];
                     d[1] = s[xoff+1];
                     d[2] = s[xoff+2];
                     xsum += iScaleX;
                     d += 3;
                     }
                  }
               d -= curx * 3;  /* Move back over stuff drawn */
               if (bTop)
                  d += width; /* Move down to next line */
               else
                  d -= width; /* Move up to next line */
               ysum += sv->iScaleY;
               cury--;
               }
            } // scaled view
         break; // 90

      case 180:
         if (iScaleX == 256 && sv->iScaleY == 256)
            {
            s = inpage->pData + inpage->iOffset + (inpage->iWidth-1-sv->iWinX)*3 + (inpage->iHeight-1-sv->iWinY) * lsize;
            while (cury)  /* loop through the whole image */
               {
               for (x=0; x<curx; x++)
                  {
                  d[0] = s[0];
                  d[1] = s[1];
                  d[2] = s[2];
                  d += 3;
                  s -= 3;
                  }
               s += 3 * curx;
               d -= 3 * curx;
               if (bTop)
                  d += width; /* Move down to next line */
               else
                  d -= width; /* Move up to next line */
               s -= lsize;
               cury--;
               }
            } // 1:1 view
         else
            {
            if ((sv->iScaleX >= 512 || sv->iScaleY >=512) && (sv->cFilter & PIL_VIEWFLAGS_AVERAGE)) // if scaling down more than 2:1
               {
               bSmooth = TRUE;
               iApertureX = (sv->iScaleX / 256); // get the integer amount of "shrinkage"
               if (iApertureX > 5)
                  iApertureX = 5; // no need to go beyond 5:1
               iApertureY = (sv->iScaleY / 256); // get the integer amount of "shrinkage"
               if (iApertureY > 5)
                  iApertureY = 5; // no need to go beyond 5:1
               iDivisor = iApertureX*iApertureY;
               // Set up limits to make sure we don't read past edges of source image
               if (sv->iWinX >= iApertureX)
                  xmin = 0;
               else
                  xmin = iApertureX << 8;
               if (inpage->iWidth - sv->iWidth*iApertureX > iApertureX)
                  xmax = (sv->iWidth*sv->iScaleX) << 8;
               else
                  xmax = ((sv->iWidth - iApertureX)*sv->iScaleX) << 8;

               if (sv->iWinY >= iApertureY)
                  ymin = 0;
               else
                  ymin = iApertureY << 8;
               if (inpage->iHeight - sv->iHeight*iApertureY > iApertureY)
                  ymax = (sv->iHeight*sv->iScaleY) << 8;
               else
                  ymax = ((sv->iHeight - iApertureY)*sv->iScaleY) << 8;
               }
            while (cury)  /* loop through the whole image */
               { /* Calculate source pointer */
               int xoff;
               s = inpage->pData + inpage->iOffset + (inpage->iWidth-1-sv->iWinX)*3 + (inpage->iHeight-1-(ysum >> 8) - sv->iWinY) * lsize;
               xsum = 0;
               if (bSmooth)
                  {
                  for (x = 0; x < curx; x++)
                     {
                     xoff = -(signed int)((xsum >> 8) * 3);
                     // don't go beyond source image size
                     if (xsum > xmin && xsum < xmax && ysum > ymin && ysum < ymax) // do pixel averaging
                        {
                        iRed = iGreen = iBlue = 0;
                        for (j=0; j<iApertureX; j++) // scan horizontally
                           {
                           for (k=0; k<iApertureY; k++) // scan vertically
                              {
                              iOffset = xoff + k*lsize + j*3;
                              iRed += s[iOffset+0];
                              iGreen += s[iOffset+1];
                              iBlue += s[iOffset+2];
                              }
                           }
                        d[0] = (unsigned char)(iRed / iDivisor); // store averaged pixel
                        d[1] = (unsigned char)(iGreen / iDivisor);
                        d[2] = (unsigned char)(iBlue / iDivisor);
                        }
                     else // just copy the single pixel
                        {
                        d[0] = s[xoff+0];
                        d[1] = s[xoff+1];
                        d[2] = s[xoff+2];
                        }
                     d += 3;
                     xsum += iScaleX;
                     }
                  }
               else // quick and dirty image scaling
                  {
                  for (x = 0; x < curx; x++)
                     {
                     xoff = (xsum >> 8) * 3;
                     d[0] = s[-xoff];
                     d[1] = s[-xoff+1];
                     d[2] = s[-xoff+2];
                     d += 3;
                     xsum += iScaleX;
                     }
                  }
               d -= curx * 3;  /* Move back over stuff drawn */
               if (bTop)
                  d += width; /* Move down to next line */
               else
                  d -= width; /* Move up to next line */
               ysum += sv->iScaleY;
               cury--;
               }
            } // scaled view
         break; // 180

      case 270:
         if (iScaleX == 256 && sv->iScaleY == 256)
            {
            s = inpage->pData + inpage->iOffset + (inpage->iWidth-1-sv->iWinY)*3 + sv->iWinX * lsize;
            while (cury)  /* loop through the whole image */
               {
               for (x=0; x<curx; x++)
                  {
                  d[0] = s[0];
                  d[1] = s[1];
                  d[2] = s[2];
                  d += 3;
                  s += lsize;
                  }
               d -= curx*3;
               s -= curx*lsize;
               s-=3;
               if (bTop)
                  d += width; /* Move down to next line */
               else
                  d -= width; /* Move up to next line */
               cury--;
               }
            } // 1:1 view
         else
            {
            if ((sv->iScaleX >= 512 || sv->iScaleY >=512) && (sv->cFilter & PIL_VIEWFLAGS_AVERAGE)) // if scaling down more than 2:1
               {
               bSmooth = TRUE;
               iApertureX = (sv->iScaleY / 256); // get the integer amount of "shrinkage"
               if (iApertureX > 5)
                  iApertureX = 5; // no need to go beyond 5:1
               iApertureY = (sv->iScaleX / 256); // get the integer amount of "shrinkage"
               if (iApertureY > 5)
                  iApertureY = 5; // no need to go beyond 5:1
               iDivisor = iApertureX*iApertureY;
               // Set up limits to make sure we don't read past edges of source image
               if (sv->iWinY >= iApertureX)
                  xmin = 0;
               else
                  xmin = iApertureX << 8;
               if (inpage->iHeight - sv->iWidth*iApertureX > iApertureX)
                  xmax = (sv->iWidth*sv->iScaleY) << 8;
               else
                  xmax = ((sv->iWidth - iApertureX)*sv->iScaleY) << 8;

               if (sv->iWinX >= iApertureY)
                  ymin = 0;
               else
                  ymin = iApertureY << 8;
               if (inpage->iWidth - sv->iHeight*iApertureY > iApertureY)
                  ymax = (sv->iHeight*sv->iScaleX) << 8;
               else
                  ymax = ((sv->iHeight - iApertureY)*sv->iScaleX) << 8;
               }
            while (cury)  /* loop through the whole image */
               { /* Calculate source pointer */
               int xoff;
               s = inpage->pData + inpage->iOffset + (inpage->iWidth-1-sv->iWinY-(ysum>>8))*3 + sv->iWinX * lsize;
               xsum = 0;
               if (bSmooth)
                  {
                  for (x = 0; x < curx; x++)
                     {
                     xoff = lsize*(xsum >> 8);
                     // don't go beyond source image size
                     if (xsum > xmin && xsum < xmax && ysum > ymin && ysum < ymax) // do pixel averaging
                        {
                        iRed = iGreen = iBlue = 0;
                        for (j=0; j<iApertureX; j++) // scan horizontally
                           {
                           for (k=0; k<iApertureY; k++) // scan vertically
                              {
                              iOffset = xoff + k*lsize + j*3;
                              iRed += s[iOffset+0];
                              iGreen += s[iOffset+1];
                              iBlue += s[iOffset+2];
                              }
                           }
                        d[0] = (unsigned char)(iRed / iDivisor); // store averaged pixel
                        d[1] = (unsigned char)(iGreen / iDivisor);
                        d[2] = (unsigned char)(iBlue / iDivisor);
                        d += 3;
                        }
                     else // just copy the single pixel
                        {
                        d[0] = s[xoff+0];
                        d[1] = s[xoff+1];
                        d[2] = s[xoff+2];
                        d += 3;
                        }
                     xsum += iScaleX;
                     }
                  }
               else // quick and dirty image scaling
                  {
                  for (x = 0; x < curx; x++)
                     {
                     xoff = (xsum >> 8) * lsize;
                     d[0] = s[xoff+0];
                     d[1] = s[xoff+1];
                     d[2] = s[xoff+2];
                     d += 3;
                     xsum += iScaleX;
                     }
                  }
               d -= curx * 3;  /* Move back over stuff drawn */
               if (bTop)
                  d += width; /* Move down to next line */
               else
                  d -= width; /* Move up to next line */
               ysum += sv->iScaleY;
               cury--;
               }
            } // scaled view
         break; // 270

      default:
         return PIL_ERROR_INVPARAM;
      }

   if (pGammaBrightness) // use the adjustment values
      {
      unsigned char *p, *pEnd;
      i = width * sv->iHeight; // total size in bytes
      p = sv->pBitmap;
      pEnd = &p[i]; // last offset
      while (p < pEnd)
	  {
         *p = pGammaBrightness[p[0]]; // adjust pixels
		 p++;
	  }
      }
   return 0;
} /* PILDraw24() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILDraw32(PIL_PAGE *, PIL_VIEW *, BOOL, BOOL)              *
 *                                                                          *
 *  PURPOSE    : Draw a view of a 32bpp SPV object into a bitmap.           *
 *                                                                          *
 ****************************************************************************/
static int PILDraw32(PIL_PAGE *inpage, PIL_VIEW *sv, int bTop, unsigned char *pGammaBrightness)
{
int curx, cury, i, lsize, width;
unsigned long ysum, xsum, iScaleX;
int x;
unsigned long *s, *d, *dataptr;
BOOL bSmooth = FALSE; // special anti-alias code
int iShift=0, iApertureX=0, iApertureY=0; // For color averaging
//int iRed, iGreen, iBlue;
unsigned long xmin=0, xmax=0, ymin=0, ymax=0;
//unsigned short us;
unsigned long ul, ulPixel, ulShiftMask;
int j, k, xoff;

   /* Pick the smaller areas to draw so we don't go beyond the edges of the image */
   if (sv->iOrientation == 0 || sv->iOrientation == 180)
      {
      i = ((inpage->iHeight - sv->iWinY) << 8) / sv->iScaleY;
      if (i < sv->iHeight)
         cury = i;
      else
         cury = sv->iHeight;
      i = ((inpage->iWidth - sv->iWinX) << 8) / sv->iScaleX;
      if (i < sv->iWidth)
         curx = i;
      else
         curx = sv->iWidth;
      }
   else
      { // 90 / 270
      i = ((inpage->iWidth - sv->iWinY) << 8) / sv->iScaleY;
      if (i < sv->iHeight)
         cury = i;
      else
         cury = sv->iHeight;

      i = ((inpage->iHeight - sv->iWinX) << 8) / sv->iScaleX;
      if (i < sv->iWidth)
         curx = i;
      else
         curx = sv->iWidth;
      } // switch

//   lsize = (inpage->iWidth + 1) & 0xfffe;
   lsize = inpage->iPitch>>2; // get as # of dwords per line
   width = mPILCalcSize(sv->iWidth, 32);
   width >>= 2; // we are calculating with longs
   if (bTop)
      d = (unsigned long *)sv->pBitmap; /* Destination pointer */
   else
      d = (unsigned long *)sv->pBitmap + (sv->iHeight-1)*width; /* Destination pointer */

   switch (sv->iOrientation)
      {
      case 0: // normal view
         dataptr = (unsigned long *)&inpage->pData[inpage->iOffset];
         iScaleX = sv->iScaleX; /* Speed up access to this value */
         ysum = 0;
         if (iScaleX == 256 && sv->iScaleY == 256) // 1:1 image, draw it faster way
            {
            s = &dataptr[sv->iWinX + (sv->iWinY * lsize)];
            while (cury)
               {
               memcpy(d, s, curx*sizeof(long));
               if (bTop)
                  d += width;
               else
                  d -= width;
               s += lsize;
               cury--;
               }
            } // 1:1 view
         else
            {
            if ((sv->iScaleX >= 512 || sv->iScaleY >=512) && (sv->cFilter & PIL_VIEWFLAGS_AVERAGE)) // if scaling down more than 2:1
               {
               bSmooth = TRUE;
               ulShiftMask = 0x07e0f81f;
               iApertureX = (sv->iScaleX >> 8); // get the integer amount of "shrinkage"
               if (iApertureX > 4)
                  iApertureX = 4; // no need to go beyond 4:1
               if (iApertureX == 3)
                  iApertureX = 2; // only allow 2 or 4 for easy shifting
               iApertureY = (sv->iScaleY >> 8); // get the integer amount of "shrinkage"
               if (iApertureY > 4)
                  iApertureY = 4; // no need to go beyond 4:1
               if (iApertureY == 3)
                  iApertureY = 2; // only allow 2 or 4 for easy shifting
               if (iApertureX == 2)
                  iShift = 2;
               else
                  iShift = 4;
               // Set up limits to make sure we don't read past edges of source image
               if (sv->iWinX >= iApertureX)
                  xmin = 0;
               else
                  xmin = iApertureX << 8;
               if (inpage->iWidth - sv->iWidth*iApertureX > iApertureX)
                  xmax = (sv->iWidth*sv->iScaleX) << 8;
               else
                  xmax = ((sv->iWidth - iApertureX)*sv->iScaleX) << 8;

               if (sv->iWinY >= iApertureY)
                  ymin = 0;
               else
                  ymin = iApertureY << 8;
               if (inpage->iHeight - sv->iHeight*iApertureY > iApertureY)
                  ymax = (sv->iHeight*sv->iScaleY) << 8;
               else
                  ymax = ((sv->iHeight - iApertureY)*sv->iScaleY) << 8;
               }
            while (cury)  /* loop through the whole image */
               { /* Calculate source pointer */
               s = &dataptr[sv->iWinX + ((ysum >> 8) + sv->iWinY) * lsize];
               xsum = 0;
               if (bSmooth)
                  {
                  for (x = 0; x < curx; x++)
                     {
                     xoff = xsum >> 8;
                     // don't go beyond source image size
                     if (xsum > xmin && xsum < xmax && ysum > ymin && ysum < ymax) // do pixel averaging
                        {
                        ulPixel = 0;
                        for (k=0; k<iApertureY; k++) // scan vertically
                           {
                           for (j=0; j<iApertureX; j++) // scan horizontally
                              {
                              ul = s[xoff + k*lsize + j];
                              ul |= ul << 16;
                              ul &= ulShiftMask;
                              ulPixel += ul;
                              }
                           }
                        ulPixel >>= iShift; // store averaged pixel
                        ulPixel &= ulShiftMask;
                        ulPixel |= (ulPixel >> 16); // combine G with R+B
                        *d++ = (unsigned short)ulPixel;
                        }
                     else // just copy the single pixel
                        {
                        *d++ = s[xoff];
                        }
                     xsum += iScaleX;
                     }
                  }
               else // quick and dirty image scaling
                  {
#ifdef USE_ARM_ASM    // use ARM asm
                  ARMDrawScaled32_0(s, d, curx, iScaleX);
                  d += curx;
#else
                  for (x = 0; x < curx; x++)
                     {
                     *d++ = s[xsum >> 8];
                     xsum += iScaleX;
                     }
#endif // USE_ARM_ASM
                  }
               d -= curx;  /* Move back over stuff drawn */
               if (bTop)
                  d += width; /* Move down to next line */
               else
                  d -= width; /* Move up to next line */
               ysum += sv->iScaleY;
               cury--;
               }
            } // scaled view
         break; // 0

      case 90:
      // start in upper left corner of virtual display
         dataptr = (unsigned long *)&inpage->pData[inpage->iOffset + inpage->iPitch*(inpage->iHeight-1)];
         iScaleX = sv->iScaleX; /* Speed up access to this value */
         ysum = 0;
         if (iScaleX == 256 && sv->iScaleY == 256) // 1:1 image, draw it faster way
            {
            s = &dataptr[sv->iWinY - (sv->iWinX * lsize)];
            while (cury)
               {
               for (x=0; x<curx; x++)
                  {
                  d[x] = s[-x*lsize];
                  }
               if (bTop)
                  d += width;
               else
                  d -= width;
               s++;
               cury--;
               }
            } // 1:1 view
         else
            {
            if ((sv->iScaleX >= 512 || sv->iScaleY >=512) && (sv->cFilter & PIL_VIEWFLAGS_AVERAGE)) // if scaling down more than 2:1
               {
               bSmooth = TRUE;
               ulShiftMask = 0x07e0f81f;
               iApertureX = (sv->iScaleY >> 8); // get the integer amount of "shrinkage"
               if (iApertureX > 4)
                  iApertureX = 4; // no need to go beyond 4:1
               iApertureY = (sv->iScaleX >> 8); // get the integer amount of "shrinkage"
               if (iApertureY > 4)
                  iApertureY = 4; // no need to go beyond 5:1
//               iDivisor = iApertureX*iApertureY;
               if (iApertureX == 3)
                  iApertureX = 2; // only allow 2 and 4
               iShift = iApertureX;
               // Set up limits to make sure we don't read past edges of source image
               if (sv->iWinY >= iApertureX)
                  xmin = 0;
               else
                  xmin = iApertureX << 8;
               if (inpage->iHeight - sv->iWidth*iApertureX > iApertureX)
                  xmax = (sv->iWidth*sv->iScaleY) << 8;
               else
                  xmax = ((sv->iWidth - iApertureX)*sv->iScaleY) << 8;

               if (sv->iWinX >= iApertureY)
                  ymin = 0;
               else
                  ymin = iApertureY << 8;
               if (inpage->iWidth - sv->iHeight*iApertureY > iApertureY)
                  ymax = (sv->iHeight*sv->iScaleX) << 8;
               else
                  ymax = ((sv->iHeight - iApertureY)*sv->iScaleX) << 8;
               }
            while (cury)  /* loop through the whole image */
               { /* Calculate source pointer */
               s = &dataptr[(ysum>>8) + sv->iWinY - (sv->iWinX * lsize)];
               xsum = 0;
               if (bSmooth)
                  {
                  for (x = 0; x < curx; x++)
                     {
                     xoff = -lsize*(xsum >> 8);
                     // don't go beyond source image size
                     if (xsum > xmin && xsum < xmax && ysum > ymin && ysum < ymax) // do pixel averaging
                        {
                        ulPixel = 0;
                        for (j=0; j<iApertureX; j++) // scan horizontally
                           {
                           for (k=0; k<iApertureY; k++) // scan vertically
                              {
                              ul = s[xoff + k*lsize + j];
                              ul |= (ul<<16);
                              ul &= ulShiftMask;
                              ulPixel += ul;
                              }
                           }
                        ulPixel >>= iShift; // store averaged pixel
                        ulPixel &= ulShiftMask;
                        ulPixel |= (ulPixel >> 16);
                        *d++ = (unsigned short)ulPixel;
                        }
                     else // just copy the single pixel
                        {
                        *d++ = s[xoff];
                        }
                     xsum += iScaleX;
                     }
                  }
               else // quick and dirty image scaling
                  {
                  for (x = 0; x < curx; x++)
                     {
                     *d++ = s[-lsize*(xsum >> 8)];
                     xsum += iScaleX;
                     }
                  }
               d -= curx;  /* Move back over stuff drawn */
               if (bTop)
                  d += width; /* Move down to next line */
               else
                  d -= width; /* Move up to next line */
               ysum += sv->iScaleY;
               cury--;
               }
            } // scaled view
         break; // 90

      case 180:
         dataptr = (unsigned long *)&inpage->pData[inpage->iOffset + (inpage->iHeight-1)*inpage->iPitch + (inpage->iWidth-1)*sizeof(long)];
         iScaleX = sv->iScaleX; /* Speed up access to this value */
         ysum = 0;
         if (iScaleX == 256 && sv->iScaleY == 256) // 1:1 image, draw it faster way
            {
            s = &dataptr[-sv->iWinX - (sv->iWinY * lsize)];
            while (cury)
               {
               for (x=0; x<curx; x++)
                  {
                  d[x] = s[-x];
                  }
               if (bTop)
                  d += width;
               else
                  d -= width;
               s -= lsize;
               cury--;
               }
            } // 1:1 view
         else
            {
            if ((sv->iScaleX >= 512 || sv->iScaleY >=512) && (sv->cFilter & PIL_VIEWFLAGS_AVERAGE)) // if scaling down more than 2:1
               {
               bSmooth = TRUE;
               ulShiftMask = 0x07e0f81f;
               iApertureX = (sv->iScaleX >> 8); // get the integer amount of "shrinkage"
               if (iApertureX > 4)
                  iApertureX = 4; // no need to go beyond 4:1
               iApertureY = (sv->iScaleY >> 8); // get the integer amount of "shrinkage"
               if (iApertureY > 4)
                  iApertureY = 4; // no need to go beyond 4:1
               if (iApertureX == 3)
                  iApertureX = 2; // only allow 2 or 4
//               iDivisor = iApertureX*iApertureY;
               iShift = iApertureX;
               // Set up limits to make sure we don't read past edges of source image
               if (sv->iWinX >= iApertureX)
                  xmin = 0;
               else
                  xmin = iApertureX << 8;
               if (inpage->iWidth - sv->iWidth*iApertureX > iApertureX)
                  xmax = (sv->iWidth*sv->iScaleX) << 8;
               else
                  xmax = ((sv->iWidth - iApertureX)*sv->iScaleX) << 8;

               if (sv->iWinY >= iApertureY)
                  ymin = 0;
               else
                  ymin = iApertureY << 8;
               if (inpage->iHeight - sv->iHeight*iApertureY > iApertureY)
                  ymax = (sv->iHeight*sv->iScaleY) << 8;
               else
                  ymax = ((sv->iHeight - iApertureY)*sv->iScaleY) << 8;
               }
            while (cury)  /* loop through the whole image */
               { /* Calculate source pointer */
               s = &dataptr[-sv->iWinX - ((ysum >> 8) + sv->iWinY) * lsize];
               xsum = 0;
               for (x = 0; x < curx; x++)
                  {
                  xoff = -(signed int)(xsum >> 8);
                  // don't go beyond source image size
                  if (xsum > xmin && xsum < xmax && ysum > ymin && ysum < ymax) // do pixel averaging
                     {
                     ulPixel = 0;
                     for (j=0; j<iApertureX; j++) // scan horizontally
                        {
                        for (k=0; k<iApertureY; k++) // scan vertically
                           {
                           ul = s[xoff + k*lsize + j];
                           ul |= (ul<<16);
                           ul &= ulShiftMask;
                           ulPixel += ul;
                           }
                        }
                     ulPixel >>= iShift; // store averaged pixel
                     ulPixel &= ulShiftMask;
                     ulPixel |= (ulPixel >> 16);
                     *d++ = (unsigned short)ulPixel;
                     }
                  else // just copy the single pixel
                     {
                     *d++ = s[xoff];
                     }
                  xsum += iScaleX;
                  }
               d -= curx;  /* Move back over stuff drawn */
               if (bTop)
                  d += width; /* Move down to next line */
               else
                  d -= width; /* Move up to next line */
               ysum += sv->iScaleY;
               cury--;
               }
            } // scaled view
         break; // 180

      case 270:
         dataptr = (unsigned long *)&inpage->pData[inpage->iOffset + (inpage->iWidth-1)*sizeof(long)];
         iScaleX = sv->iScaleX; /* Speed up access to this value */
         ysum = 0;
         if (iScaleX == 256 && sv->iScaleY == 256) // 1:1 image, draw it faster way
            {
            s = &dataptr[-sv->iWinY + sv->iWinX * lsize];
            while (cury)
               {
               for (x=0; x<curx; x++)
                  {
                  d[x] = s[x*lsize];
                  }
               if (bTop)
                  d += width;
               else
                  d -= width;
               s--;
               cury--;
               }
            } // 1:1 view
         else
            {
            if ((sv->iScaleX >= 512 || sv->iScaleY >=512) && (sv->cFilter & PIL_VIEWFLAGS_AVERAGE)) // if scaling down more than 2:1
               {
               bSmooth = TRUE;
               ulShiftMask = 0x07e0f81f;
               iApertureX = (sv->iScaleY >> 8); // get the integer amount of "shrinkage"
               if (iApertureX > 4)
                  iApertureX = 4; // no need to go beyond 4:1
               iApertureY = (sv->iScaleX >> 8); // get the integer amount of "shrinkage"
               if (iApertureY > 4)
                  iApertureY = 4; // no need to go beyond 4:1
               if (iApertureX == 3)
                  iApertureX = 2; // only allow 2 or 4
//               iDivisor = iApertureX*iApertureY;
               iShift = iApertureX;
               // Set up limits to make sure we don't read past edges of source image
               if (sv->iWinY >= iApertureX)
                  xmin = 0;
               else
                  xmin = iApertureX << 8;
               if (inpage->iHeight - sv->iWidth*iApertureX > iApertureX)
                  xmax = (sv->iWidth*sv->iScaleY) << 8;
               else
                  xmax = ((sv->iWidth - iApertureX)*sv->iScaleY) << 8;

               if (sv->iWinX >= iApertureY)
                  ymin = 0;
               else
                  ymin = iApertureY << 8;
               if (inpage->iWidth - sv->iHeight*iApertureY > iApertureY)
                  ymax = (sv->iHeight*sv->iScaleX) << 8;
               else
                  ymax = ((sv->iHeight - iApertureY)*sv->iScaleX) << 8;
               }
            while (cury)  /* loop through the whole image */
               { /* Calculate source pointer */
               s = &dataptr[-(signed int)(sv->iWinY + (ysum >> 8)) + sv->iWinX * lsize];
               xsum = 0;
               if (bSmooth)
                  {
                  for (x = 0; x < curx; x++)
                     {
                     xoff = lsize*(xsum >> 8);
                     // don't go beyond source image size
                     if (xsum > xmin && xsum < xmax && ysum > ymin && ysum < ymax) // do pixel averaging
                        {
                        ulPixel = 0;
                        for (j=0; j<iApertureX; j++) // scan horizontally
                           {
                           for (k=0; k<iApertureY; k++) // scan vertically
                              {
                              ul = s[xoff + k*lsize + j];
                              ul |= (ul<<16);
                              ul &= ulShiftMask;
                              ulPixel += ul;
                              }
                           }
                        ulPixel >>= iShift; // store averaged pixel
                        ulPixel &= ulShiftMask;
                        ulPixel |= (ulPixel >> 16);
                        *d++ = (unsigned short)ulPixel;
                        }
                     else // just copy the single pixel
                        {
                        *d++ = s[xoff];
                        }
                     xsum += iScaleX;
                     }
                  }
               else // quick and dirty image scaling
                  {
                  for (x = 0; x < curx; x++)
                     {
                     *d++ = s[(xsum >> 8)*lsize];
                     xsum += iScaleX;
                     }
                  }
               d -= curx;  /* Move back over stuff drawn */
               if (bTop)
                  d += width; /* Move down to next line */
               else
                  d -= width; /* Move up to next line */
               ysum += sv->iScaleY;
               cury--;
               }
            } // scaled view
         break; // 270

      default: // invalid angle
         return PIL_ERROR_INVPARAM;
      } // switch

   if (pGammaBrightness) // adjust gamma/brightness based on lookup table
      {
//      dataptr = (unsigned short *)&pGammaBrightness[0x100]; // point to 16-bit table
//      s = (unsigned short *)sv->pBitmap;
//      i = width * sv->iHeight; // number of words to convert
//      d = &s[i]; // ending pointer
//      while (s < d)
//         *s++ = dataptr[s[0]]; // adjust each 16-bit pixel
      }
   return 0; /* No errors */

} /* PILDraw32() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILDraw()                                                  *
 *                                                                          *
 *  PURPOSE    : Render a view of a page.                                   *
 *                                                                          *
 ****************************************************************************/
int mPILDraw(PIL_PAGE *pPage, PIL_VIEW *pView, BOOL bTopDown, void *pGammaBrightness)
{
int iErr;

   iErr = 0;
   if (pView->iScaleX < 32 || pView->iScaleY < 32) /* Don't allow > 8x magnification */
      return PIL_ERROR_INVPARAM;
   if (pView->pBitmap == NULL || pView->iWidth <= 0 || pView->iHeight <= 0)
      return PIL_ERROR_INVPARAM;
   switch (pPage->cBitsperpixel) /* Call the specific code based on the bits per pel */
      {
      case 4:
         iErr = PILDraw4(pPage, pView, bTopDown);
         break;
      case 8:
         iErr = PILDraw8(pPage, pView, bTopDown);
         break;
      case 16:
         iErr = PILDraw16(pPage, pView, bTopDown, pGammaBrightness);
         break;
      case 24:
         iErr = PILDraw24(pPage, pView, bTopDown, pGammaBrightness);
         break;
      case 32:
         iErr = PILDraw32(pPage, pView, bTopDown, pGammaBrightness);
         break;
         
	  default:
         iErr = PIL_ERROR_INVPARAM;
         break;
      }

   return iErr;

} /* mPILDraw() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILInvert()                                                *
 *                                                                          *
 *  PURPOSE    : Invert the colors of the current page.                     *
 *                                                                          *
 ****************************************************************************/
static int PILInvert(PIL_PAGE *pPage)
{
int i, y, lsize, *index;
unsigned long *l;
unsigned char c1, c2, *irlcptr;

      switch (pPage->cBitsperpixel)
         {
         case 1:
            index = (int *)pPage->pData;
            for (y=0; y<pPage->iHeight; y++)
               {
               irlcptr = (unsigned char *)index[y];
               c1 = irlcptr[0];
               if (c1)
                  { // more difficult case of inserting a leading zero
                  *irlcptr++ = 0;
                  while (irlcptr[0] || irlcptr[1])
                     {
                     c2 = irlcptr[0];
                     *irlcptr++ = c1;
                     c1 = c2;  // swap the bytes
                     }
                  *irlcptr++ = c1;
                  }
               else // easier case which starts with 0
                  {
                  while (irlcptr[0] || irlcptr[1])
                     {
                     irlcptr[0] = irlcptr[1];
                     irlcptr++;
                     }
                  }
               *irlcptr++ = 0;      // make sure to store the trailing zeros
               *irlcptr++ = 0;
               }
            break;
         case 4:
         case 8:  // since it is a palette image, invert the palette
            l = (unsigned long *)pPage->pPalette;
            for (i=0; i<192; i++)
               {
               *l = ~*l;
			   l++;
               }
            break;
         case 16:
         case 24:
            lsize = mPILCalcSize(pPage->iWidth, pPage->cBitsperpixel);
            l = (unsigned long *)pPage->pData;
            lsize *= pPage->iHeight;
            lsize >>= 2; // number of dwords
            for (i=0; i<lsize; i++)
               {
               *l = ~*l;
			   l++;
               }
            break;
         default:
            return PIL_ERROR_INVPARAM;
         }
      return 0;
} /* PILInvert() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILFliph()                                                 *
 *                                                                          *
 *  PURPOSE    : Flip a page horizontally                                   *
 *                                                                          *
 ****************************************************************************/
static int PILFliph(PIL_PAGE *pPage)
{
int y;
int iCount, *pIndex;
unsigned char *temp;
unsigned char c1,c2,c3, *s, *d;
unsigned short sTemp, *sd, *ss;
unsigned long lTemp, *ld, *ls;
int x, lsize;

   lsize = mPILCalcSize(pPage->iWidth, pPage->cBitsperpixel);
   iCount = pPage->iHeight; /* number of lines to do */

   switch (pPage->cBitsperpixel)
      {
      case 1:
         temp = MiniIOAlloc(65536); /* Allocate 64K to play with */
         pIndex = (int *)pPage->pData;
         for (y=0; y < pPage->iHeight; y++)
            {
            s = (unsigned char *)pIndex[y];
            d = &temp[65536]; /* point to end of buffer */
            iCount = 0; /* Run count */
            while (s[0] || s[1])
               {
               c1 = s[0];
               iCount++; // increment run count
               if (c1 & 0x80) // 1 or 2 bytes?
                  {  // 2 byte
                  d -= 2;
                  d[0] = c1;
                  d[1] = s[1];
                  s += 2;
                  }
               else // byte-sized run
                  {
                  d--;
                  d[0] = c1;
                  s++;
                  }
               }
            // End of the line, store it back
            s = (unsigned char *)pIndex[y];
            if (!(iCount & 1)) // does it end on black?
               {
               *s++ = 0; // start with white 0
               }
            iCount = (temp + 65536 - d); // get the data size
            memcpy(s, d, iCount);
            }
         MiniIOFree(temp);
         return 0;
      case 4:
         s = &pPage->pData[pPage->iOffset];
         while (iCount)
            {
            d = s + (pPage->iWidth>>1) - 1;
            x = pPage->iWidth >> 2;
            while (x)
               {
               c1 = *s;
               c2 = *d;
               c1 = (c1 << 4) | (c1 >> 4);
               c2 = (c2 << 4) | (c2 >> 4);
               *s++ = c2;
               *d-- = c1;
               x--;
               }
            s += lsize - (pPage->iWidth>>2);
            iCount--;
            }
         return 0;
      case 8:
         s = (unsigned char *)&pPage->pData[pPage->iOffset];
         while (iCount)
            {
            d = s + pPage->iWidth - 1;
            x = pPage->iWidth >> 1;
            while (x)
               {
               c1 = *s;
               *s++ = *d;
               *d-- = c1;
               x--;
               }
            s += lsize - (pPage->iWidth>>1);
            iCount--;
            }
         return 0;
      case 16:
         ss = (unsigned short *)&pPage->pData[pPage->iOffset];
         while (iCount)
            {
            sd = ss + pPage->iWidth - 1;
            x = pPage->iWidth >> 1;
            while (x)
               {
               sTemp = *ss;
               *ss++ = *sd;
               *sd-- = sTemp;
               x--;
               }
            ss += (lsize/2) - (pPage->iWidth>>1);
            iCount--;
            }
         return 0;
      case 24:
         s = (unsigned char *)&pPage->pData[pPage->iOffset];
         while (iCount)
            {
            d = s + (pPage->iWidth - 1)*3;
            x = pPage->iWidth >> 1;
            while (x)
               {
               c1 = s[0];
               c2 = s[1];
               c3 = s[2];
               s[0] = d[0];
               s[1] = d[1];
               s[2] = d[2];
               s += 3;
               d[0] = c1;
               d[1] = c2;
               d[2] = c3;
               d -= 3;
               x--;
               }
            s += lsize - (pPage->iWidth>>1)*3;
            iCount--;
            }
         return 0;
      case 32:
         ls = (unsigned long *)&pPage->pData[pPage->iOffset];
         while (iCount)
            {
            ld = ls + pPage->iWidth - 1;
            x = pPage->iWidth >> 1;
            while (x)
               {
               lTemp = *ls;
               *ls++ = *ld;
               *ld-- = lTemp;
               x--;
               }
            ls += (lsize>>2) - (pPage->iWidth>>1);
            iCount--;
            }
         return 0;
      default: // bad parameter
         return PIL_ERROR_INVPARAM;
      }
} /* PILFliph() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILFlipv()                                                 *
 *                                                                          *
 *  PURPOSE    : Flip a page vertically                                     *
 *                                                                          *
 ****************************************************************************/
int mPILFlipv(PIL_PAGE *pPage)
{
unsigned char c, *s, *d;
int x, lsize, ycount;

   if (pPage->cCompression != PIL_COMP_NONE)
      return PIL_ERROR_INVPARAM;
      
   lsize = pPage->iPitch;
   s = (unsigned char *)&pPage->pData[pPage->iOffset];
   d = s + (pPage->iHeight - 1) * lsize;
   ycount = pPage->iHeight >> 1;
   while (ycount)
      {
      for (x = 0; x < lsize; x++)
         {
         c = *s;
         *s++ = *d;
         *d++ = c;
         }
      d -= lsize * 2; /* Move destination up a line */
      ycount--;
      }
   return 0;

} /* mPILFlipv() */

#ifdef PIL_IMAGE_READ
/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILReadBMP()                                               *
 *                                                                          *
 *  PURPOSE    : Read a BMP file into a static memory block or IRLC         *
 *                                                                          *
 ****************************************************************************/
static int PILReadBMP(PIL_PAGE *pInPage, PIL_PAGE *pOutPage)
{
//   pOutPage->cState = PIL_FILE_STATE_CLOSED;
      pOutPage->pData = pInPage->pData; // just re-use the data pointer
   return 0;

} /* PILReadBMP() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILReadRLE()                                               *
 *                                                                          *
 *  PURPOSE    : Read a Windows run-length bitmap into memory.              *
 *                                                                          *
 ****************************************************************************/
static int PILReadRLE(PIL_PAGE *pInPage, PIL_PAGE *pOutPage)
{
int lsize, x, y, iByteCount;
unsigned char cRed, cGreen, cBlue;
int dy, iExtra;
unsigned char c1, c2, c, color,*buf, *s;
BOOL bDone, bOdd;

   iExtra = pInPage->iWidth & 3;
   if (iExtra)
      {
      iExtra = 4 - iExtra; // number of missing columns to complete a 4-byte width
      }
   lsize = pOutPage->iPitch; /* Bytes per line */
   pOutPage->cState = PIL_PAGE_STATE_LOADED;
   pOutPage->pData = MiniIOAlloc(lsize * pOutPage->iHeight);
   if (pOutPage->pData == NULL)
      return PIL_ERROR_MEMORY;
   pOutPage->iDataSize = lsize * pOutPage->iHeight;
   // reverse the flags because we decode it upside down
   pOutPage->cFlags = PIL_PAGEFLAGS_TOPDOWN;
   pOutPage->iOffset = 0; // we're writing it starting at 0
   s = &pInPage->pData[pInPage->iOffset]; // pointer to source data
   y = pOutPage->iHeight-1;
   x = 0;
   buf = pOutPage->pData + lsize * y; // start on first (bottom) line
   bDone = FALSE;
   if (pOutPage->cBitsperpixel == 4) // 4-bit rle and 8-bit are different
      {
      while (!bDone)
         {
         c = *s++;
         if (c) // repeat pixel count
            {
            color = *s++;
            c1 = color >> 4;  // split the even/odd colors
            c2 = color & 0xf;
            bOdd = FALSE;   // even/odd source pixel
            while (c)
               {
               c--;
               if (bOdd)   // ugly, but necessary
                  {
                  if (x & 1)
                     buf[x>>1] |= c2;
                  else
                     buf[x>>1] |= (c2 << 4);
                  }
               else
                  {
                  if (x & 1)
                     buf[x>>1] |= c1;
                  else
                     buf[x>>1] |= (c1 << 4);
                  }
               x++;
               if (x == pOutPage->iWidth) // move to next line
                  {
                  x = 0; // wrap around - this is a bug
                  y--;
                  if (y < 0)
                     {
                     bDone = TRUE;
                     c = 0;
                     }
                  buf = pOutPage->pData + lsize * y;
                  }
               bOdd = !bOdd;
               }
            }
         else // zero indicates command byte
            {
            c = *s++; // get command byte
            switch (c)
               {
               case 0: // end of line
                  if (x >= 2) // if already at start of line, ignore
                     y--;
                  if (y < 0)
                     bDone = TRUE;
                  x = 0;
                  buf = pOutPage->pData + lsize * y;
                  break; 
               case 1: // end of bitmap
                  bDone = TRUE;
                  break;
               case 2: // move
                  x += *s++;   // move right
                  buf -= (*s++)*lsize; // move up
                  break;
               default: // uncompressed data
                  iByteCount = c / 2;
                  if (x & 1) // odd case is more difficult
                     {
                     bOdd = FALSE;   // even/odd source pixel
                     while (c)
                        {
                        if (!bOdd)
                           color = *s++;
                        c1 = color >> 4;
                        c2 = color & 0xf;
                        if (bOdd)   // ugly, but necessary
                           {
                           if (x & 1)
                              buf[x>>1] |= c2;
                           else
                              buf[x>>1] |= (c2 << 4);
                           }
                        else
                           {
                           if (x & 1)
                              buf[x>>1] |= c1;
                           else
                              buf[x>>1] |= (c1 << 4);
                           }
                        c--;
                        x++;
                        if (x == pOutPage->iWidth) // move to next line
                           {
                           x = 0;
                           y--;
                           if (y < 0)
                              bDone = TRUE;
                           buf = pOutPage->pData + lsize * y;
                           }
                        bOdd = !bOdd;
                        }
                     }
                  else // the source and dest pixels are aligned, just copy
                     {
                     memcpy(&buf[x>>1], s, iByteCount + (c & 1));
                     s += iByteCount + (c & 1);
                     x += c;
                     if (x == pOutPage->iWidth) // move to next line
                        {
                        x = 0;
                        y--;
                        if (y < 0)
                           bDone = TRUE;
                        buf = pOutPage->pData + lsize * y;
                        }
                     }
                  if ((int)s & 1)  // make sure source remains on even-byte boundary
                     s++;
                  break;
               }
            }
         } // while
      }
   if (pOutPage->cBitsperpixel == 8) // 8-bit RLE
      {
      while (!bDone)
         {
         c = *s++;
         if (c) // repeat pixel count
            {
            color = *s++;
            while (c)
               {
               c--;
               *buf++ = color;
               x++;
               if (x == pInPage->iWidth)
                  { // wrap to the next line
                  x = 0;
                  y--;
                  if (y < 0)
                     {
                     bDone = TRUE;  // at end, quit
                     c = 0;
                     }
                  buf = pOutPage->pData + lsize * y;
                  }
               }
            }
         else // zero indicates command byte
            {
            c = *s++; // get command byte
            switch (c)
               {
               case 0: // end of line
                  if (x > iExtra) // if we already didn't wrap the line
                     {
                     y--;
                     if (y < 0)
                        bDone = TRUE;
                     }
                  buf = pOutPage->pData + lsize * y;
                  x = 0;
                  break; 
               case 1: // end of bitmap
                  bDone = TRUE;
                  break;
               case 2: // move
                  buf += *s++;   // move right
                  dy = *s++; // move up
                  y -= dy;
                  buf -= lsize * dy;
                  if (y < 0)
                     bDone = TRUE;
                  break;
               default: // uncompressed data
                  c2 = c; // keep a copy of the length
                  if ((x + c) <= pInPage->iWidth) // fits on the current line
                     {
                     memcpy(buf, s, c);
                     buf += c;
                     s += c;
                     x += c;
                     }
                  else // split to multiple lines
                     {
                     while (c > (pInPage->iWidth - x))
                        {
                        c1 = (unsigned char)(pInPage->iWidth - x);
                        memcpy(buf, s, c1);
                        c -= c1;
                        buf += c;
                        s += c1;
                        x = 0;
                        y--;
                        if (y < 0)
                           bDone = TRUE;
                        buf = pOutPage->pData + lsize * y;
                        }
                     if (c)
                        {
                        memcpy(buf, s, c);
                        buf += c;
                        s += c;
                        x += c;
                        }
                     }
                  if (c2 & 1) // if odd, need to put on even byte boundary
                     s++;
                  break;
               }
            }
         } // while
      }
   if (pOutPage->cBitsperpixel == 24) // 24-bit RLE
      {
      while (!bDone)
         {
         c = *s++;
         if (c) // repeat pixel count
            {
            cBlue  = s[0];
            cGreen = s[1];
            cRed   = s[2];
            s += 3;
            while (c)
               {
               c--;
               buf[0] = cBlue;
               buf[1] = cGreen;
               buf[2] = cRed;
               buf += 3;
               }
            }
         else // zero indicates command byte
            {
            c = *s++; // get command byte
            switch (c)
               {
               case 0: // end of line
                  y--;
                  buf = pOutPage->pData + lsize * y;
                  if (y < 0)
                     bDone = TRUE;
                  break; 
               case 1: // end of bitmap
                  bDone = TRUE;
                  break;
               case 2: // move
                  buf += (*s++)*3;   // move right
                  dy = *s++; // move up
                  y -= dy;
                  buf -= lsize * dy;
                  if (y < 0)
                     bDone = TRUE;
                  break;
               default: // uncompressed data
                  memcpy(buf, s, c*3);
                  buf += c*3;
                  s += c*3;
                  if (c & 1) // if odd, need to put on even byte boundary
                     s++;
                  break;
               }
            }
         } // while
      }
   return 0;

} /* PILReadRLE() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILTIFFHoriz(char *, SPVOBJ *)                             *
 *                                                                          *
 *  PURPOSE    : Perform horizontal differencing on TIFF LZW data.          *
 *                                                                          *
 ****************************************************************************/
static void PILTIFFHoriz(unsigned char *buf, PIL_PAGE *InPage)
{
unsigned char *p;
int i, y;
int lsize;

   lsize = PILCalcBSize(InPage->iWidth, InPage->cBitsperpixel);

   if (InPage->cBitsperpixel == 24)
      {
      for (y = 0; y<InPage->iHeight; y++)
         {
         p = buf + y * lsize;
         for (i = 0; i<(InPage->iWidth-1); i++)
            {
            p[3] += p[0];
            p[4] += p[1];
            p[5] += p[2];
            p += 3;
            }
         }
      }
   else
      {
      for (y = 0; y<InPage->iHeight; y++)
         {
         p = buf + y * lsize;
         for (i = 0; i<(InPage->iWidth-1); i++)
            {
            p[1] += p[0];
            p++;
            }
         }
      }

} /* PILTIFFHoriz() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILMakeGifPels(signed short *, unsigned char *, short, int *, BOOL) *
 *                                                                          *
 *  PURPOSE    : Convert a linked list of codes into pixel data.            *
 *                                                                          *
 ****************************************************************************/
static unsigned char *PILMakeGifPels(unsigned short *giftabs, unsigned char *templine, unsigned char *linebuf, signed short code, int *xcount, unsigned char *buf, int *y, PIL_PAGE *pPage, int lsize, BOOL bGIF)
{
int i, k, iPixCount;
unsigned char *s, *pEnd;

   /* Copy this string of sequential pixels to output buffer */
   iPixCount = 0;
   s = linebuf + 65536; /* Pixels will come out in reversed order */

   while (code >= 0)
      {
      if (s == linebuf) /* Houston, we have a problem */
         {
         return NULL; /* Exit with error */
         }
      *(--s) = (unsigned char)giftabs[CTLAST + code];
//      iPixCount++;
      code = giftabs[CTLINK + code];
      }
   iPixCount = (int)(linebuf + 65536) - (int)s;

   while (iPixCount)
      {
      if (*xcount > iPixCount)  /* Pixels fit completely on the line */
         {
         if (pPage->cBitsperpixel == 4 && bGIF)
            {
            k = pPage->iWidth - *xcount; /* Current x */
            for (i=0; i < iPixCount; i++)
               {
               if ((k & 1) == 0)
                  *buf = (*s++) << 4;
               else
                  *buf++ |= *s++;
               k++;
               }
            }
         else if (pPage->cBitsperpixel == 1 && bGIF)
            {
            unsigned char ucMask;
            int iOff;
            k = pPage->iWidth - *xcount; /* Current x */
            iOff = (k >> 3);
            ucMask = 0x80 >> (k & 7);
            for (i=0; i < iPixCount; i++)
               {
               if (s[0]) // white
                  buf[iOff] |= ucMask;
               else // black
                  buf[iOff] &= ~ucMask;
               ucMask >>= 1;
               if (ucMask == 0)
                  {
                  ucMask = 0x80; // start new byte
                  iOff++;
                  }
               s++;
               }
            }
         else // 8bpp
            {
//            memcpy(buf, s, iPixCount);
//            buf += iPixCount;
            pEnd = buf + iPixCount;
            while (buf < pEnd)
               {
               *buf++ = *s++;
               }
            }
         *xcount -= iPixCount;
         iPixCount = 0;
         }
      else  /* Pixels cross into next line */
         {
         if (pPage->cBitsperpixel == 4 && bGIF)
            {
            k = pPage->iWidth - *xcount; /* Current x */
            for (i=0; i < *xcount; i++)
               {
               if ((k & 1) == 0)
                  *buf = (*s++) << 4;
               else
                  *buf++ |= *s++;
               k++;
               }
            }
         else if (pPage->cBitsperpixel == 1 && bGIF)
            {
            unsigned char ucMask;
            int iOff;
            k = pPage->iWidth - *xcount; /* Current x */
            iOff = (k >> 3);
            ucMask = 0x80 >> (k & 7);
            for (i=0; i < *xcount; i++)
               {
               if (s[0]) // white
                  buf[iOff] |= ucMask;
               else // black
                  buf[iOff] &= ~ucMask;
               ucMask >>= 1;
               if (ucMask == 0)
                  {
                  ucMask = 0x80; // start new byte
                  iOff++;
                  }
               s++;
               }
            }
         else
            {
//            memcpy(buf, s, *xcount);
//            buf += *xcount;
//            s += *xcount;
            pEnd = buf + *xcount;
            while (buf < pEnd)
               {
               *buf++ = *s++;
               }
            }
         iPixCount -= *xcount;
         if (!bGIF)
            *xcount = PILCalcBSize(pPage->iWidth, pPage->cBitsperpixel);
         else
            *xcount = pPage->iWidth; /* Reset pixel count */
         if (bGIF)
            {
            if (pPage->cBitsperpixel == 1)
               buf += lsize;
            else
               buf += lsize - PILCalcBSize(pPage->iWidth, pPage->cBitsperpixel); /* Advance to next line */
            }
         if (pPage->cBitsperpixel == 4 && bGIF && (pPage->iWidth & 1) == 1)
            buf++;
         (*y)++;
         }
      } /* while */
   return buf;
} /* PILMakeGifPels() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILReadLZW()                                               *
 *                                                                          *
 *  PURPOSE    : Decompress an LZW image.                                   *
 *                                                                          *
 ****************************************************************************/
static int PILReadLZW(PIL_PAGE *InPage, PIL_PAGE *OutPage, BOOL bGIF, int iOptions)
{
int i, y, xcount;
int bitnum, bitoff, lsize, iMap;
int iDelta, iStripNum;
unsigned short oldcode, codesize, nextcode, nextlim;
unsigned short *giftabs, cc, eoi;
short sMask;
unsigned char *p, *buf, *linebuf, *templine, codestart;
int iEndRow;
unsigned long ulBits;
unsigned short code;


/* Code limit is different for TIFF and GIF */
   if (!bGIF)
      {
      iDelta = 1;
      if (OutPage->cBitsperpixel == 1) // TIFF LZW has the photometric inverted for our use
         OutPage->cPhotometric = 1 - OutPage->cPhotometric;
      }
   else
      iDelta = 0;
   templine = NULL; /* Suppress compiler warning */
   OutPage->iOffset = 0; // new data offset is 0 (for our uncompressed output)
   if (!bGIF)
      lsize = PILCalcBSize(InPage->iWidth, InPage->cBitsperpixel); // must be TIFF
   else
      lsize = mPILCalcSize(InPage->iWidth, InPage->cBitsperpixel);
   i = lsize * InPage->iHeight+1; /* color bitmap size */
   if (!(iOptions & PIL_CONVERT_NOALLOC))
      OutPage->pData = MiniIOAlloc(i); /* Output buffer is actual bitmap */
   OutPage->iDataSize = i;
   if (OutPage->pData == NULL)
      return PIL_ERROR_MEMORY;
   buf = OutPage->pData;
   p = &InPage->pData[InPage->iOffset];
   iStripNum = 0;
   iEndRow = InPage->iHeight;
   if (InPage->iStripCount)
      {
      p = &InPage->pData[InPage->plStrips[0]];
      iEndRow = InPage->iRowCount;
      }

   if (!bGIF)
      {
      codestart = 8; /* Always 8 bits for TIFF LZW */
      bitoff = 0;
      iMap = 0; /* Don't let it get accidentally un-interlaced */
      }
   else
      {
      iMap = p[0]; /* Get the GIF flags */
      codestart = p[1]; /* Starting code size */
      bitoff = 2; /* Offset into data */
      }
   sMask = -1 << (codestart+1);
   sMask = 0xffff - sMask;
   cc = (sMask >> 1) + 1; /* Clear code */
   eoi = cc + 1;
   y = 0;
   linebuf = MiniIOAlloc(65536);
   giftabs = MiniIOAlloc(33000);  /* 3 8K dictionary tables with extra space */

lzwdoitagain: /* Go here to do more strips */
   if (!bGIF)
      xcount = PILCalcBSize(InPage->iWidth, InPage->cBitsperpixel);
   else
      xcount = InPage->iWidth;
   bitnum = 0;
init_codetable:
   codesize = codestart + 1;
   sMask = -1 << (codestart+1);
   sMask = 0xffff - sMask;
   nextcode = cc + 2;
   nextlim = (unsigned short)((1 << codesize) - iDelta);
   /* Initialize code table */
   for (i=0; i < cc; i++)
      {
      giftabs[CTFIRST+i] = giftabs[CTLAST+i] = (unsigned short)i;
      giftabs[CTLINK+i] = 0xffff;
      }
   for (i=cc; i < 4096; i++) /* Fill remaining slots */
      giftabs[CTLINK+i] = 0xfffe;
   oldcode = (unsigned short)-1;
   code = (unsigned short)-1;
   if (bGIF)
      ulBits = INTELLONG(&p[bitoff]);
   else
      ulBits = MOTOLONG(&p[bitoff]);
   while (code != eoi && y < iEndRow) /* Loop through all lines of the image (or strip) */
       {
       if (!bGIF)
          {
          if (bitnum > 16)
             {
             bitnum -= 16;
             bitoff += 2;
             ulBits <<= 16;
             ulBits |= MOTOSHORT(&p[bitoff+2]);
             }
          code = (unsigned short)(ulBits >> (32-codesize-bitnum));
          code &= sMask;
          }
       else
          {
          if (bitnum > (32-codesize))
             {
             bitoff += (bitnum >> 3);
             bitnum &= 7;
             ulBits = INTELLONG(&p[bitoff]);
             }
          code = (unsigned short)(ulBits >> bitnum); /* Read a 32-bit chunk */
          code &= sMask;
          }
       bitnum += codesize;
       if (code == cc && oldcode != -1) /* Clear code?, and not first code */
          goto init_codetable;
       if (code != eoi)
          {
          if (giftabs[CTLINK + code] == 0xfffe) /* Old code */
             {
             if (oldcode == 0xffff)
                goto giferror;
             giftabs[CTLINK + nextcode] = oldcode;
             giftabs[CTLAST + nextcode] = giftabs[CTFIRST + oldcode];
             giftabs[CTFIRST + nextcode] = giftabs[CTFIRST + oldcode];
             nextcode++;
             if (nextcode >= nextlim && codesize <12) /* Code size must increase */
                {
                codesize++;
                nextlim <<= 1;
                nextlim += (unsigned short)iDelta; /* TIFF LZW irregularity */
                sMask = (sMask << 1) | 1;
                }
             buf = PILMakeGifPels(giftabs, templine, linebuf, code, &xcount, buf, &y, OutPage, lsize, bGIF);
             if (buf == NULL)
                goto lzwnextline; /* Leave with error */
             }
          else /* New code */
             {
             if (oldcode != 0xffff)
                {
                giftabs[CTLINK + nextcode] = oldcode;
                giftabs[CTLAST + nextcode] = giftabs[CTFIRST + code];
                giftabs[CTFIRST + nextcode] = giftabs[CTFIRST + oldcode];
                nextcode++;
                if (nextcode >= nextlim && codesize <12) /* Code size must increase */
                   {
                   codesize++;
                   nextlim <<= 1;
                   nextlim += (unsigned short)iDelta; /* TIFF LZW irregularity */
                   sMask = (sMask << 1) | 1;
                   }
                }
             buf = PILMakeGifPels(giftabs, templine, linebuf, code, &xcount, buf, &y, OutPage, lsize, bGIF);
             if (buf == NULL)
                goto lzwnextline;
             }
          oldcode = code;
          }
       } /* while not end of LZW code stream */
lzwnextline:
    if (InPage->iStripCount && y < InPage->iHeight) /* If there are multiple strips (TIFF only) */
       {
       buf = (unsigned char *)OutPage->pData + (y * lsize);
       iStripNum++;
       p = &InPage->pData[InPage->plStrips[iStripNum]];
       bitoff = 0;
       iEndRow += InPage->iRowCount;
       if (iEndRow > InPage->iHeight)
          iEndRow = InPage->iHeight;
       if (iStripNum < InPage->iStripCount)
          goto lzwdoitagain; /* keep decoding... */
       }
   if (y < (iEndRow-1) && (!(iOptions & PIL_CONVERT_IGNORE_ERRORS)))
      goto giferror; // short page, report error
gifshort:
   OutPage->cCompression = PIL_COMP_NONE;
   if (iMap & 0x40) /* Interlaced? */
      { /* re-order the scan lines */
      unsigned char *buf2;
      int iDest, iGifPass = 0;
      i = lsize * InPage->iHeight; /* color bitmap size */
      buf = (unsigned char *)OutPage->pData; /* reset ptr to start of bitmap */
      buf2 = MiniIOAlloc(i);
      if (buf2 == NULL)
         {
         MiniIOFree(giftabs);
         MiniIOFree(linebuf);
         if (!(iOptions & PIL_CONVERT_NOALLOC))
            MiniIOFree(OutPage->pData); /* Free the image buffer */
         return PIL_ERROR_MEMORY;
         }
      y = 0;
      for (i=0; i<InPage->iHeight; i++)
         {
         iDest = y * lsize;
         bitoff = i * lsize;
         memcpy(&buf2[iDest], &buf[bitoff], lsize);
         y += cGIFPass[iGifPass*2];
         if (y >= InPage->iHeight)
            {
            iGifPass++;
            y=cGIFPass[iGifPass*2+1];
            }
         }
      MiniIOFree(buf); /* Free the old buffer */
      i = lsize * InPage->iHeight; /* color bitmap size */
      OutPage->pData = buf2; /* Replace with corrected bitmap */
      } /* If interlaced GIF */
   MiniIOFree(giftabs);
   MiniIOFree(linebuf);
   i = InPage->cFlags & PIL_PAGEFLAGS_PREDICTOR; /* Check for horizontal differencing */
   if (!bGIF && i == PIL_PAGEFLAGS_PREDICTOR)
      PILTIFFHoriz(OutPage->pData, OutPage); /* Perform horizontal differencing */
   /* Swap red and blue */
   if (!bGIF && InPage->cBitsperpixel == 24)
      {
      for (y=0; y<InPage->iHeight; y++)
         {
         p = (unsigned char *)OutPage->pData + lsize*y;
         PILFixTIFFRGB(p, InPage->iWidth);
         }
      }
   return 0;
giferror:
   if (iOptions & PIL_CONVERT_IGNORE_ERRORS) // suppress the error
      goto gifshort;
   MiniIOFree(giftabs);
   MiniIOFree(linebuf);
   if (!(iOptions & PIL_CONVERT_NOALLOC))
      MiniIOFree(OutPage->pData); /* Free the image buffer */
   return PIL_ERROR_DECOMP;
} /* PILReadLZW() */

static unsigned char PILPAETH(unsigned char a, unsigned char b, unsigned char c)
{
int p, pa, pb, pc;

   p = a + b - c; // initial estimate
   pa = abs(p - a); // distances to a, b, c
   pb = abs(p - b);
   pc = abs(p - c);
   // return nearest of a,b,c,
   // breaking ties in order a,b,c.
   if (pa <= pb && pa <= pc)
      return a;
   else if (pb <= pc)
      return b;
   else return c;

} /* PILPAETH() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILReadPNG(PIL_PAGE *, PIL_PAGE *, int)                    *
 *                                                                          *
 *  PURPOSE    : Convert a buffer of PNG data into uncompressed BMP.        *
 *                                                                          *
 ****************************************************************************/
static int PILReadPNG(PIL_PAGE *inpage, PIL_PAGE *outpage, int iOptions)
{
int i;
int iErr = 0;
unsigned int iOffset, lsize, iLen, iMarker, iUncompressedSize, iCompressedSize;
BOOL bDone;
unsigned char cFilter, *pBuf, *cOutput;
z_stream d_stream; /* decompression stream */
unsigned int j, err, iOut, iIn;

   lsize = PILCalcBSize(outpage->iWidth, outpage->cBitsperpixel);
   outpage->iPitch = lsize;
   pBuf = MiniIOAlloc(inpage->iDataSize); // allocate another buffer for the compressed data
   iUncompressedSize = outpage->iHeight; // make sure it's a multiple of 16-lines tall
   iUncompressedSize *= lsize+1; /* color bitmap size */
   if (iOptions & PIL_CONVERT_NOALLOC)
      {
      cOutput = outpage->pData;
      }
   else
      {
      cOutput = MiniIOAlloc(iUncompressedSize); /* Output buffer is actual bitmap */
      if (cOutput == NULL)
         return PIL_ERROR_MEMORY;
      outpage->pData = (unsigned char *)cOutput;
      }
   outpage->iDataSize = iUncompressedSize;
   outpage->iOffset = 0;
   outpage->cState = PIL_PAGE_STATE_LOADED;
   outpage->cFlags = PIL_PAGEFLAGS_TOPDOWN;
   if (inpage->cBitsperpixel > 1 && inpage->cBitsperpixel < 16) // has a palette
      {
      outpage->pPalette = MiniIOAlloc(1024);
      memcpy(outpage->pPalette, inpage->pPalette, 1024);
      }
   // gather up all of the compressed (FLATE) data
   // and pass it to the standard INFLATE code
   iOffset = 8;
   bDone = FALSE;
   iCompressedSize = 0;
   while (!bDone && iOffset < (unsigned)inpage->iDataSize-8)
      {
      iLen = MOTOLONG(&inpage->pData[iOffset]); // chunk length
      iMarker = MOTOLONG(&inpage->pData[iOffset+4]);
      iOffset += 8; // point to the data
      switch (iMarker)
         {
         case 0x49444154 /*'IDAT'*/: // image data block
            // copy this data block
            memcpy(&pBuf[iCompressedSize], &inpage->pData[iOffset], iLen);
            iCompressedSize += iLen;
            break;
         case 0x49454e44 /*'IEND'*/:
            bDone = TRUE;
            break;
         }
      iOffset += iLen + 4; // skip data + CRC
      }
   // inflate the image data
    d_stream.zalloc = (alloc_func)0;
    d_stream.zfree = (free_func)0;
    d_stream.opaque = (voidpf)0;
    d_stream.next_in  = pBuf;
    d_stream.avail_in = 0;
    d_stream.next_out = cOutput;
    err = inflateInit(&d_stream);
    iOut = iIn = 0x10000;
    while (d_stream.total_out < iUncompressedSize && d_stream.total_in < iCompressedSize)
        {
        if (iUncompressedSize - d_stream.total_out < 0x10000)
           iOut = iUncompressedSize - d_stream.total_out;
        d_stream.avail_in = iIn;
        d_stream.avail_out = iOut; /* force small buffers */
        err = inflate(&d_stream, Z_NO_FLUSH);
        if (err == Z_STREAM_END || err == Z_DATA_ERROR || err == Z_STREAM_ERROR)
           break;
//        CHECK_ERR(err, "inflate");
        }
   err = inflateEnd(&d_stream);
   // post process the image with the filter
   iOut = 0;
   iOffset = 0;
   iIn = inpage->cBitsperpixel >> 3; // byte distance between pixels
   if (iIn < 1) iIn = 1; // round up for sub-8bpp images
   for (i=0; i<outpage->iHeight; i++)
      {
      cFilter = cOutput[iOffset++]; // get filter type
      switch (cFilter)
         {
         case 0: // no filter, just copy
            memcpy(&cOutput[iOut], &cOutput[iOffset], lsize);
            iOut += lsize;
            iOffset += lsize;
            break;
         case 1: // sub
            for (j=0;j<lsize;j++)
               {
               if (j < iIn)
                  cOutput[iOut] = cOutput[iOffset];
               else
                  cOutput[iOut] = cOutput[iOffset]+cOutput[iOut-iIn];
               iOut++;
               iOffset++;
               }
            break;
         case 2: // up
            for (j=0;j<lsize;j++)
               {
               cOutput[iOut] = cOutput[iOffset]+cOutput[iOut-lsize];
               iOut++;
               iOffset++;
               }
            break;
         case 3: // average
            for (j=0; j<lsize; j++)
               {
               int a;
               if (i==0 || j < iIn)
                  {
                  if (i == 0)
                     {
                     if (j < iIn)
                        a = 0;
                     else
                        a = cOutput[iOut-iIn]>>1;
                     }
                  else
                     a = cOutput[iOut-lsize]>>1;
                  }
               else
                  {
                  a = (cOutput[iOut-iIn] + cOutput[iOut-lsize])>>1;
                  }
               cOutput[iOut] = (unsigned char)(cOutput[iOffset] + a);
               iOut++;
               iOffset++;
               }
            break;
         case 4: // paeth
            for (j=0; j<lsize; j++)
               {
               unsigned char a,b,c;
               if (j < iIn)
                  a = 0;
               else
                  a = cOutput[iOut-iIn]; // left
               if (i == 0)
                  b = 0; // above doesn't exist
               else
                  b = cOutput[iOut-lsize]; // above
               if (i == 0 || j < iIn)
                  c = 0;
               else
                  c = cOutput[iOut-lsize-iIn]; // above left
               cOutput[iOut] = cOutput[iOffset] + PILPAETH(a,b,c);
               iOut++;
               iOffset++;
               }
            break;
         default: // problem - undefined filter
            j = 0;
            break;
         } // switch
      } // for each scanline
// need to go through the image in another pass if RGB24/RGB32
   if (iIn==3 || iIn==4) // RGB order is reversed
      {
      unsigned char *p, t;
      for (i=0; i<inpage->iHeight; i++)
         {
         p = &cOutput[i*lsize];
         for (j=0; j<lsize; j+=iIn)
            { // swap R and B
            t = p[j+2];
            p[j+2] = p[j];
            p[j] = t;
            }
         }
      }
   if (inpage->cBitsperpixel == 2) // special case - need to convert to 4bpp since we don't support 2bpp
      {
      unsigned char c, c1, c2, *s, *d;
      int x, y;
      lsize = mPILCalcSize(inpage->iWidth, 4);
      cOutput = MiniIOAlloc(lsize * inpage->iHeight);
      for (y=0; y<inpage->iHeight; y++)
         {
         s = &outpage->pData[outpage->iPitch * y];
         d = &cOutput[lsize * y];
         for (x=0; x<outpage->iPitch; x++)
            {
            c = *s++; // source byte has 4 pixels
            c1 = (c >> 2) & 0x30; // pixel 1
            c1 |= ((c >> 4) & 3);  // pixel 2
            c2 = (c << 2) & 0x30; // pixel 3
            c2 |= (c & 3); // pixel 4;
            *d++ = c1;
            *d++ = c2;
            } // for x
         } // for y
      MiniIOFree(outpage->pData);
      outpage->pData = cOutput;
      outpage->iPitch = lsize;
      outpage->cBitsperpixel = 4;
      } // bpp == 2
   MiniIOFree(pBuf); // free the extra compressed data buffer
   return iErr;
   
} /* PILReadPNG() */

#endif // PIL_IMAGE_READ

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILConvert()                                               *
 *                                                                          *
 *  PURPOSE    : Convert between various image formats                      *
 *                                                                          *
 ****************************************************************************/
int mPILConvert(PIL_PAGE *pInPage, PIL_PAGE *pOutPage, int iOptions)
{
int iErr, iDestType;
PIL_PAGE TempPage;

   iErr = 0;
   iDestType = pOutPage->cCompression;
   memcpy(&TempPage, pInPage, sizeof(PIL_PAGE));
   // A converted page will never have strip info
   pOutPage->iStripCount = TempPage.iStripCount = 0;
   pOutPage->plStrips = TempPage.plStrips = NULL;
   pOutPage->plStripSize = TempPage.plStripSize = NULL;
   pOutPage->pPalette = TempPage.pPalette = NULL;

   if (iOptions & PIL_CONVERT_NOALLOC)
      {
      TempPage.pData = pOutPage->pData; // preserve image buffer
      TempPage.iPitch = pOutPage->iPitch;
      TempPage.lUser = pOutPage->lUser; // preserve private data
      TempPage.pJPEG = pOutPage->pJPEG;
      }

   /* If a color image, copy the palette data */
   if ((pInPage->cBitsperpixel == 4 || pInPage->cBitsperpixel == 8) && pInPage->pPalette != NULL)
      {
      TempPage.pPalette = MiniIOAlloc(768);
      memcpy(TempPage.pPalette, pInPage->pPalette, 768);
      }

   /*--- Just make a copy of the page ---*/
   if (pInPage->cCompression == pOutPage->cCompression)
      {
      TempPage.pData = MiniIOAlloc(pInPage->iDataSize);
      if (TempPage.pData == NULL)
         return PIL_ERROR_MEMORY;
      if (TempPage.cCompression == PIL_COMP_RLE) // special case for run-length data
         { // Need to fix the index offsets for the new memory address
         int y, iDelta, *index, *oldindex;
         iDelta = (int)TempPage.pData - (int)pInPage->pData;
         index = (int *)TempPage.pData;
         oldindex = (int *)pInPage->pData;
         for (y=0; y<TempPage.iHeight; y++)
            {
            index[y] = oldindex[y] + iDelta;
            }
         }
      else
         {
         memcpy(TempPage.pData, &pInPage->pData[pInPage->iOffset], pInPage->iDataSize);
         }
      TempPage.cState = PIL_PAGE_STATE_LOADED;
      TempPage.iOffset = 0;
      memcpy(pOutPage, &TempPage, sizeof(PIL_PAGE));
      if (pOutPage->cFlags & PIL_PAGEFLAGS_BOTTOMUP)
         {
         mPILFlipv(pOutPage); /* Flip it vertically since BMP files are sometimes upside-down */
         pOutPage->cFlags ^= (PIL_PAGEFLAGS_TOPDOWN | PIL_PAGEFLAGS_BOTTOMUP);  // fix the flags
         }
      }
   else
      {
      switch (pInPage->cCompression) /* Convert to IRLC or BMP for conversion to other */
         {
#ifdef PIL_IMAGE_READ
         case PIL_COMP_WINRLE:   // decompress Windows RLE
            iErr = PILReadRLE(pInPage, &TempPage);
            break;
         case PIL_COMP_RLE: // nothing to do
            break;
         case PIL_COMP_GIF:
            iErr = PILReadLZW(pInPage, &TempPage, TRUE, iOptions);
            break;
         case PIL_COMP_LZW:
            iErr = PILReadLZW(pInPage, &TempPage, FALSE, iOptions);
            break;
         case PIL_COMP_FLATE: // png image data
            iErr = PILReadPNG(pInPage, &TempPage, iOptions);
            break;
         case PIL_COMP_NONE:
            iErr = PILReadBMP(pInPage, &TempPage);
            break;
         case PIL_COMP_JPEG:
            iErr = PILReadJPEG(pInPage, &TempPage, iOptions, FALSE);
            break;
#endif // PIL_IMAGE_READ
         default:
            iErr = PIL_ERROR_INVPARAM;
            break;
         }
      if (!iErr)
         {
         unsigned char *pTemp;
         TempPage.cCompression = (char)iDestType;
         pTemp = pOutPage->pData; // preserve data pointer in case we are reusing the buffer
         memcpy(pOutPage, &TempPage, sizeof(PIL_PAGE)); // copy size and res info to output page
         pOutPage->pData = pTemp; // restore it
         switch (iDestType) /* Convert to requested format */
            {
            case PIL_COMP_NONE:
               memcpy(pOutPage, &TempPage, sizeof(PIL_PAGE));
               break;
#ifdef PIL_IMAGE_WRITE
            case PIL_COMP_WINRLE:   // compress Windows RLE
               iErr = PILMakeRLE(&TempPage, pOutPage, iOptions);
               break;
            case PIL_COMP_GIF:
               iErr = PILMakeLZW(&TempPage, pOutPage, TRUE);
               break;
            case PIL_COMP_LZW:
               iErr = PILMakeLZW(&TempPage, pOutPage, FALSE);
               break;
            case PIL_COMP_JPEG:
               if (iOptions < 0 || iOptions > 0xf)
                  iErr = PIL_ERROR_INVPARAM;
               else   
                  iErr = PILMakeJPEG(&TempPage, pOutPage, iOptions);
               break;
#endif // PIL_IMAGE_WRITE
            default:
               iErr = PIL_ERROR_INVPARAM;
               break;
            }
         }
      }

   return iErr;

} /* mPILConvert() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILFree()                                                  *
 *                                                                          *
 *  PURPOSE    : Free the page                                              *
 *                                                                          *
 ****************************************************************************/
int mPILFree(PIL_PAGE *pPage)
{
int iErr, i;

   iErr = 0;
   i = 0; // total memory blocks freed
   if (pPage->pData)
      {
      MiniIOFree(pPage->pData);
      pPage->pData = NULL;
      i++;
      }
   if (pPage->pPalette && (pPage->cBitsperpixel == 4 || pPage->cBitsperpixel == 8))
      {
      MiniIOFree(pPage->pPalette);
      pPage->pPalette = NULL;
      i++;
      }
   if (pPage->lUser)
      {
#ifdef _WIN32
      if (!IsBadReadPtr((void *)pPage->lUser, 4))
#endif
         {
         MiniIOFree((void *)pPage->lUser);
         pPage->lUser = 0;
         }
      }
   if (pPage->plStrips)
      {
      MiniIOFree(pPage->plStrips);
      pPage->plStrips = NULL;
      i++;
      }
   if (pPage->plStripSize)
      {
      MiniIOFree(pPage->plStripSize);
      pPage->plStripSize = NULL;
      i++;
      }
   pPage->iStripCount = 0;
   if (i == 0) // nothing to free      
      iErr = PIL_ERROR_INVPARAM;
   return iErr;

} /* mPILFree() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILBestColors16_8()                                        *
 *                                                                          *
 *  PURPOSE    : Convert 16 to 8-bit with idealized palette                 *
 *                                                                          *
 ****************************************************************************/
static int PILBestColors16_8(PIL_PAGE *pPage)
{
int i, x, y, lsize, lsize2;
unsigned short us, *pPixels;
unsigned char r, g, b;
unsigned char c, *pNew, *pDest; // destination 8bpp image

      lsize = mPILCalcSize(pPage->iWidth, pPage->cBitsperpixel);
      lsize2 = mPILCalcSize(pPage->iWidth, 8);
      i = lsize2* pPage->iHeight;
      pNew = pDest = MiniIOAlloc(i); // destination 8bpp image 
      if (pNew == NULL)
         return PIL_ERROR_MEMORY;

      for (y=0; y<pPage->iHeight; y++)
         {
         pPixels = (unsigned short *)pPage->pData;
         pPixels += (y * lsize)/2;
         pDest = pNew + y * lsize2;
         for (x=0; x<pPage->iWidth; x++)
            {
            // break the pixel into r,g,b
            us = *pPixels++;
            r = (unsigned char)((us & 0xe000) >>8);
            g = (unsigned char)((us & 0x700) >> 6);
            b = (unsigned char)((us & 0x18) >> 3);
            // Create 3-3-2 palette index for dest pixel
            c = r | g | b;
            *pDest++ = c;
            }
         }
      // Replace the 16-bpp image with the new 8-bpp image
      MiniIOFree(pPage->pData);
      pPage->pData = pNew;
      pPage->iPitch = lsize2;
      pPage->cBitsperpixel = 8;
      return 0;

} /* PILBestColors16_8() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILBestColors24_8()                                        *
 *                                                                          *
 *  PURPOSE    : Convert 24 to 8-bit with idealized palette                 *
 *                                                                          *
 ****************************************************************************/
static int PILBestColors24_8(PIL_PAGE *pPage)
{
int i, x, y, lsize, lsize2;
unsigned char *pPixels;
unsigned char r, g, b;
unsigned char c, *pNew, *pDest; // destination 8bpp image

      lsize = mPILCalcSize(pPage->iWidth, pPage->cBitsperpixel);
      lsize2 = mPILCalcSize(pPage->iWidth, 8);
      i = lsize2* pPage->iHeight;
      pNew = pDest = MiniIOAlloc(i); // destination 8bpp image 
      if (pNew == NULL)
         return PIL_ERROR_MEMORY;

      for (y=0; y<pPage->iHeight; y++)
         {
         pPixels = (unsigned char *)pPage->pData;
         pPixels += (y * lsize);
         pDest = pNew + y * lsize2;
         for (x=0; x<pPage->iWidth; x++)
            {
            r = pPixels[x*3+2] & 0xe0;
            g = (pPixels[x*3+1] >> 3) & 0x1c;
            b = pPixels[x*3+0] >> 6;
            // Create 3-3-2 palette index for dest pixel
            c = r | g | b;
            *pDest++ = c;
            }
         }
      // Replace the 16-bpp image with the new 8-bpp image
      MiniIOFree(pPage->pData);
      pPage->pData = pNew;
      pPage->cBitsperpixel = 8;
      pPage->iPitch = lsize2;
      return 0;

} /* PILBestColors24_8() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILErrorDiff16_8()                                         *
 *                                                                          *
 *  PURPOSE    : Optimize colors with error diffusion.                      *
 *                                                                          *
 ****************************************************************************/
static int PILErrorDiff16_8(PIL_PAGE *pPage)
{
int err, i, x, y, lsize, lsize2;
unsigned short us, *pPixels; // line to hold downward errors
signed long *pBuf, *pErrors, lFErr[3], v, h;
signed long e1,e2,e3,e4;
unsigned char r, g, b, rNew, gNew, bNew; // forward errors for RGB
unsigned char cRangeTable[3072];
unsigned char c, *pNew, *pDest; // destination 8bpp image

      lsize = mPILCalcSize(pPage->iWidth, pPage->cBitsperpixel);
      pBuf = (signed long *)MiniIOAlloc(12*pPage->iWidth + 24);  // char error for rgb
      lsize2 = mPILCalcSize(pPage->iWidth, 8);
      i = lsize2* pPage->iHeight;
      pNew = pDest = MiniIOAlloc(i); // destination 8bpp image 
      if (pNew == NULL)
         {
         MiniIOFree(pBuf);
         return PIL_ERROR_MEMORY;
         }

   /*--- Create range limit table ---*/
      memset(cRangeTable,0,1024);
      for (i=0; i<256; i++)
         cRangeTable[1024+i] = (unsigned char)i;
      memset(&cRangeTable[1280], 0xff, 256);

      for (y=0; y<pPage->iHeight; y++)
         {
         pPixels = (unsigned short *)pPage->pData;
         pPixels += (y * lsize)/2;
         pDest = pNew + y * lsize2;
         pErrors = &pBuf[3]; // point to second pixel to reference first
         lFErr[0] = pErrors[0];
         lFErr[1] = pErrors[1];
         lFErr[2] = pErrors[2];
         for (x=0; x<pPage->iWidth; x++)
            {
            // break the pixel into r,g,b
            us = *pPixels;
            r = (unsigned char)((us & 0xf800) >>8);
            g = (unsigned char)((us & 0x7e0) >> 3);
            b = (unsigned char)((us & 0x1f) << 3);
            // add errors
            err = b + lFErr[0];
            bNew = cRangeTable[err + 1024]; // range limit
            err = g + lFErr[1];
            gNew = cRangeTable[err + 1024];
            err = r + lFErr[2];
            rNew = cRangeTable[err + 1024];
            // Store the adjusted pixel
            c = rNew & 0xe0;
            c |= ((gNew & 0xe0) >> 3);
            c |= ((bNew & 0xc0) >> 6);
            pPixels++; // skip to next pixel
            *pDest++ = c;
            // calculate the Floyd-Steinberg error for this pixel
            v = bNew - (bNew & 0xc0); // new error for 8-bit color blue
            h = v >> 1;
            e1 = (7*h)>>3;  // 7/16
            e2 = h - e1;  // 1/16
            e3 = (5*h) >> 3;   // 5/16
            e4 = h - e3;  // 3/16
            // distribute
            if (x < pPage->iWidth-1)
               {
               lFErr[0] = e1 + pErrors[3];
               pErrors[3] = e2;
               if (x == 0)
                  pErrors[0] = e3;
               else
                  {
                  pErrors[0] += e3;
                  pErrors[-3] += e4;
                  }
               }
            else
               lFErr[0] = e1;

            v = gNew - (gNew & 0xe0); // new error for 8-bit color green
            h = v >> 1;
            e1 = (7*h)>>3;
            e2 = h - e1;
            e3 = (5*h) >> 3;
            e4 = h - e3;
            // distribute
            if (x < pPage->iWidth-1)
               {
               lFErr[1] = e1 + pErrors[4];
               pErrors[4] = e2;
               if (x == 0)
                  pErrors[1] = e3;
               else
                  {
                  pErrors[1] += e3;
                  pErrors[-2] += e4;
                  }
               }
            else
               lFErr[1] = e1;

            v = rNew - (rNew & 0xe0); // new error for 8-bit color red
            h = v >> 1;
            e1 = (7*h)>>3;
            e2 = h - e1;
            e3 = (5*h) >> 3;
            e4 = h - e3;
            // distribute
            if (x < pPage->iWidth-1)
               {
               lFErr[2] = e1 + pErrors[5];
               pErrors[5] = e2;
               if (x == 0)
                  pErrors[2] = e3;
               else
                  {
                  pErrors[2] += e3;
                  pErrors[-1] += e4;
                  }
               }
            else
               lFErr[2] = e1;
            pErrors += 3; // point to next pixel errors
            }
         }
      MiniIOFree(pBuf);

      // Replace the 16-bpp image with the new 8-bpp image
      MiniIOFree(pPage->pData);
      pPage->pData = pNew;
      pPage->cBitsperpixel = 8;
      pPage->iPitch = lsize2;
      return 0;

} /* PILErrorDiff16_8() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILErrorDiff24_8()                                         *
 *                                                                          *
 *  PURPOSE    : Optimize colors with error diffusion.                      *
 *                                                                          *
 ****************************************************************************/
static int PILErrorDiff24_8(PIL_PAGE *pPage)
{
int err, i, x, y, lsize, lsize2;
unsigned char *pPixels; // line to hold downward errors
signed long *pBuf, *pErrors, lFErr[3], v, h;
signed long e1,e2,e3,e4;
unsigned char r, g, b, rNew, gNew, bNew; // forward errors for RGB
unsigned char cRangeTable[3072];
unsigned char c, *pNew, *pDest; // destination 8bpp image

      lsize = mPILCalcSize(pPage->iWidth, pPage->cBitsperpixel);
      pBuf = (signed long *)MiniIOAlloc(12*pPage->iWidth + 24);  // char error for rgb
      lsize2 = mPILCalcSize(pPage->iWidth, 8);
      i = lsize2* pPage->iHeight;
      pNew = pDest = MiniIOAlloc(i); // destination 8bpp image 
      if (pNew == NULL)
         {
         MiniIOFree(pBuf);
         return PIL_ERROR_MEMORY;
         }

   /*--- Create range limit table ---*/
      memset(cRangeTable,0,1024);
      for (i=0; i<256; i++)
         cRangeTable[1024+i] = (unsigned char)i;
      memset(&cRangeTable[1280], 0xff, 256);

      for (y=0; y<pPage->iHeight; y++)
         {
         pPixels = (unsigned char *)pPage->pData;
         pPixels += y * lsize;
         pDest = pNew + y * lsize2;
         pErrors = &pBuf[3]; // point to second pixel to reference first
         lFErr[0] = pErrors[0];
         lFErr[1] = pErrors[1];
         lFErr[2] = pErrors[2];
         for (x=0; x<pPage->iWidth; x++)
            {
            // break the pixel into r,g,b
            r = pPixels[2];
            g = pPixels[1];
            b = pPixels[0];
            pPixels += 3; // skip to the next pixel
            // add errors
            err = b + lFErr[0];
            bNew = cRangeTable[err + 1024]; // range limit
            err = g + lFErr[1];
            gNew = cRangeTable[err + 1024];
            err = r + lFErr[2];
            rNew = cRangeTable[err + 1024];
            // Store the adjusted pixel
            c = rNew & 0xe0;
            c |= ((gNew & 0xe0) >> 3);
            c |= ((bNew & 0xc0) >> 6);
            *pDest++ = c;
            // calculate the Floyd-Steinberg error for this pixel
            v = bNew - (bNew & 0xc0); // new error for 8-bit color blue
            h = v >> 1;
            e1 = (7*h)>>3;  // 7/16
            e2 = h - e1;  // 1/16
            e3 = (5*h) >> 3;   // 5/16
            e4 = h - e3;  // 3/16
            // distribute
            if (x < pPage->iWidth-1)
               {
               lFErr[0] = e1 + pErrors[3];
               pErrors[3] = e2;
               if (x == 0)
                  pErrors[0] = e3;
               else
                  {
                  pErrors[0] += e3;
                  pErrors[-3] += e4;
                  }
               }
            else
               lFErr[0] = e1;

            v = gNew - (gNew & 0xe0); // new error for 8-bit color green
            h = v >> 1;
            e1 = (7*h)>>3;
            e2 = h - e1;
            e3 = (5*h) >> 3;
            e4 = h - e3;
            // distribute
            if (x < pPage->iWidth-1)
               {
               lFErr[1] = e1 + pErrors[4];
               pErrors[4] = e2;
               if (x == 0)
                  pErrors[1] = e3;
               else
                  {
                  pErrors[1] += e3;
                  pErrors[-2] += e4;
                  }
               }
            else
               lFErr[1] = e1;

            v = rNew - (rNew & 0xe0); // new error for 8-bit color red
            h = v >> 1;
            e1 = (7*h)>>3;
            e2 = h - e1;
            e3 = (5*h) >> 3;
            e4 = h - e3;
            // distribute
            if (x < pPage->iWidth-1)
               {
               lFErr[2] = e1 + pErrors[5];
               pErrors[5] = e2;
               if (x == 0)
                  pErrors[2] = e3;
               else
                  {
                  pErrors[2] += e3;
                  pErrors[-1] += e4;
                  }
               }
            else
               lFErr[2] = e1;
            pErrors += 3; // point to next pixel errors
            }
         }
      MiniIOFree(pBuf);

      // Replace the 16-bpp image with the new 8-bpp image
      MiniIOFree(pPage->pData);
      pPage->pData = pNew;
      pPage->cBitsperpixel = 8;
      pPage->iPitch = lsize2;
      return 0;

} /* PILErrorDiff24_8() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PIL332Palette(void)                                        *
 *                                                                          *
 *  PURPOSE    : Create an idealized 3-3-2 palette.                         *
 *                                                                          *
 ****************************************************************************/
static unsigned char * PIL332Palette(void)
{
unsigned char *p, *ppal;
int x;

   ppal = p = MiniIOAlloc(1024);
   for (x=0; x<256; x++)
      {
      *p++ = b_pal[x & 3]; // b
      *p++ = rg_pal[(x & 0x1c) >> 2];  // g
      *p++ = rg_pal[(x & 0xe0)>>5]; // r
      }
   return ppal;

} /* PIL332Palette() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILErrorDiff24_12_16()                                        *
 *                                                                          *
 *  PURPOSE    : Optimize colors with error diffusion.                      *
 *                                                                          *
 ****************************************************************************/
static int PILErrorDiff24_12_16(PIL_PAGE *pPage, int iDestBits)
{
int err, i, x, y, lsize, iDelta;
unsigned char *pPixels; // line to hold downward errors
signed long *pBuf, *pErrors, lFErr[3], v, h;
signed long e1,e2,e3,e4;
unsigned char rNew, gNew, bNew; // forward errors for RGB
unsigned char cRangeTable[3072];
unsigned char ucMaskB, ucMaskG, ucMaskR;

      if (pPage->cBitsperpixel == 24)
         iDelta = 3;
      else
         iDelta = 4; // 32-bpp
      if (iDestBits == 12) // masks for RGB444
         {
         ucMaskB = 0xf0;
         ucMaskG = 0xf0;
         ucMaskR = 0xf0;
         }
      else // masks for RGB565
         {
         ucMaskB = 0xf8;
         ucMaskG = 0xfc;
         ucMaskR = 0xf8;
         }
      lsize = mPILCalcSize(pPage->iWidth, pPage->cBitsperpixel);
      pBuf = (signed long *)MiniIOAlloc(12*pPage->iWidth + 24);  // char error for rgb

   /*--- Create range limit table ---*/
      memset(cRangeTable,0,1024);
      for (i=0; i<256; i++)
         cRangeTable[1024+i] = (unsigned char)i;
      memset(&cRangeTable[1280], 0xff, 256);

      for (y=0; y<pPage->iHeight; y++)
         {
         pPixels = (unsigned char *)pPage->pData;
         pPixels += y * lsize;
         pErrors = &pBuf[iDelta]; // point to second pixel to reference first
         lFErr[0] = pErrors[0];
         lFErr[1] = pErrors[1];
         lFErr[2] = pErrors[2];
         for (x=0; x<pPage->iWidth; x++)
            {
            // break the pixel into r,g,b
            // add errors
            err = pPixels[0] + lFErr[0];
            bNew = cRangeTable[err + 1024]; // range limit
            err = pPixels[1] + lFErr[1];
            gNew = cRangeTable[err + 1024];
            err = pPixels[2] + lFErr[2];
            rNew = cRangeTable[err + 1024];
            // store the adjusted pixel
            pPixels[0] = bNew;
            pPixels[1] = gNew;
            pPixels[2] = rNew;
            pPixels += iDelta;
            // calculate the Floyd-Steinberg error for this pixel
            v = bNew - (bNew & ucMaskB); // new error for 12/16-bit color
            h = v >> 1;
            e1 = (7*h)>>3;
            e2 = h - e1;
            e3 = (5*h) >> 3;
            e4 = h - e3;
            // distribute
            if (x < pPage->iWidth-1)
               {
               lFErr[0] = e1 + pErrors[3];
               pErrors[3] = e2;
               if (x == 0)
                  pErrors[0] = e3;
               else
                  {
                  pErrors[0] += e3;
                  pErrors[-3] += e4;
                  }
               }
            else
               lFErr[0] = e1;

            v = gNew - (gNew & ucMaskG); // new error for 12/16-bit color
            h = v >> 1;
            e1 = (7*h)>>3;
            e2 = h - e1;
            e3 = (5*h) >> 3;
            e4 = h - e3;
            // distribute
            if (x < pPage->iWidth-1)
               {
               lFErr[1] = e1 + pErrors[4];
               pErrors[4] = e2;
               if (x == 0)
                  pErrors[1] = e3;
               else
                  {
                  pErrors[1] += e3;
                  pErrors[-2] += e4;
                  }
               }
            else
               lFErr[1] = e1;

            v = rNew - (rNew & ucMaskR); // new error for 12/16-bit color
            h = v >> 1;
            e1 = (7*h)>>3;
            e2 = h - e1;
            e3 = (5*h) >> 3;
            e4 = h - e3;
            // distribute
            if (x < pPage->iWidth-1)
               {
               lFErr[2] = e1 + pErrors[5];
               pErrors[5] = e2;
               if (x == 0)
                  pErrors[2] = e3;
               else
                  {
                  pErrors[2] += e3;
                  pErrors[-1] += e4;
                  }
               }
            else
               lFErr[2] = e1;
            pErrors += 3; // point to next pixel errors
            }
         }
      MiniIOFree(pBuf);
      return 0;

} /* PILErrorDiff24_12_16() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILAdjustColors()                                          *
 *                                                                          *
 *  PURPOSE    : Change the color depth of the page.                        *
 *                                                                          *
 ****************************************************************************/
static int PILAdjustColors(PIL_PAGE *inpage, int iDestBpp, int iOptions)
{
unsigned char r, g, b, *pTemp;
unsigned short us, *pDest, *pus;
unsigned char *p, *pSrc, *pPalette;
int x, y, lsize, lsize2, iErr;

   iErr = 0;

   switch (inpage->cBitsperpixel)
      {
      case 8:
         switch (iDestBpp)
            {
            case 16: // convert from 8 to 16bpp
               pPalette = inpage->pPalette;
               lsize = mPILCalcSize(inpage->iWidth, iDestBpp);
               pTemp = MiniIOAlloc(lsize * inpage->iHeight);
               if (pTemp == NULL)
                  return PIL_ERROR_MEMORY;
               pSrc = inpage->pData;
               pDest = (unsigned short *)pTemp;
               for (y=0; y<inpage->iHeight; y++)
                  {
                  for (x=0; x<inpage->iWidth; x++)
                     {
                     b = pPalette[pSrc[x]*3];
                     g = pPalette[pSrc[x]*3+1];
                     r = pPalette[pSrc[x]*3+2];
                     us = (b >> 3);
                     us |= (unsigned short)(g & 0xfc) << 3;
                     us |= (unsigned short)(r & 0xf8) << 8;
                     pDest[x] = us;
                     }
                  pSrc += inpage->iPitch;
                  pDest += lsize/2;
                  }
               if (inpage->cState == PIL_PAGE_STATE_LOADED)
                  MiniIOFree(inpage->pData);
               inpage->pData = pTemp;
               inpage->cBitsperpixel = (char)iDestBpp;
               MiniIOFree(inpage->pPalette);
               inpage->pPalette = NULL;
               inpage->iDataSize = lsize * inpage->iHeight;
               inpage->iPitch = lsize;
               inpage->cState = PIL_PAGE_STATE_LOADED;
               inpage->iOffset = 0; // new image starts at offset 0
               break;
            case 24: // convert from 8 to 24bpp
               pPalette = inpage->pPalette;
               lsize = mPILCalcSize(inpage->iWidth, iDestBpp);
               lsize2 = mPILCalcSize(inpage->iWidth, inpage->cBitsperpixel);
               pTemp = MiniIOAlloc(lsize * (inpage->iHeight+1));
               if (pTemp == NULL)
                  return PIL_ERROR_MEMORY;
               pSrc = inpage->pData;
               p = (unsigned char *)pTemp;
               for (y=0; y<inpage->iHeight; y++)
                  {
                  for (x=0; x<inpage->iWidth; x++)
                     {
                     p[0] = pPalette[pSrc[x]*3];
                     p[1] = pPalette[pSrc[x]*3+1];
                     p[2] = pPalette[pSrc[x]*3+2];
                     p += 3;
                     }
                  p -= inpage->iWidth*3;
                  p += lsize;
                  pSrc += lsize2;
                  }
               if (inpage->cState == PIL_PAGE_STATE_LOADED)
                  MiniIOFree(inpage->pData);
               inpage->pData = pTemp;
               inpage->cBitsperpixel = (char)iDestBpp;
               MiniIOFree(inpage->pPalette);  // don't need a palette
               inpage->pPalette = NULL;
               inpage->iDataSize = lsize * inpage->iHeight;
               inpage->iPitch = lsize;
               inpage->cState = PIL_PAGE_STATE_LOADED;
               inpage->iOffset = 0; // new image starts at offset 0
               break;
            }
         break;
      case 16:
         switch (iDestBpp)
            {
            case 24: // up from 16 to 24bpp
               lsize = mPILCalcSize(inpage->iWidth, iDestBpp);
               pTemp = MiniIOAlloc(lsize * inpage->iHeight);
               if (pTemp == NULL)
                  return PIL_ERROR_MEMORY;
               pus = (unsigned short *)inpage->pData;
               p = (unsigned char *)pTemp;
               for (y=0; y<inpage->iHeight; y++)
                  {
                  for (x=0; x<inpage->iWidth; x++)
                     {
                     us = pus[x];
                     p[2] = (unsigned char)((us & 0xf800) >> 8);
                     p[1] = (unsigned char)((us & 0x7e0) >> 3);
                     p[0] = (unsigned char)((us & 0x1f) << 3);
                     p += 3;
                     }
                  pus += inpage->iPitch/2;
                  p -= inpage->iWidth * 3;
                  p += lsize;
                  }
               if (inpage->cState == PIL_PAGE_STATE_LOADED)
                  MiniIOFree(inpage->pData);
               inpage->pData = pTemp;
               inpage->cBitsperpixel = (char)iDestBpp;
               inpage->iDataSize = lsize * inpage->iHeight;
               inpage->iPitch = lsize;
               inpage->cState = PIL_PAGE_STATE_LOADED;
               inpage->iOffset = 0; // new image starts at offset 0
               break;
            case 8:
               if (iOptions == PIL_COLORS_ERRORDIFF)
                  iErr = PILErrorDiff16_8(inpage);
               else
                  iErr = PILBestColors16_8(inpage);
         // create a 3-3-2 default palette
               inpage->pPalette = PIL332Palette();
               inpage->iOffset = 0; // new image starts at offset 0
               break;
            }
         break;
      case 24:
         switch (iDestBpp)
            {
            case 8: // error diffuse
               if (iOptions == PIL_COLORS_ERRORDIFF)
                  iErr = PILErrorDiff24_8(inpage);
               else
                  iErr = PILBestColors24_8(inpage);
         // create a 3-3-2 default palette
               inpage->pPalette = PIL332Palette();
               inpage->iOffset = 0; // new image starts at offset 0
               break;
            case 16: // convert image from 24 to 16 bpp
               if (iOptions == PIL_COLORS_ERRORDIFF)
                  PILErrorDiff24_12_16(inpage, iDestBpp);
               else
                  {
                  lsize = mPILCalcSize(inpage->iWidth, iDestBpp);
                  pTemp = MiniIOAlloc(lsize * inpage->iHeight);
                  if (pTemp == NULL)
                     return PIL_ERROR_MEMORY;
                  pSrc = inpage->pData;
                  pDest = (unsigned short *)pTemp;
                  for (y=0; y<inpage->iHeight; y++)
                     {
                     for (x=0; x<inpage->iWidth; x++)
                        {
                        b = pSrc[x*3];
                        g = pSrc[x*3+1];
                        r = pSrc[x*3+2];
                        us = b >> 3;
                        us |= (unsigned short)((g>>2) << 5);
                        us |= (unsigned short)((r>>3) << 11);
                        pDest[x] = us;
                        }
                     pSrc += inpage->iPitch;
                     pDest += lsize/2;
                     }
                  if (inpage->cState == PIL_PAGE_STATE_LOADED)
                     MiniIOFree(inpage->pData);
                  inpage->pData = pTemp;
                  inpage->cBitsperpixel = (char)iDestBpp;
                  inpage->iDataSize = lsize * inpage->iHeight;
                  inpage->iPitch = lsize;
                  inpage->cState = PIL_PAGE_STATE_LOADED;
                  inpage->iOffset = 0; // new image starts at offset 0
                  }
               break;
            }
         break;
      case 32:
         if (iDestBpp == 16 && iOptions == PIL_COLORS_ERRORDIFF)
            PILErrorDiff24_12_16(inpage, iDestBpp);
         break;
      }
   return iErr;

} /* PILAdjustColors() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILRotate()                                                *
 *                                                                          *
 *  PURPOSE    : Rotate a page by n degrees.                                *
 *                                                                          *
 ****************************************************************************/
static int PILRotate(PIL_PAGE *inpage, int iAngle)
{
int i, y;
int iDelta, width, lsize;
char *temp;
char *s, *d, *ctemp, *cEnd;
int x;
unsigned short *us, *ustemp, *usDest, *usEnd;

if (inpage->cBitsperpixel != 1 && inpage->cCompression != PIL_COMP_NONE)
   return PIL_ERROR_INVPARAM;

lsize = mPILCalcSize(inpage->iWidth, inpage->cBitsperpixel);
width = mPILCalcSize(inpage->iHeight, inpage->cBitsperpixel);
iDelta = lsize - PILCalcBSize(inpage->iWidth, inpage->cBitsperpixel);
i = inpage->iHeight; /* swap x and y */
inpage->iHeight = inpage->iWidth;
inpage->iWidth = i;

switch (inpage->cBitsperpixel)
   {
   case 8:
      ctemp = (char *)MiniIOAlloc(inpage->iHeight * width);
      if (ctemp == NULL)
         {
         i = inpage->iHeight; /* swap x and y back since we did not rotate */
         inpage->iHeight = inpage->iWidth;
         inpage->iWidth = i;
         return PIL_ERROR_MEMORY;
         }
      s = (char *)inpage->pData;
      for (y = 0; y < inpage->iWidth; y++)
         {
         d = ctemp + inpage->iWidth - 1 - y; /* Start at right edge */
         cEnd = ctemp + inpage->iHeight * width; // comparing pointer is faster than FOR loop
//         for (x=0; x < InPage->iHeight; x++)
         while (d < cEnd)
            {
            *d = *s++;
            d += width;
            }
         s += lsize - inpage->iHeight;
         }
      if (inpage->cState == PIL_PAGE_STATE_LOADED)
         MiniIOFree(inpage->pData); /* Free the original */
      inpage->cState = PIL_PAGE_STATE_LOADED;
      inpage->pData = (unsigned char *)ctemp; /* Use the new one */
      inpage->iDataSize = inpage->iHeight * width;
      inpage->iPitch = width;
      return 0;
   case 16:
      lsize >>= 1; // correct for working with ushorts
      width >>= 1;
      ustemp = MiniIOAlloc(inpage->iHeight * width * 2);
      if (ustemp == NULL)
         {
         i = inpage->iHeight; /* swap x and y back since we did not rotate */
         inpage->iHeight = inpage->iWidth;
         inpage->iWidth = i;
         return PIL_ERROR_MEMORY;
         }
      us = (unsigned short *)inpage->pData;
      for (y = 0; y < inpage->iWidth; y++)
         {
         usDest = ustemp + inpage->iWidth - 1 - y; /* Start at right edge */
         usEnd = ustemp + inpage->iHeight * width; // comparing pointer is faster than FOR loop
//         for (x=0; x < InPage->iHeight; x++)
         while (usDest < usEnd)
            {
            *usDest = *us++;
            usDest += width;
            }
         us += lsize - inpage->iHeight;
         }
      if (inpage->cState == PIL_PAGE_STATE_LOADED)
         MiniIOFree(inpage->pData); /* Free the original */
      inpage->cState = PIL_PAGE_STATE_LOADED;
      inpage->pData = (unsigned char *)ustemp; /* Use the new one */
      inpage->iDataSize = inpage->iHeight * width * 2;
      inpage->iPitch = width * 2;
      return 0;

   case 24:
      temp = MiniIOAlloc(inpage->iHeight * width);
      if (temp == NULL)
         {
         i = inpage->iHeight; /* swap x and y back since we did not rotate */
         inpage->iHeight = inpage->iWidth;
         inpage->iWidth = i;
         return PIL_ERROR_MEMORY;
         }
      s = (char *)inpage->pData;
      for (y = 0; y < inpage->iWidth; y++)
         {
         d = temp + (inpage->iWidth - 1 - y)*3; /* Start at right edge */
         for (x=0; x < inpage->iHeight; x++)
            {
            d[0] = s[0];
            d[1] = s[1];
            d[2] = s[2];
            d += width;
            s += 3;
            }
         s += iDelta;
         }
      if (inpage->cState == PIL_PAGE_STATE_LOADED)
         MiniIOFree(inpage->pData); /* Free the original */
      inpage->cState = PIL_PAGE_STATE_LOADED;
      inpage->pData = (unsigned char *)temp; /* Use the new one */
      inpage->iDataSize = inpage->iHeight * width;
      inpage->iPitch = width;
      return 0;
   default:
	  return PIL_ERROR_INVPARAM;
   } /* switch */

} /* PILRotate() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILCrop()                                                  *
 *                                                                          *
 *  PURPOSE    : Crop the image.                                            *
 *                                                                          *
 ****************************************************************************/
int mPILCrop(PIL_PAGE *pPage, PIL_VIEW *pView)
{
int iSize;
int iErr = 0;
int iPageX, iPageY;

   if (pView->iOrientation == 0 || pView->iOrientation == 180)
      {
      iPageX = pPage->iWidth;
      iPageY = pPage->iHeight;
      }
   else
      {
      iPageY = pPage->iWidth;
      iPageX = pPage->iHeight;
      }

   if ((pView->iWinX + pView->iWidth) > iPageX || (pView->iWinY + pView->iHeight) > iPageY)
      return PIL_ERROR_INVPARAM;
   if (pView->iWidth > iPageX || pView->iHeight > iPageY)
      return PIL_ERROR_INVPARAM;
   if (pView->iWinX < 0 || pView->iWinY < 0)
      return PIL_ERROR_INVPARAM;

   /* BMP size of new image */
   iSize = mPILCalcSize(pView->iWidth, pPage->cBitsperpixel);
   pView->iPitch = iSize;
   iSize *= (pView->iHeight+1); /* Allow some slop */
   pView->pBitmap = MiniIOAlloc(iSize);

   if (pView->pBitmap == NULL)
      return PIL_ERROR_MEMORY;

   pView->iScaleX = pView->iScaleY = 256; /* Always draw at 1:1 */
   pView->cFilter = 0;

   /* Draw the new bitmap */
   mPILDraw(pPage, pView, TRUE, NULL);

   if (pPage->cState == PIL_PAGE_STATE_LOADED)
      MiniIOFree(pPage->pData); /* Free the old image data */
   pPage->cState = PIL_PAGE_STATE_LOADED;

   pPage->pData = pView->pBitmap;
   pPage->iDataSize = iSize;
   pPage->iWidth = pView->iWidth;
   pPage->iHeight = pView->iHeight;
   pPage->iPitch = mPILCalcSize(pView->iWidth, pPage->cBitsperpixel);
   return iErr;

} /* mPILCrop() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILResize()                                                *
 *                                                                          *
 *  PURPOSE    : Resize the page.                                           *
 *                                                                          *
 ****************************************************************************/
int mPILResize(PIL_PAGE *pPage, PIL_PAGE *pOutPage, int iNewX, int iNewY)
{
PIL_VIEW TempView;
int lsize, iSize;

   if (iNewX <= 0 || iNewX > 32767 || iNewY <= 0 || iNewY > 32767)
      return PIL_ERROR_INVPARAM;
   if (pPage->cBitsperpixel != 1 && pPage->cCompression != PIL_COMP_NONE)
      return PIL_ERROR_INVPARAM;

   /* BMP size of new image */
   lsize = mPILCalcSize(iNewX, pPage->cBitsperpixel);
   iSize = lsize * iNewY;
   TempView.pBitmap = MiniIOAlloc(iSize);
   if (TempView.pBitmap == NULL)
      return PIL_ERROR_MEMORY;

   memcpy(pOutPage, pPage, sizeof(PIL_PAGE));
   pOutPage->iPitch = lsize;

   TempView.iWinX = TempView.iWinY = 0;
   TempView.iScaleX = (pPage->iWidth * 256)/iNewX;
   TempView.iScaleY = (pPage->iHeight * 256)/iNewY;
   TempView.iWidth = iNewX;
   TempView.iHeight = iNewY;
   TempView.cFilter = PIL_VIEWFLAGS_AVERAGE; // do pixel averaging for better results with photos
   TempView.iPitch = lsize;
   TempView.iOrientation = 0;

   /* Draw the new bitmap */
   mPILDraw(pPage, &TempView, TRUE, NULL);

   pOutPage->cCompression = PIL_COMP_NONE;
   pOutPage->iDataSize = iSize;
   pOutPage->iWidth = iNewX;
   pOutPage->iHeight = iNewY;
   pOutPage->pData = TempView.pBitmap;
   pOutPage->cState = PIL_PAGE_STATE_LOADED;
   if (pPage->cBitsperpixel == 4 || pPage->cBitsperpixel == 8) // copy the palette
      {
      pOutPage->pPalette = MiniIOAlloc(768);
      memcpy(pOutPage->pPalette, pPage->pPalette, 768);
      }
   return 0;
  
} /* mPILResize() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILGray(PIL_PAGE *)                                        *
 *                                                                          *
 *  PURPOSE    : Convert a page to grayscale.                               *
 *                                                                          *
 ****************************************************************************/
static int PILGray(PIL_PAGE *inpage)
{
int x, y, i, iDelta, lsize;
PIL_PAGE outpage;
unsigned short us, *pus;
unsigned char *s, *d, r, g, b;
int iError = 0;

switch (inpage->cBitsperpixel)
   {
   case 8: /* Modify the image data so it is true, ordered grayscale */
      d = MiniIOAlloc(256); /* Allocate a conversion table */
      if (!d)
         return PIL_ERROR_MEMORY;
      s = inpage->pPalette;
      for (i=0; i<256; i++)
         { /* Replace each entry with a gray equivalent */
         d[i] = (s[0] + s[1]*2 + s[2]) >> 2;
         s += 3;
         }
      /* Use this table to convert the image data */
      lsize = mPILCalcSize(inpage->iWidth, 8) * inpage->iHeight; /* # of bytes */
      s = inpage->pData;
      for (i=0; i<lsize; i++) /* Convert all of the pixels */
         s[i] = d[s[i]];
      MiniIOFree(inpage->pPalette); /* Free the old palette */
      inpage->pPalette = PILGrayPalette(8);
      break;

   case 16: /* Special case, reduces it to 8bpp */
      iDelta = mPILCalcSize(inpage->iWidth, 16) - (inpage->iWidth * 2);
      iDelta >>= 1; // convert to short pointer offset
      lsize = mPILCalcSize(inpage->iWidth, 8);
      i = lsize * inpage->iHeight;
      i += 4; /* Plus some safety margin at end */
      outpage.iDataSize = i;
      d = outpage.pData = MiniIOAlloc(i); /* Allocate an 8bpp destination bitmap */
      if (!d)
         return PIL_ERROR_MEMORY;
      pus = (unsigned short *)inpage->pData;
      for (y=0; y<inpage->iHeight; y++)
         {
         for (x=0; x<inpage->iWidth; x++)
            {
            us = *pus++;
            r = (us >> 8) & 0xf8;
            g = (us >> 3) & 0xfc;
            b = (us << 3) & 0xf8;
            d[x] = (r + g*2 + b) >> 2; /* Store the gray value */
            }
         d += lsize;
         pus += iDelta;
         }
      MiniIOFree(inpage->pData);
      inpage->pData = outpage.pData;
      inpage->iDataSize = lsize * inpage->iHeight;
      inpage->cBitsperpixel = 8;
      inpage->pPalette = PILGrayPalette(8); /* Allocate an 8bpp gray palette */
      inpage->iPitch = lsize;
      break;

   case 24: /* Special case, reduces it to 8bpp */
      iDelta = mPILCalcSize(inpage->iWidth, 24) - (inpage->iWidth * 3);
      lsize = mPILCalcSize(inpage->iWidth, 8);
      i = lsize * inpage->iHeight;
      i += 4; /* Plus some safety margin at end */
      outpage.iDataSize = i;
      d = outpage.pData = MiniIOAlloc(i); /* Allocate an 8bpp destination bitmap */
      if (!d)
         return PIL_ERROR_MEMORY;
      s = inpage->pData;
      for (y=0; y<inpage->iHeight; y++)
         {
         for (x=0; x<inpage->iWidth; x++)
            {
            d[x] = (s[0] + s[1]*2 + s[3]) >> 2; /* Store the gray value */
            s += 3;
            }
         s += iDelta;
         d += lsize;
         }
      MiniIOFree(inpage->pData);
      inpage->pData = outpage.pData;
      inpage->iDataSize = lsize * inpage->iHeight;
      inpage->cBitsperpixel = 8;
      inpage->pPalette = PILGrayPalette(8); /* Allocate an 8bpp gray palette */
      inpage->iPitch = lsize;
      break;

   default: /* Can't make bilevel into gray */
      iError = PIL_ERROR_INVPARAM;
      break;
   }
   
   return iError;
   
} /* PILGray() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILDarken()                                                *
 *                                                                          *
 *  PURPOSE    : Lighten or darken a page.                                  *
 *                                                                          *
 ****************************************************************************/
static int PILDarken(PIL_PAGE *pPage, int iType)
{
unsigned char *p;
unsigned short us, *pus;
signed long r, g, b;
int i, iCount;

   switch (pPage->cBitsperpixel)
      {
      case 8:
         p = pPage->pPalette;
         iCount = 3*(1 << pPage->cBitsperpixel);
         for (i=0; i<iCount; i++)
            {
            r = p[0];
            r -= iType * 0x11;
            if (r < 0)
               r = 0;
            if (r > 255)
               r = 255;
            p[0] = (unsigned char)r;
            p++;
            }
         break;
      case 16:  // adjust each pixel
         pus = (unsigned short *)pPage->pData;
         iCount = pPage->iWidth * pPage->iHeight;
         for (i=0; i<iCount; i++)
            {
            us = *pus;
            r = us & 0xf800;
            g = us & 0x7e0;
            b = us & 0x1f;
            r -= 0xf80*iType;
            g -= 0x7e*iType;
            b -= 2*iType;
            if (r < 0)
               r = 0;
            if (g < 0)
               g = 0;
            if (b < 0)
               b = 0;
            if (r > 0xf800)
               r = 0xf800;
            if (g > 0x7e0)
               g = 0x7e0;
            if (b > 0x1f)
               b = 0x1f;
            us = (unsigned short)(r & 0xf800);
            us |= (g & 0x7e0);
            us |= (b & 0x1f);
            *pus++ = us;
            }
         break;
      case 24:
         p = pPage->pData;
         iCount = pPage->iPitch * pPage->iHeight;
         for (i=0; i<iCount; i++)
            {
            r = p[0];
            r -= 0x11*iType;
            if (r < 0)
               r = 0;
            if (r > 255)
               r = 255;
            p[0] = (unsigned char)r;
            p++;
            }
         break;
      }
   return 0;

} /* PILDarken() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILModify()                                                *
 *                                                                          *
 *  PURPOSE    : Adjust the page                                            *
 *                                                                          *
 ****************************************************************************/
int mPILModify(PIL_PAGE *pPage, int iOperation, int iParam1, int iParam2)
{
int iErr;

      switch (iOperation)
         {
         case PIL_MODIFY_COLORS:
            iErr = PILAdjustColors(pPage, iParam1, iParam2);
            break;
         case PIL_MODIFY_LIGHTEN:
            iErr = PILDarken(pPage, -1);
            break;
         case PIL_MODIFY_DARKEN:
            iErr = PILDarken(pPage, 1);
            break;
         case PIL_MODIFY_GRAY:
            iErr = PILGray(pPage);
            break;
         case PIL_MODIFY_ROTATE:
            iErr = PILRotate(pPage, iParam1);
            break;
         case PIL_MODIFY_FLIPH:
            iErr = PILFliph(pPage);
            break;
         case PIL_MODIFY_FLIPV:
            iErr = mPILFlipv(pPage);
            break;
         case PIL_MODIFY_INVERT:
            iErr = PILInvert(pPage);
            break;
         }

   return iErr;
} /* mPILModify() */

#ifdef PIL_IMAGE_READ
/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILAnimate()                                               *
 *                                                                          *
 *  PURPOSE    : BitBlt the new data onto the old page for animation (GIF). *
 *                                                                          *
 ****************************************************************************/
int mPILAnimate(PIL_PAGE *pDestPage, PIL_PAGE *pSrcPage)
{
unsigned char c, c1=0, *s, *d, cTransparent;
int iWidth, sx, x, y, iDestPitch, iSrcPitch;

   if (pSrcPage->iX < 0 || pSrcPage->iY < 0 || (pSrcPage->iX + pSrcPage->iWidth) > pDestPage->iWidth || (pSrcPage->iY + pSrcPage->iHeight) > pDestPage->iHeight)
         return PIL_ERROR_INVPARAM; // bad parameter

   if (pDestPage->cBitsperpixel > 1 && pSrcPage->pPalette) // has a palette
      memcpy(pDestPage->pPalette, pSrcPage->pPalette, 768); // get local color table changes

   switch (pDestPage->cBitsperpixel)
      {
      case 1:
         break;
      case 4:
         iDestPitch = mPILCalcSize(pDestPage->iWidth, 4);
         iSrcPitch = mPILCalcSize(pSrcPage->iWidth, 4);
         c = (pSrcPage->cGIFBits >> 2) & 7; // disposal/removal flag
         if (c == 2) // draw background color over old rectangle
            {
            c = pDestPage->cBackground;
            c |= (c << 4);
            iWidth = iDestPitch * pDestPage->iHeight;
            memset(pDestPage->pData, c, iWidth);
            }
         // Draw new sub-image onto animation bitmap
         s = pSrcPage->pData;
         d = pDestPage->pData + (iDestPitch * pSrcPage->iY);
         cTransparent = pSrcPage->cTransparent;
         if (pSrcPage->cGIFBits & 1) // if transparency used
            {
            for (y=0; y<pSrcPage->iHeight; y++)
               {
               sx = 0;
               for (x=pSrcPage->iX; x<(pSrcPage->iX+pSrcPage->iWidth); x++)
                  {
                  if (!(sx & 1))
                     {
                     c = *s++;
                     c1 = c & 0xf;
                     c >>= 4;
                     if (c != cTransparent)
                        {
                        if (x & 1)
                           {
                           d[x/2] &= 0xf0;
                           d[x/2] |= c;
                           }
                        else
                           {
                           d[x/2] &= 0xf;
                           d[x/2] |= (c << 4);
                           }
                        }
                     }
                  else // odd source pixel
                     {
                     if (c1 != cTransparent)
                        {
                        if (x & 1)
                           {
                           d[x/2] &= 0xf0;
                           d[x/2] |= c1;
                           }
                        else
                           {
                           d[x/2] &= 0xf;
                           d[x/2] |= (c1 << 4);
                           }
                        }
                     }
                  sx++;
                  }
               s += iSrcPitch - (pSrcPage->iWidth+1)/2;
               d += iDestPitch;
               }
            }
         else // no transparency
            {
            for (y=0; y<pSrcPage->iHeight; y++)
               {
               for (x=0; x<pSrcPage->iWidth; x++)
                  {
                  *d++ = *s++;
                  }
               s += iSrcPitch - pSrcPage->iWidth;
               d += iDestPitch - pSrcPage->iWidth;
               }
            }
         break;
      case 8:
         iDestPitch = mPILCalcSize(pDestPage->iWidth, 8);
         iSrcPitch = mPILCalcSize(pSrcPage->iWidth, 8);
         c = (pSrcPage->cGIFBits >> 2) & 7; // disposal/removal flag
         if (c == 2 || c==3) // draw background color over old rectangle
            {
            iWidth = iDestPitch * pDestPage->iHeight;
//            c = pDestPage->pData[0];
            c = pDestPage->cBackground;
            memset(pDestPage->pData, c, iWidth);
            }
         // Draw new sub-image onto animation bitmap
         s = pSrcPage->pData;
         d = pDestPage->pData + (iDestPitch * pSrcPage->iY) + pSrcPage->iX;
         cTransparent = pSrcPage->cTransparent;
         if (pSrcPage->cGIFBits & 1) // if transparency used
            {
            for (y=0; y<pSrcPage->iHeight; y++)
               {
               for (x=0; x<pSrcPage->iWidth; x++)
                  {
                  c = *s++;
                  if (c != cTransparent)
                     *d = c;
                  d++;
                  }
               s += iSrcPitch - pSrcPage->iWidth;
               d += iDestPitch - pSrcPage->iWidth;
               }
            }
         else // no transparency
            {
            for (y=0; y<pSrcPage->iHeight; y++)
               {
               for (x=0; x<pSrcPage->iWidth; x++)
                  {
                  *d++ = *s++;
                  }
               s += iSrcPitch - pSrcPage->iWidth;
               d += iDestPitch - pSrcPage->iWidth;
               }
            }
         break;
      }
   return 0;

} /* mPILAnimate() */
#endif // PIL_IMAGE_READ

// JPEG support starts here

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILGetEXIFInfo(char *, int, int, BOOL, PIL_PAGE *)         *
 *                                                                          *
 *  PURPOSE    : Try to extract useful info from the JPEG EXIF header.      *
 *                                                                          *
 ****************************************************************************/
static void PILGetEXIFInfo(unsigned char *buf, int iEXIF1, int iEXIF2, BOOL bMotorola, PIL_PAGE *outpage)
{
int iCount, iOffset, j, k, x, y;
int iMNOffset, iMNLen, iMarker;
int iMaker = 0;
char szTemp[256];

#ifdef _UNICODE
   mbstowcs(szTemp, outpage->szInfo1, strlen(outpage->szInfo1)+1);
#else
#ifdef _WIN32
   _tcscpy(szTemp, outpage->szInfo1);
#else
	strcpy((char *) szTemp, (char *) outpage->szInfo1);
#endif	// #ifdef _WIN32
#endif	// #ifdef _UNICODE

#ifdef _WIN32
   _tcsupr(szTemp);
   if (_tcscmp(szTemp, TEXT("CANON")) == 0)
      iMaker = 1;
   if (_tcscmp(szTemp, TEXT("SONY")) == 0)
      iMaker = 2;
   if (_tcscmp(szTemp, TEXT("EASTMAN KODAK COMPANY")) == 0)
      iMaker = 3;
   if (_tcscmp(szTemp, TEXT("RICOH      ")) == 0)
      iMaker = 4;
   if (_tcscmp(szTemp, TEXT("NIKON")) == 0) // Nikon Type II
      iMaker = 5;
#else
   strupr((char *) szTemp);
   if (strcmp((char *) szTemp, (char *) TEXT("CANON")) == 0)
      iMaker = 1;
   if (strcmp((char *) szTemp, (char *) TEXT("SONY")) == 0)
      iMaker = 2;
   if (strcmp((char *) szTemp, (char *) TEXT("EASTMAN KODAK COMPANY")) == 0)
      iMaker = 3;
   if (strcmp((char *) szTemp, (char *) TEXT("RICOH      ")) == 0)
      iMaker = 4;
   if (strcmp((char *) szTemp, (char *) TEXT("NIKON")) == 0) // Nikon Type II
      iMaker = 5;
#endif

   iMNLen = 0;
   // Walk the EXIF subIFD for tags we want
   k = iEXIF1 + iEXIF2; // start of IFD
   iCount = PILTIFFSHORT(&buf[k], bMotorola); // tag count
   k += 2;
   if (iCount > 64) // something is very wrong, leave
      return;
   for (j=0; j<iCount; j++) // search for useful info
      {
      iMarker = PILTIFFSHORT(&buf[k], bMotorola);
      switch (iMarker)
         {
         case 0x829a: // shutter speed
            iOffset = PILTIFFVALUE(&buf[k], bMotorola) + iEXIF1;
            if (iOffset < 0x8000)
               {
               x = PILTIFFLONG(&buf[iOffset], bMotorola);
               y = PILTIFFLONG(&buf[iOffset+4], bMotorola);
               if (x != 0)
                  outpage->iShutter = y / x; // reverse rational since it is a fractional value of 1/n
               }
            break;
         case 0x829d: // F number
            iOffset = PILTIFFVALUE(&buf[k], bMotorola) + iEXIF1;
            if (iOffset < 0x8000)
               {
               x = PILTIFFLONG(&buf[iOffset], bMotorola);
               y = PILTIFFLONG(&buf[iOffset+4], bMotorola);
               if (y != 0)
                  outpage->iFStop = (x*100) / y;
               }
            break;
         case 0x8822: // exposure program
            outpage->iExposureProgram = PILTIFFVALUE(&buf[k], bMotorola);
            break;
         case 0x8827: // ISO speed
            outpage->iISO = PILTIFFVALUE(&buf[k], bMotorola);
            break;
         case 0x9003: // ASCII date and time
            x = PILTIFFVALUE(&buf[k], bMotorola) + iEXIF1; /* Get the offset */
            y = PILTIFFLONG(&buf[k+4], bMotorola); /* Get the count */
            if (y> 31)
               y = 31; // max we can handle
            if (y)
               {
               memset(outpage->szDateTime, 0, 32);
               if (y <= 4)
                  memcpy(outpage->szDateTime, &buf[k+8], y);
               else
                  memcpy(outpage->szDateTime, &buf[x], y);
               }
            break;
         case 0x9204: // exposure bias
            iOffset = PILTIFFVALUE(&buf[k], bMotorola) + iEXIF1;
            if (iOffset < 0x8000)
               {
               x = PILTIFFLONG(&buf[iOffset], bMotorola);
               y = PILTIFFLONG(&buf[iOffset+4], bMotorola);
               if (y != 0)
                  outpage->iExposure = (x*10) / y;
               }
            break;
         case 0x9207: // metering mode
            outpage->iMetering = PILTIFFVALUE(&buf[k], bMotorola);
            break;
         case 0x9209: // flash used
            outpage->iFlash = PILTIFFVALUE(&buf[k], bMotorola);
            break;
         case 0x920a: // focal length
            iOffset = PILTIFFVALUE(&buf[k], bMotorola) + iEXIF1;
            if (iOffset < 0x8000)
               {
               x = PILTIFFLONG(&buf[iOffset], bMotorola);
               y = PILTIFFLONG(&buf[iOffset+4], bMotorola);
               if (y != 0)
                  outpage->iFocalLength = (x*10) / y;
               }
            break;
         case 0x927c: // MakerNote - private data unique to each camera
            iMNOffset = PILTIFFLONG(&buf[k+8], bMotorola); // offset to private data
            iMNLen = PILTIFFLONG(&buf[k+4], bMotorola); // length of private data
            break;
         case 0xa002: // true image width
            outpage->iOriginalWidth = PILTIFFVALUE(&buf[k], bMotorola);
            break;
         case 0xa003: // true image height
            outpage->iOriginalHeight = PILTIFFVALUE(&buf[k], bMotorola);
            break;
         case 0xa403: // white balance
            outpage->iWhiteBalance = PILTIFFVALUE(&buf[k], bMotorola);
            break;
         default:
            k |= 0;
            break;
         }
      k += 12; // skip to next tag
      }
   if (iMNLen && strcmp((const char *)&buf[iMNOffset+iEXIF1], "Nikon") == 0) // Nikon type 2
      iMaker = 6;

// If we did not get ISO info, try to get it from MakerNote
   if (outpage->iISO == -1 && iMNLen && iMaker)
      {
      k = iMNOffset + iEXIF1; // start of IFD
      iCount = PILTIFFSHORT(&buf[k], bMotorola); // tag count
      k += 2;
      switch (iMaker)
         {
         case 5: // Nikon type 2
            for (j=0; j<iCount; j++) // search for useful info
               {
               iMarker = PILTIFFSHORT(&buf[k], bMotorola);
               switch (iMarker)
                  {
                  case 2: // ISO
                     outpage->iISO = PILTIFFSHORT(&buf[k+10], bMotorola);
                     break;
                  }
               k += 12;
               }
            break; // Nikon type 2
            
         case 6: // Nikon type 1
            k = 8 + iMNOffset + iEXIF1; // start of IFD
            iCount = PILTIFFSHORT(&buf[k], bMotorola); // tag count
            k += 2;
            for (j=0; j<iCount; j++) // search for useful info
               {
               iMarker = PILTIFFSHORT(&buf[k], bMotorola);
               switch (iMarker)
                  {
                  case 6: // ISO
                     x = PILTIFFSHORT(&buf[k+8], bMotorola);
                     if (x == 0)
                        outpage->iISO = 80;
                     if (x == 2)
                        outpage->iISO = 160;
                     if (x == 4)
                        outpage->iISO = 320;
                     if (x == 5)
                        outpage->iISO = 100;
                     break;
                  }
               k += 12;
               }
            break; // Nikon type 1
            
         case 1: // Canon
            for (j=0; j<iCount; j++) // search for useful info
               {
               iMarker = PILTIFFSHORT(&buf[k], bMotorola);
               switch (iMarker)
                  {
                  case 1: // useful info for Canon
                     iOffset = PILTIFFLONG(&buf[k+8], bMotorola); // offset to private data
                     x = iOffset+iEXIF1+16*2; // short #16
                     if (PILTIFFSHORT(&buf[x], bMotorola) == 15)
                        outpage->iISO = 0; // Auto
                     if (PILTIFFSHORT(&buf[x], bMotorola) == 16)
                        outpage->iISO = 50;
                     if (PILTIFFSHORT(&buf[x], bMotorola) == 17)
                        outpage->iISO = 100;
                     if (PILTIFFSHORT(&buf[x], bMotorola) == 18)
                        outpage->iISO = 200;
                     if (PILTIFFSHORT(&buf[x], bMotorola) == 19)
                        outpage->iISO = 400;
                     break;
                  }
               k += 12;
               }
            break; // CANON
         } // switch on camera maker
      } // makernote
} /* PILGetEXIFInfo() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILReadJPEG(PIL_PAGE *, PIL_PAGE *, int)                   *
 *                                                                          *
 *  PURPOSE    : Convert a buffer of JPEG data into uncompressed BMP.       *
 *                                                                          *
 ****************************************************************************/
static int PILReadJPEG(PIL_PAGE *inpage, PIL_PAGE *outpage, int iOptions, BOOL bMJPEG)
{
//int iMarker;
int lsize, i, k, iErr, iOff, iBit;
//int iRestart = 0; /* Restart interval */
//int iNumComps = 0; /* Number of color components */
signed short *pMCU=NULL, *pMCUData;
unsigned char *buf, *cOutput;
JPEGDATA *pJPEG;
unsigned short us;
int iSeekErr;
int iNumBytes;
BOOL bThumbnail = FALSE;

   iErr = PIL_ERROR_SUCCESS;
   if (iOptions & (PIL_CONVERT_THUMBNAIL | PIL_CONVERT_EIGHTHSIZE))
      bThumbnail = TRUE;
   if (bThumbnail)
      { // shrink image to size of macro blocks
      outpage->iWidth = (inpage->iWidth+7) >> 3;
      outpage->iHeight = (inpage->iHeight+7) >> 3;
      inpage->iWidth = outpage->iWidth;
      inpage->iHeight = outpage->iHeight;
      }
   if (iOptions & PIL_CONVERT_16BPP) // force RGB565 output from either RGB888 or grayscale-8
      {
      outpage->cBitsperpixel = 16;
      inpage->cBitsperpixel = 16;
      }
   if (iOptions & PIL_CONVERT_HALFSIZE)
      {
      outpage->iWidth = (inpage->iWidth+1) >> 1;
      outpage->iHeight = (inpage->iHeight+1) >> 1;
      }
   if (iOptions & PIL_CONVERT_QUARTERSIZE)
      {
      outpage->iWidth = (inpage->iWidth+3) >> 2;
      outpage->iHeight = (inpage->iHeight+3) >> 2;
      }
   if (iOptions & PIL_CONVERT_NOALLOC)
      {
      cOutput = outpage->pData;
      lsize = outpage->iPitch;
      if (lsize == 0)
         lsize = mPILCalcSize(outpage->iWidth, outpage->cBitsperpixel);
      }
   else
      {
      lsize = mPILCalcSize(outpage->iWidth, outpage->cBitsperpixel);
      inpage->iPitch = outpage->iPitch = lsize;
      i = (outpage->iHeight + 15) & 0xfff0; // make sure it's a multiple of 16-lines tall
      i *= lsize; /* color bitmap size */
      cOutput = MiniIOAlloc(i); /* Output buffer is actual bitmap */
//      cOutput = VirtualAlloc(NULL, i, MEM_COMMIT, PAGE_NOCACHE | PAGE_READWRITE);
      if (cOutput == NULL)
         return PIL_ERROR_MEMORY;
      outpage->pData = (unsigned char *)cOutput;
      outpage->iDataSize = i;
      }
   outpage->iOffset = 0;
   outpage->cState = PIL_PAGE_STATE_LOADED;
   outpage->cFlags = PIL_PAGEFLAGS_TOPDOWN;
   pMCUData = NULL; 
   if (bMJPEG || (iOptions & PIL_CONVERT_NOALLOC))
      pJPEG = inpage->pJPEG; // pass the precalced Huffman table data here
   else
      {
      pJPEG = mPILPrepJPEGStruct(); /* jpeg tables */
      if (pJPEG == NULL)
         {
         iErr = PIL_ERROR_MEMORY;
         goto getjpgz;
         }
      }
   pJPEG->iOptions = iOptions;
//   pJPEG->pTables = pTables; // fast pixel conversion tables
//   memset(pJPEG,0,sizeof(JPEGDATA));
   for (i=0; i<3; i++)
      {
      pJPEG->JPCI[i].h_samp_factor = 1;
      pJPEG->JPCI[i].v_samp_factor = 1;
      }

   if (inpage->iOffset != 0 && inpage->iHandle != 0) // need to re-read the starting buffer
      {
      iSeekErr = MiniIOSeek(inpage->iHandle, inpage->iOffset, 0);
	  if (iSeekErr < 0)
	  {
		  iErr = PIL_ERROR_IO;
		  goto getjpgz;
	  }

      inpage->iFilePos = inpage->iOffset + PIL_BUFFER_SIZE; // next read pos
      iNumBytes = MiniIORead(inpage->iHandle, inpage->pData, PIL_BUFFER_SIZE);
	  if ((iNumBytes < 0) || (iNumBytes != PIL_BUFFER_SIZE))
	  {
		  iErr = PIL_ERROR_IO;
		  goto getjpgz;
	  }

      inpage->iOffset = 0;
      }
   buf = inpage->pData; // point to raw JPEG data
   iOff = inpage->iOffset;
   iBit = 0;  /* Pointer into data stream */
   us = MOTOSHORT(buf); /* Do we start with an SOI marker? */
   if (us != 0xffd8)
      {
      us = MOTOSHORT(&buf[1]); // Ricoh files have a strange extra byte here
      if (us == 0xffd8)
         iOff += 3;
      }
   else
      iOff += 2;
   if (us != 0xffd8) /* Error not a JPEG file */
      {
      iErr = PIL_ERROR_BADHEADER;
      goto getjpgz;
      }
   outpage->iXres = inpage->iYres = 1; /* assume no valid resolution info */
   JPEGGetSOI(pJPEG); /* Process Start Of Image info */
   i = JPEGProcessTables(buf, &iOff, pJPEG, inpage->iDataSize);
   strcpy(outpage->szComment, pJPEG->szComment); // copy any comments from JPEG header
   // We don't have TIFF code in the MINI_PIL, so don't try to read EXIF info
   if (0) //pJPEG->iEXIF) // ancillary info and thumbnail image in JPEG header, get it !
      {
      PIL_FILE pfTemp;
      PIL_PAGE ppTemp; // load the thumbnail from the embedded TIFF file
      BOOL bMotorola;
      int iMarker, iError, iTags;
      unsigned char *pData;
      memset(&pfTemp, 0, sizeof(PIL_FILE));
      memset(&ppTemp, 0, sizeof(PIL_PAGE));
      // Copy over the page info for thumbnail display's use
      ppTemp.iPageWidth = inpage->iPageWidth;
      ppTemp.iPageHeight = inpage->iPageHeight;
      ppTemp.iX = inpage->iX;
      ppTemp.iGIFDelay = inpage->iGIFDelay;
      ppTemp.lUser = inpage->lUser;
      ppTemp.pJPEG = inpage->pJPEG;
      // Pretend the embedded TIFF file is a separate, memory-mapped file
      bMotorola = (buf[pJPEG->iEXIF] == 'M');
      pfTemp.pData = &buf[pJPEG->iEXIF];
      k = pJPEG->iEXIF - 8;
      pfTemp.iFileSize = MOTOSHORT(&buf[k]); // use the App1 marker length as a fake file length
      pfTemp.cFileType = PIL_FILE_TIFF;
      pfTemp.cState = PIL_FILE_STATE_OPEN;
      pfTemp.iPageTotal = 2;
      pfTemp.pPageList = (int *)pJPEG->iScanOffset; // thumbnails don't encode as progressive, so we can borrow this space
      pfTemp.pPageList[0] = PILTIFFLONG(&buf[pJPEG->iEXIF+4], bMotorola); // offset to first IFD
      iTags = PILTIFFSHORT(&buf[pJPEG->iEXIF+pfTemp.pPageList[0]], bMotorola);  /* Number of tags in this dir */
      pfTemp.pPageList[1] = PILTIFFLONG(&buf[pJPEG->iEXIF+pfTemp.pPageList[0]+2+iTags*12], bMotorola); // IFD of second page
      ppTemp.iGIFDelay = 0; // assume no extra EXIF info
      iError = mPILRead(&pfTemp, &ppTemp, 0, 0); // Read the text info and EXIF offset from the first page
      outpage->iOrientation = ppTemp.iOrientation; // transfer JPEG rotation info to output page
      if (ppTemp.iGIFDelay) // EXIF info included, get it
         {
         if (ppTemp.szInfo1[0])
            strcpy(outpage->szInfo1, ppTemp.szInfo1);
         PILGetEXIFInfo(buf, pJPEG->iEXIF, ppTemp.iGIFDelay, bMotorola, outpage);
         }
      if (iOptions & PIL_CONVERT_THUMBNAIL) // if we are reading a thumbnail image, try to get it here
         {
         iError = mPILRead(&pfTemp, &ppTemp, 1, 0); // Read the image info for the thumbnail page
         if (iError)
            goto normal_jpeg; // try to read a 1/64 sized version if the thumbnail image is bad
         outpage->cCompression = PIL_COMP_NONE; // decompress
         // Get the image width and height - without these, loading will fail
         if (ppTemp.cCompression == PIL_COMP_JPEG) // if JPEG compressed thumbnail, extract size
            {
            k = 2; /* Start at offset of first marker */
            iMarker = 0; /* Search for SOF (start of frame) marker */
            pData = &ppTemp.pData[ppTemp.iOffset];
            while (k < ppTemp.iDataSize)
               {
               iMarker = MOTOSHORT(&pData[k]) & 0xfffc;
               k += 2;
               if (iMarker < 0xff00) // invalid marker, could be generated by "Arles Image Web Page Creator" or Accusoft
                  continue; // skip 2 bytes and try to resync
               if (iMarker == 0xffc0)
                  break;
               k += MOTOSHORT(&pData[k]); /* Skip to next marker */
               }
            if (iMarker != 0xffc0) // something went wrong, try to load the normal way
               {
               goto normal_jpeg; // try to load it normally
               }
            else
               {
               ppTemp.iHeight = MOTOSHORT(&pData[k+3]);
               ppTemp.iWidth = MOTOSHORT(&pData[k+5]);
               if (pData[k+7] == 1) /* number of components */
                  {
                  ppTemp.cBitsperpixel = 8; /* grayscale */
                  // Create a gray palette
                  ppTemp.pPalette = PILGrayPalette(8);
                  }
               else
                  ppTemp.cBitsperpixel = 24;
               }
            // Isn't recursion great? :)
            ppTemp.pData = buf;
            ppTemp.iOffset += pJPEG->iEXIF; // add the EXIF offset to the thumbnail
            ppTemp.iHandle = inpage->iHandle; // read the file directly
            iError = mPILConvert(&ppTemp, outpage, PIL_CONVERT_16BPP); // we want a thumbnail...
            ppTemp.pData = NULL; // don't free the data buffer since it is from the original page object
            mPILFree(&ppTemp); // free this page because it has been "filtered" and a new data buffer was allocated
            if (iError)
               goto normal_jpeg; // try to load a 1/64th version if EXIF thumbnail fails
            }
         else // uncompressed thumbnail, convert to 16bpp
            {
            if (ppTemp.cBitsperpixel == 24)
               mPILModify(&ppTemp, PIL_MODIFY_COLORS, 16, PIL_COLORS_BEST);
            memcpy(outpage, &ppTemp, sizeof(PIL_PAGE)); // use the page as-is
            }
         MiniIOFree(pJPEG);
         MiniIOFree(pMCUData);
         MiniIOFree(cOutput); // don't need this buffer since we decompressed another image
         outpage->iPitch = mPILCalcSize(outpage->iWidth, outpage->cBitsperpixel); // recalc pitch
         return iError; // we are done!
         }
      else // copy any text info from the EXIF header
         {
         if (ppTemp.szInfo1[0])
            strcpy(outpage->szInfo1, ppTemp.szInfo1);
         if (ppTemp.szInfo2[0])
            strcpy(outpage->szInfo2, ppTemp.szInfo2);
         if (ppTemp.szComment[0])
            strcpy(outpage->szComment, ppTemp.szComment);
         }
      }
normal_jpeg:
   if (!bThumbnail) // use the real image size instead of the EXIF size which may be wrong
      {
      outpage->iOriginalWidth = inpage->iWidth;
      outpage->iOriginalHeight = inpage->iHeight;
      }
   if (i != 0xc0 && i != 0xc2) // we only handle baseline DCT jpeg and progressive DCT
      {
      iErr = PIL_ERROR_UNSUPPORTED;
      goto getjpgz;
      }
   if (i == 0xc2)
      pJPEG->iOptions |= PIL_CONVERT_PROGRESSIVE; // mark this as a progressive JPEG

   if (JPEGGetSOF(buf, &iOff, pJPEG)) // get start of frame marker
      {
      iErr = PIL_ERROR_BADHEADER;
      goto getjpgz;
      }
   if (pJPEG->ucNumComponents != 1 && pJPEG->ucNumComponents != 3) // cmyk or other unsupported color space
      {
      iErr = PIL_ERROR_UNSUPPORTED;
      goto getjpgz;
      }
   if (pJPEG->ucNumComponents == 1) // must be grayscale
      {
      if (pJPEG->iOptions & PIL_CONVERT_16BPP)
         outpage->cBitsperpixel = 16;
      else
         {
         outpage->cBitsperpixel = 8;
         outpage->pPalette = PILGrayPalette(8);
         }
      }
   if (pJPEG->ucNumComponents == 3) // color
      {
      if (pJPEG->iOptions & PIL_CONVERT_16BPP)
         outpage->cBitsperpixel = 16;
      else
         outpage->cBitsperpixel = 24;
      }
   if (JPEGProcessTables(buf, &iOff, pJPEG, inpage->iDataSize) != 0xda) // check for M_SOS
      {
      iErr = PIL_ERROR_BADHEADER;
      goto getjpgz;
      }
   pJPEG->iScan = 0; // get info ready for first scan (of multiscan progressive file)
   pJPEG->iHuffOffset[0] = 0; // use already-loaded Huff table for first scan
   pJPEG->iScanOffset[0] = 0; // first scan offset is always 0
   JPEGGetSOS(buf, &iOff, pJPEG);
   pJPEG->iScan++; // at least 1 scan
   // create huffman tables
   if (!bMJPEG)
      JPEGMakeHuffTables(pJPEG);
   if (pJPEG->iOptions & PIL_CONVERT_PROGRESSIVE)
      {
#ifdef JPEG_PROGRESSIVE
      iErr = JPEGFilter(inpage, iOff, pJPEG); // start with a clean data stream to decode
      iScanTotal = pJPEG->iScan; // total number of scans (for progressive)
      iOff = 0;
      buf = inpage->pData; // pointer to new data buffer
      if (iErr)
         goto getjpgz;
#endif // JPEG_PROGRESSIVE
      }
   iBit = 0;  // point to start of image data stream
   pJPEG->iDataSize = inpage->iOffset + inpage->iDataSize;
   iOff += inpage->iOffset; // start at real data 
   inpage->iOffset = 0;
   pJPEG->ulBits = JPEGGet32Bits(buf, &iOff); // start with 32-bits of huffman data
// Loop through all of the MCU's and decode the image
// reorder and fix the quantization table for decoding
   JPEGFixQuantD(pJPEG);

   if (!(bMJPEG || (iOptions & PIL_CONVERT_NOALLOC)))
      JPEGInitTables(pJPEG);
   if (bThumbnail && !(pJPEG->iOptions & PIL_CONVERT_PROGRESSIVE))
      {
      iErr = GetJPEGxxThumb(inpage, &iOff, &iBit, &pJPEG->pMCUs[0], pJPEG, cOutput);
      }
   else
      {
      if (pJPEG->iOptions & PIL_CONVERT_PROGRESSIVE)
         {
#ifdef JPEG_PROGRESSIVE
         if (bThumbnail)
            i = pJPEG->ucNumComponents * inpage->iWidth * inpage->iHeight;
         else
            i = pJPEG->ucNumComponents * ((inpage->iWidth + 7)/8) * ((inpage->iHeight+7)/8);
         pMCU = pMCUData = MiniIOAlloc((i + 32)*DCTSIZE2*sizeof(short)); /* Allocate space for MCU buffers */
         if (pMCU == NULL)
            {
            iErr = PIL_ERROR_MEMORY;
            goto getjpgz;
            }
         if (bThumbnail)
            iScanTotal = 1; // only run the first scan for thumbnail images
         for (iScan = 0; iScan < iScanTotal && !iErr; iScan++)
            {
            iOff = pJPEG->iScanOffset[iScan]; // offset of jpeg data to decode
//               pJPEG->ulBits = JPEGGet32Bits(inpage->pData, &iOff); // start with 32-bits of huffman data
            if (iScan != 0) // we already have data for the first scan
               JPEGGetSOS(inpage->pData, &iOff, pJPEG); // Get scan-specific info for progressive JPEGs
            iBit = 0;  // each scan starts at bit 0
            k = pJPEG->iHuffOffset[iScan];
            if (k) // if there is a new Huffman table, use it
               {
               PILFreeHuffTables(pJPEG);  // free the old tables
               i = MOTOSHORT(&inpage->pData[k]); // get the huff data length
               i -= 2; // minus the length itself
               JPEGGetHuffTables(&inpage->pData[k+2], i, pJPEG); // get the new Huffman table(s)
               JPEGMakeHuffTables(pJPEG); // Create the new decode tables
               }
            switch (pJPEG->jpegsample) // call the correct code based on color subsampling
               {
               case 0x00: // grayscale
                  iErr = GetJPEGGrayP(inpage, &iOff, &iBit, pMCU, pJPEG, cOutput);
                  break;
               case 0x22: // subsampled in both directions
                  iErr = GetJPEG22P(inpage, &iOff, &iBit, pMCU, pJPEG, cOutput);
                  break;
               case 0x11: // no subsampling
                  iErr = GetJPEG11P(inpage, &iOff, &iBit, pMCU, pJPEG, cOutput);
                   break;
               case 0x12: // 2:1 vertical
                  iErr = GetJPEG12P(inpage, &iOff, &iBit, pMCU, pJPEG, cOutput);
                  break;
               case 0x21: // 2:1 horizontal
                  iErr = GetJPEG21P(inpage, &iOff, &iBit, pMCU, pJPEG, cOutput);
                  break;
               }
            }
         // Now that we have decoded the scans, convert the MCUs into an image
         switch (pJPEG->jpegsample) // call the correct code based on color subsampling
            {
            case 0x00: // grayscale
               DrawJPEGGrayP(inpage, pMCU, pJPEG, cOutput);
               break;
            case 0x22: // subsampled in both directions
               DrawJPEG22(inpage, pMCU, pJPEG, cOutput);
               break;
            case 0x11: // no subsampling
               DrawJPEG11(inpage, pMCU, pJPEG, cOutput);
                break;
            case 0x12: // 2:1 vertical
               DrawJPEG12(inpage, pMCU, pJPEG, cOutput);
               break;
            case 0x21: // 2:1 horizontal
               DrawJPEG21(inpage, pMCU, pJPEG, cOutput);
               break;
            }
#endif // JPEG_PROGRESSIVE
         }
      else
         {
         pMCU = pMCUData = &pJPEG->pMCUs[0];
         iBit = 0;  // each scan starts at bit 0
         iErr = GetJPEGxx(inpage, &iOff, &iBit, pMCU, pJPEG, cOutput);
         }
      }

getjpgz:
   if (pJPEG->iOptions & PIL_CONVERT_PROGRESSIVE)
      MiniIOFree(pMCU);
   if (!bMJPEG)
      {
      PILFreeHuffTables(pJPEG);
      if (!(iOptions & PIL_CONVERT_NOALLOC))
         MiniIOFree(pJPEG);
      }
   outpage->iPitch = mPILCalcSize(outpage->iWidth, outpage->cBitsperpixel); // recalc pitch
   if ((iOptions & PIL_CONVERT_IGNORE_ERRORS)  && iErr == PIL_ERROR_DECOMP)
      iErr = 0; // try to load the corrupt page
   if (iErr && !(iOptions & PIL_CONVERT_NOALLOC)) /* Error, free the image data too */
      {
      MiniIOFree(cOutput);
      if (outpage->cBitsperpixel == 8)
         MiniIOFree(outpage->pPalette);
      }
   return iErr;

} /* PILReadJPEG() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGFixQuantD(JPEGDATA *)                                  *
 *                                                                          *
 *  PURPOSE    : Fix and reorder the quantization table for faster decoding.*
 *                                                                          *
 ****************************************************************************/
static void JPEGFixQuantD(JPEGDATA *pJPEG)
{
int iTable, iTableOffset;
signed short sTemp[DCTSIZE2];
int i;
unsigned short *p;

   for (iTable=0; iTable<pJPEG->ucNumComponents; iTable++)
      {
      iTableOffset = iTable * DCTSIZE2;
      p = &pJPEG->sQuantTable[iTableOffset];
      if (pJPEG->iOptions & PIL_CONVERT_THUMBNAIL) // don't need to do the whole table
         {
         p[0] = (unsigned short)((p[0] * iScaleBits[0]) >> 12);
         }
      else
         {
         for (i=0; i<DCTSIZE2; i++)
             {
             sTemp[i] = p[cZigZag[i]];
             }
         memcpy(&pJPEG->sQuantTable[iTableOffset], sTemp, DCTSIZE2*sizeof(short)); // copy back to original spot

   // Prescale for DCT multiplication
         p = &pJPEG->sQuantTable[iTableOffset];
         for (i=0; i<DCTSIZE2; i++)
            {
            p[i] = (unsigned short)((p[i] * iScaleBits[i]) >> 12);
            }
         }
      }
} /* JPEGFixQuantD() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGPixel(...)                                             *
 *                                                                          *
 *  PURPOSE    : Output a 16bpp or 24bpp color pixel from YCbCr info.       *
 *                                                                          *
 ****************************************************************************/
static void JPEGPixel(JPEGDATA *pJPEG, unsigned char *pDest, int x, int iY, int iCb, int iCr)
{
int iCBB, iCBG, iCRG, iCRR;
unsigned short usPixel;

   iCBB = 7258  * (iCb-0x80);
   iCBG = -1409 * (iCb-0x80);
   iCRG = -2925 * (iCr-0x80);
   iCRR = 5742  * (iCr-0x80);
   if (pJPEG->iOptions & PIL_CONVERT_16BPP)
      {
      usPixel = pJPEG->usRangeTableB[((iCBB + iY) >> 12) & 0x3ff]; // blue pixel
      usPixel |= pJPEG->usRangeTableG[((iCBG + iCRG + iY) >> 12) & 0x3ff]; // green pixel
      usPixel |= pJPEG->usRangeTableR[((iCRR + iY) >> 12) & 0x3ff]; // red pixel
      *(short *)&pDest[x<<1] = usPixel;
      }
   else
      {
      pDest += x*3;
//      i = ((iCBB + iY) >> 12);
//      if (i > 255) i = 255;
//      if (i < 0) i = 0;
//      *pDest++ = (unsigned char)i;
//      i = ((iCBG + iCRG + iY) >> 12);
//      if (i > 255) i = 255;
//      if (i < 0) i = 0;
//      *pDest++ = (unsigned char)i;
//      i = ((iCRR + iY) >> 12);
//      if (i > 255) i = 255;
//      if (i < 0) i = 0;
//      *pDest++ = (unsigned char)i;
      *pDest++ = pJPEG->cRangeTable2[((iCBB + iY) >> 12) & 0x3ff]; // blue pixel
      *pDest++ = pJPEG->cRangeTable2[((iCBG + iCRG + iY) >> 12) & 0x3ff]; // green pixel
      *pDest++ = pJPEG->cRangeTable2[((iCRR + iY) >> 12) & 0x3ff]; // red pixel
      }

} /* JPEGPixel() */

/*****************************************************************************
 *                                                                           *
 *  FUNCTION   : JPEGPixel2(...)                                             *
 *                                                                           *
 *  PURPOSE    : Output a pair of 16bpp or 24bpp color pixel from YCbCr info.*
 *                                                                           *
 *****************************************************************************/
static void JPEGPixel2(JPEGDATA *pJPEG, unsigned char *pDest, int x, int iY1, int iY2, int iCb, int iCr)
{
int iCBB, iCBG, iCRG, iCRR;
unsigned long ulPixel1, ulPixel2;

   iCBB = 7258  * (iCb-0x80);
   iCBG = -1409 * (iCb-0x80);
   iCRG = -2925 * (iCr-0x80);
   iCRR = 5742  * (iCr-0x80);
   if (pJPEG->iOptions & PIL_CONVERT_16BPP)
      {
      ulPixel1 = pJPEG->usRangeTableB[((iCBB + iY1) >> 12) & 0x3ff]; // blue pixel
      ulPixel1 |= pJPEG->usRangeTableG[((iCBG + iCRG + iY1) >> 12) & 0x3ff]; // green pixel
      ulPixel1 |= pJPEG->usRangeTableR[((iCRR + iY1) >> 12) & 0x3ff]; // red pixel

      ulPixel2 = pJPEG->usRangeTableB[((iCBB + iY2) >> 12) & 0x3ff]; // blue pixel
      ulPixel2 |= pJPEG->usRangeTableG[((iCBG + iCRG + iY2) >> 12) & 0x3ff]; // green pixel
      ulPixel2 |= pJPEG->usRangeTableR[((iCRR + iY2) >> 12) & 0x3ff]; // red pixel
      *(long *)&pDest[x<<1] = ulPixel1 | (ulPixel2<<16);
      }
   else
      {
      pDest += x*3;
      *pDest++ = pJPEG->cRangeTable2[((iCBB + iY1) >> 12) & 0x3ff]; // blue pixel
      *pDest++ = pJPEG->cRangeTable2[((iCBG + iCRG + iY1) >> 12) & 0x3ff]; // green pixel
      *pDest++ = pJPEG->cRangeTable2[((iCRR + iY1) >> 12) & 0x3ff]; // red pixel

      *pDest++ = pJPEG->cRangeTable2[((iCBB + iY2) >> 12) & 0x3ff]; // blue pixel
      *pDest++ = pJPEG->cRangeTable2[((iCBG + iCRG + iY2) >> 12) & 0x3ff]; // green pixel
      *pDest++ = pJPEG->cRangeTable2[((iCRR + iY2) >> 12) & 0x3ff]; // red pixel
      }

} /* JPEGPixel2() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGPixel(...)                                             *
 *                                                                          *
 *  PURPOSE    : Output a 16bpp or 24bpp color pixel from YCbCr info.       *
 *                                                                          *
 ****************************************************************************/
static void JPEGPixel_A(JPEGDATA *pJPEG, unsigned char *pDest, int x, int iY, int iCb, int iCr)
{
int iCBB, iCBG, iCRG, iCRR;
unsigned short usPixel;

   iCBB = 7258  * iCb;
   iCBG = -1409 * iCb;
   iCRG = -2925 * iCr;
   iCRR = 5742  * iCr;
   if (pJPEG->iOptions & PIL_CONVERT_16BPP)
      {
      usPixel = pJPEG->usRangeTableB[((iCBB + iY) >> 12) & 0x3ff]; // blue pixel
      usPixel |= pJPEG->usRangeTableG[((iCBG + iCRG + iY) >> 12) & 0x3ff]; // green pixel
      usPixel |= pJPEG->usRangeTableR[((iCRR + iY) >> 12) & 0x3ff]; // red pixel
      *(short *)&pDest[x<<1] = usPixel;
      }
   else
      {
      pDest += x*3;
//      i = ((iCBB + iY) >> 12);
//      if (i > 255) i = 255;
//      if (i < 0) i = 0;
//      *pDest++ = (unsigned char)i;
//      i = ((iCBG + iCRG + iY) >> 12);
//      if (i > 255) i = 255;
//      if (i < 0) i = 0;
//      *pDest++ = (unsigned char)i;
//      i = ((iCRR + iY) >> 12);
//      if (i > 255) i = 255;
//      if (i < 0) i = 0;
//      *pDest++ = (unsigned char)i;
      *pDest++ = pJPEG->cRangeTable2[((iCBB + iY) >> 12) & 0x3ff]; // blue pixel
      *pDest++ = pJPEG->cRangeTable2[((iCBG + iCRG + iY) >> 12) & 0x3ff]; // green pixel
      *pDest++ = pJPEG->cRangeTable2[((iCRR + iY) >> 12) & 0x3ff]; // red pixel
      }

} /* JPEGPixel_A() */

/*****************************************************************************
 *                                                                           *
 *  FUNCTION   : JPEGPixel2(...)                                             *
 *                                                                           *
 *  PURPOSE    : Output a pair of 16bpp or 24bpp color pixel from YCbCr info.*
 *                                                                           *
 *****************************************************************************/
static void JPEGPixel2_A(JPEGDATA *pJPEG, unsigned char *pDest, int x, int iY1, int iY2, int iCb, int iCr)
{
int iCBB, iCBG, iCRG, iCRR;
unsigned long ulPixel1, ulPixel2;

   iCBB = 7258  * iCb;
   iCBG = -1409 * iCb;
   iCRG = -2925 * iCr;
   iCRR = 5742  * iCr;
   if (pJPEG->iOptions & PIL_CONVERT_16BPP)
      {
      ulPixel1 = pJPEG->usRangeTableB[((iCBB + iY1) >> 12) & 0x3ff]; // blue pixel
      ulPixel1 |= pJPEG->usRangeTableG[((iCBG + iCRG + iY1) >> 12) & 0x3ff]; // green pixel
      ulPixel1 |= pJPEG->usRangeTableR[((iCRR + iY1) >> 12) & 0x3ff]; // red pixel

      ulPixel2 = pJPEG->usRangeTableB[((iCBB + iY2) >> 12) & 0x3ff]; // blue pixel
      ulPixel2 |= pJPEG->usRangeTableG[((iCBG + iCRG + iY2) >> 12) & 0x3ff]; // green pixel
      ulPixel2 |= pJPEG->usRangeTableR[((iCRR + iY2) >> 12) & 0x3ff]; // red pixel
      *(long *)&pDest[x<<1] = ulPixel1 | (ulPixel2<<16);
      }
   else
      {
      pDest += x*3;
      *pDest++ = pJPEG->cRangeTable2[((iCBB + iY1) >> 12) & 0x3ff]; // blue pixel
      *pDest++ = pJPEG->cRangeTable2[((iCBG + iCRG + iY1) >> 12) & 0x3ff]; // green pixel
      *pDest++ = pJPEG->cRangeTable2[((iCRR + iY1) >> 12) & 0x3ff]; // red pixel

      *pDest++ = pJPEG->cRangeTable2[((iCBB + iY2) >> 12) & 0x3ff]; // blue pixel
      *pDest++ = pJPEG->cRangeTable2[((iCBG + iCRG + iY2) >> 12) & 0x3ff]; // green pixel
      *pDest++ = pJPEG->cRangeTable2[((iCRR + iY2) >> 12) & 0x3ff]; // red pixel
      }

} /* JPEGPixel2_A() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : GetJPEGxxThumb(...)                                        *
 *                                                                          *
 *  PURPOSE    : Decode loop for JPEG images to be loaded as a thumbnail.   *
 *               The DC values from each MCU are used to make pixels.       *
 *                                                                          *
 ****************************************************************************/
static int GetJPEGxxThumb(PIL_PAGE *inpage, int *iOff, int *iBit, signed short *pMCU, JPEGDATA *pJPEG, unsigned char *cOutput)
{
int cx, cy, x, y, lsize;
unsigned long Cb,Cr;
signed int Y1,Y2,Y3,Y4;
signed int iDCPred0, iDCPred1, iDCPred2;
signed int iQuant1, iQuant2;
//unsigned short *usOutput;
int iErr;
unsigned char cACTable0, cDCTable0, cACTable1, cDCTable1, cACTable2, cDCTable2;

    cDCTable0 = pJPEG->JPCI[0].dc_tbl_no;
    cACTable0 = pJPEG->JPCI[0].ac_tbl_no;
    cDCTable1 = pJPEG->JPCI[1].dc_tbl_no;
    cACTable1 = pJPEG->JPCI[1].ac_tbl_no;
    cDCTable2 = pJPEG->JPCI[2].dc_tbl_no;
    cACTable2 = pJPEG->JPCI[2].ac_tbl_no;

    lsize = mPILCalcSize(inpage->iWidth, inpage->cBitsperpixel);
    iDCPred0 = iDCPred1 = iDCPred2 = 0;
// set up the parameters for the different subsampling options
    if (pJPEG->jpegsample == 0 || pJPEG->jpegsample == 0x11 || pJPEG->jpegsample == 0x12)
       cx = (pJPEG->cx + 7) >> 3;
    else
       cx = (pJPEG->cx + 15) >> 4;
    if (pJPEG->jpegsample == 0 || pJPEG->jpegsample == 0x11 || pJPEG->jpegsample == 0x21)
       cy = (pJPEG->cy + 7) >> 3;
    else
       cy = (pJPEG->cy + 15) >> 4;
    // faster access with reg vars
    iQuant1 = pJPEG->sQuantTable[0];
    iQuant2 = pJPEG->sQuantTable[DCTSIZE2];
    Y1 = Y2 = Y3 = Y4 = 0; // suppress compiler warning
    for (y=0; y<cy; y++)
       {
//       if (!(y & 15) && pfnProgress) // call progress function to display current progress
//          {
//          if ((*pfnProgress)(y, cy) == PIL_PROGRESS_CANCEL)
//             return PIL_ERROR_CANCELED;
//          }

       for (x=0; x<cx; x++)
          {
          if (inpage->cState == PIL_PAGE_STATE_OPEN && *iOff > PIL_BUFFER_HIGHWATER) // need to read more data
             PILReadBlock(inpage, iOff); // read the next chunk of data
//          if (*iOff > pJPEG->iDataSize)  // went past end of data
//             return PIL_ERROR_DECOMP;
          pJPEG->pHuffACFast = pJPEG->huffacFast[cACTable0];
          pJPEG->pHuffDCFast = pJPEG->huffdcFast[cDCTable0];
          // do the luminance components
          iErr = JPEGDecodeMCUFast(inpage->pData, iOff, iBit, pJPEG, &iDCPred0);
          Y1 = iDCPred0;
          if (pJPEG->jpegsample > 0x11) // color subsampled image
             {
             iErr |= JPEGDecodeMCUFast(inpage->pData, iOff, iBit, pJPEG, &iDCPred0);
             Y2 = iDCPred0;
             if (pJPEG->jpegsample == 0x22) // 2:2 has 4 Lums
                {
                iErr |= JPEGDecodeMCUFast(inpage->pData, iOff, iBit, pJPEG, &iDCPred0);
                Y3 = iDCPred0;
                iErr |= JPEGDecodeMCUFast(inpage->pData, iOff, iBit, pJPEG, &iDCPred0);
                Y4 = iDCPred0;
                }
             }
          if (pJPEG->jpegsample > 0) // if color image
             {
             // first chroma
             pJPEG->pHuffDCFast = pJPEG->huffdcFast[cDCTable1];
             pJPEG->pHuffACFast = pJPEG->huffacFast[cACTable1];
             iErr |= JPEGDecodeMCUFast(inpage->pData, iOff, iBit, pJPEG, &iDCPred1);
             // second chroma
             pJPEG->pHuffDCFast = pJPEG->huffdcFast[cDCTable2];
             pJPEG->pHuffACFast = pJPEG->huffacFast[cACTable2];
             iErr |= JPEGDecodeMCUFast(inpage->pData, iOff, iBit, pJPEG, &iDCPred2);
             }
          if (iErr)
             return iErr;  // decoding error, stop
          switch (pJPEG->jpegsample) // custom pixel routines for each type of image
             {
             case 0x00: // grayscale
                if (pJPEG->iOptions & PIL_CONVERT_16BPP) // want 16bpp grayscale output
                   {
                   unsigned char c;
                   unsigned short us;
                   c = pJPEG->cRangeTable[((iDCPred0 * iQuant1)>>5) & 0x3ff];
                   us = (c >> 3) | ((c>>2)<<5) | ((c>>3)<<11);
                   *(unsigned short *)&cOutput[(x<<1)] = us;
                   }
                else
                   cOutput[x] = pJPEG->cRangeTable[((iDCPred0 * iQuant1)>>5) & 0x3ff];
                break;
             case 0x11: // 1:1 color
                Y1 = (((Y1 * iQuant1)>>5)+0x80) << 12;
                Cb = ((iDCPred1 * iQuant2)>>5);
                Cr = ((iDCPred2 * iQuant2)>>5);
                JPEGPixel_A(pJPEG, cOutput, x, Y1, Cb, Cr);
                break;
             case 0x12: // 1:2 color
                Y1 = (((Y1 * iQuant1)>>5)+0x80) << 12;
                Y2 = (((Y2 * iQuant1)>>5)+0x80) << 12;
                Cb = ((iDCPred1 * iQuant2)>>5);
                Cr = ((iDCPred2 * iQuant2)>>5);
                JPEGPixel_A(pJPEG, cOutput, x, Y1, Cb, Cr);
                JPEGPixel_A(pJPEG, &cOutput[lsize], x, Y2, Cb, Cr);
                break;
             case 0x21: // 2:1 color
                Y1 = (((Y1 * iQuant1)>>5)+0x80) << 12;
                Y2 = (((Y2 * iQuant1)>>5)+0x80) << 12;
                Cb = ((iDCPred1 * iQuant2)>>5);
                Cr = ((iDCPred2 * iQuant2)>>5);
                JPEGPixel2_A(pJPEG, cOutput, x<<1, Y1, Y2, Cb, Cr);
                break;
             case 0x22: // 2:2 color
                Y1 = (((Y1 * iQuant1)>>5)+0x80)<<12;
                Y2 = (((Y2 * iQuant1)>>5)+0x80)<<12;
                Y3 = (((Y3 * iQuant1)>>5)+0x80)<<12;
                Y4 = (((Y4 * iQuant1)>>5)+0x80)<<12;
                Cb = ((iDCPred1 * iQuant2)>>5);
                Cr = ((iDCPred2 * iQuant2)>>5);
                JPEGPixel2_A(pJPEG, cOutput, x<<1, Y1, Y2, Cb, Cr);
                JPEGPixel2_A(pJPEG, &cOutput[lsize], x<<1, Y3, Y4, Cb, Cr);
                break;
             } // pixel generation
          if (pJPEG->iResInterval)
             {
             if (--pJPEG->iResCount == 0)
                {
                pJPEG->iResCount = pJPEG->iResInterval;
                iDCPred0 = iDCPred1 = iDCPred2 = 0; // reset DC predictors
                if (*iBit & 7)
                   {
                   *iBit += (8 - (*iBit & 7));
//                   *iBit = 0;  // new restart interval starts on byte boundary
//                   (*iOff)++;
                   }
                }
             }
          } // for x
       if (pJPEG->jpegsample == 0x12 || pJPEG->jpegsample == 0x22)
          cOutput += lsize * 2; // skip 2 lines for subsampled color
       else
          cOutput += lsize; // next line
       } // for y
    return 0;
} /* GetJPEGxxThumb() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGPutMCUGray(...)                                        *
 *                                                                          *
 *  PURPOSE    : Draw a gray macro block into destination image.            *
 *                                                                          *
 ****************************************************************************/
static void JPEGPutMCUGray(PIL_PAGE *inpage, unsigned char *pSrc, unsigned char *cOutput, JPEGDATA *pJPEG, int x, int y, int lsize)
{
int i, j, xcount, ycount;
unsigned long lPixel;
unsigned char *pDest;

    if (pJPEG->iOptions & PIL_CONVERT_16BPP) // convert 8-bit grayscale to 16bpp grayscale
       {
       unsigned short *usDest;
       if (pJPEG->iOptions & PIL_CONVERT_HALFSIZE)
          {
          usDest = (unsigned short *)&cOutput[x*8 + y*4*lsize];
          ycount = 4;
          xcount = 4;
          if ((y*4 + ycount) > inpage->iHeight)
             ycount = inpage->iHeight & 3;
          if ((x*4 + xcount) > inpage->iWidth)
             xcount = inpage->iWidth & 3;
          for (i=0; i<ycount; i++) // do up to 8 rows
             {
             for (j=0; j<xcount; j++)
                {
                lPixel = pSrc[j*2];
                lPixel += pSrc[j*2+1];
                lPixel += pSrc[j*2+8];
                lPixel += pSrc[j*2+9];
                lPixel >>= 2;
                usDest[j] = (unsigned short)((lPixel >> 3) | ((lPixel>>2)<<5) | ((lPixel>>3)<<11));
                }
             usDest += (lsize>>1);
             pSrc += 16; // skip 2 source rows
             }
          }
       else if (pJPEG->iOptions & PIL_CONVERT_QUARTERSIZE)
          {
          usDest = (unsigned short *)&cOutput[x*4 + y*2*lsize];
          usDest[0] = (pSrc[0]>>3) | ((pSrc[0]>>2)<<5) | ((pSrc[0]>>3)<<11);
          usDest[1] = (pSrc[1]>>3) | ((pSrc[1]>>2)<<5) | ((pSrc[1]>>3)<<11);
          usDest += (lsize>>1);
          usDest[0] = (pSrc[2]>>3) | ((pSrc[2]>>2)<<5) | ((pSrc[2]>>3)<<11);
          usDest[1] = (pSrc[3]>>3) | ((pSrc[3]>>2)<<5) | ((pSrc[3]>>3)<<11);
          }
       else // fullsize
          {
          usDest = (unsigned short *)&cOutput[x*16 + y*8*lsize];
          ycount = 8;
          xcount = 8;
          if ((y*8 + ycount) > inpage->iHeight)
             ycount = inpage->iHeight & 7;
          if ((x*8 + xcount) > inpage->iWidth)
             xcount = inpage->iWidth & 7;
          for (i=0; i<ycount; i++) // do up to 8 rows
             {
             for (j=0; j<xcount; j++)
                {
                usDest[j] = (pSrc[j] >> 3) | ((pSrc[j]>>2)<<5) | ((pSrc[j]>>3)<<11);
                }
             usDest += (lsize>>1);
             pSrc += 8;
             }
          }
       }
    else if (pJPEG->iOptions & PIL_CONVERT_HALFSIZE)
       {
       pDest = x*4 + y*4*lsize + cOutput;
       ycount = 4;
       xcount = 4;
       if ((y*4 + ycount) > inpage->iHeight)
          ycount = inpage->iHeight & 3;
       if ((x*4 + xcount) > inpage->iWidth)
          xcount = inpage->iWidth & 3;
       // average 4 pixels together to create a 4x4 block instead of 8x8
       for (i=0; i<ycount; i++) // do up to 4 rows
          {
          for (j=0; j<xcount; j++)
             {
             lPixel = pSrc[j*2];
             lPixel += pSrc[j*2+1];
             lPixel += pSrc[j*2+8];
             lPixel += pSrc[j*2+9];
             pDest[j] = (unsigned char)(lPixel >> 2); // Average the 4 pixels
             }
          pDest += lsize;
          pSrc += 16; // skip 2 source rows
          }
       }
    else if (pJPEG->iOptions & PIL_CONVERT_QUARTERSIZE)
       {
       pDest = x*2 + y*2*lsize + cOutput;
       pDest[0] = pSrc[0];
       pDest[1] = pSrc[1];
       pDest += lsize;
       pDest[0] = pSrc[2];
       pDest[1] = pSrc[3];
       }
    else // full size
       {
       pDest = x*8 + y*8*lsize + cOutput;
       ycount = 8;
       xcount = 8;
       if ((y*8 + ycount) > inpage->iHeight)
          ycount = inpage->iHeight & 7;
       if ((x*8 + xcount) > inpage->iWidth)
          xcount = inpage->iWidth & 7;
       for (i=0; i<ycount; i++) // do up to 8 rows
          {
          if (xcount >= 5) // if not up against right edge
             { // copy 8 pixels in one shot
             *(long *)&pDest[0] = *(long *)&pSrc[0];
             *(long *)&pDest[4] = *(long *)&pSrc[4];
             }
          else
             {
             for (j=0; j<xcount; j++)
                {
                pDest[j] = pSrc[j]; // copy a partial row
                }
             }
          pDest += lsize;
          pSrc += 8;
          }
       }
} /* JPEGPutMCUGray() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGPutMCU11(int, int, int, int*, char *)                  *
 *                                                                          *
 *  PURPOSE    : Combine and output a subsampled color macro block.         *
 *                                                                          *
 ****************************************************************************/
static void JPEGPutMCU11(PIL_PAGE *inpage, int x, int y, int lsize, signed short *pMCU, unsigned char *cOutput, JPEGDATA *pJPEG)
{
unsigned long Cr,Cb;
signed int Y;
int iRow, iCol, iXCount, iYCount;
unsigned char *pY, *pCr, *pCb;

   pY  = (unsigned char *)&pMCU[MCU0];
   pCb = (unsigned char *)&pMCU[MCU1];
   pCr = (unsigned char *)&pMCU[MCU2];

   /* Convert YCC pixels into RGB pixels and store in output image */
   cOutput += y*8*lsize;
   if (pJPEG->iOptions & PIL_CONVERT_16BPP)
      cOutput += x<<4;
   else
      cOutput += x*24;
   if (y*8+7 >= inpage->iHeight)  // if last block overflows bottom of image
      iYCount = inpage->iHeight & 7;
   else
      iYCount = 8;
   if (x*8+7 >= inpage->iWidth)
      iXCount = inpage->iWidth & 7;
   else
      iXCount = 8;
#ifndef MIPS
#ifdef _WIN32_WCE
#ifdef USE_ARM_ASM
   if ((pJPEG->iOptions & PIL_CONVERT_16BPP) && iYCount == 8 && iXCount == 8) // draw the full sized 16bpp block with ASM
      ARMDrawMCU11(pMCU, cOutput, lsize);
   else
#endif // USE_ARM_ASM   
#endif // WCE
#endif // MIPS
   {
   if (0) //pJPEG->pTables && pJPEG->iOptions & PIL_CONVERT_16BPP) // we can do fast RGB565 conversion
      {
      unsigned long usPixel;
      for (iRow=0; iRow<8; iRow++) // up to 8 rows to do
         {
         for (iCol=0; iCol<8; iCol++) // up to 4x2 cols to do
            {
            usPixel = ((*pCr++ >> 3) << 11);
            usPixel |= (((*pCb++ >> 3) & 0x1f) << 6);
            usPixel |= ((*pY++ >> 2) & 0x3f);
            } // for col
         cOutput += lsize;
         } // for row
      } // if fast table
   else
      {
      for (iRow=0; iRow<iYCount; iRow++) // up to 8 rows to do
         {
         for (iCol=0; iCol<iXCount; iCol++) // up to 8 cols to do
            {
            Y = pY[iCol];
            Y <<= 12;  // scale to level of conversion table
            Cb = pCb[iCol];
            Cr = pCr[iCol];
            JPEGPixel(pJPEG, cOutput, iCol, Y, Cb, Cr);
            }
         pY += 8; // skip to next line of source pixels
         pCb += 8;
         pCr += 8;
         cOutput += lsize;
         }
      }
   }
} /* JPEGPutMCU11() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGPutMCU11HALF(int, int, int, int*, char *)              *
 *                                                                          *
 *  PURPOSE    : Combine and output a subsampled color macro block.         *
 *                                                                          *
 ****************************************************************************/
static void JPEGPutMCU11HALF(PIL_PAGE *inpage, int x, int y, int lsize, signed short *pMCU, unsigned char *cOutput, JPEGDATA *pJPEG)
{
unsigned long Cr,Cb;
signed int Y;
int iRow, iCol, iXCount, iYCount;
unsigned char *pY, *pCr, *pCb;

   pY  = (unsigned char *)&pMCU[MCU0];
   pCb = (unsigned char *)&pMCU[MCU1];
   pCr = (unsigned char *)&pMCU[MCU2];

   /* Convert YCC pixels into RGB pixels and store in output image */
   cOutput += y*4*lsize;
   if (pJPEG->iOptions & PIL_CONVERT_16BPP)
      cOutput += x<<3;
   else
      cOutput += x*12;
   if (y*4+3 >= inpage->iHeight)  // if last block overflows bottom of image
      iYCount = inpage->iHeight & 3;
   else
      iYCount = 4;
   if (x*4+3 >= inpage->iWidth)
      iXCount = inpage->iWidth & 3;
   else
      iXCount = 4;
   for (iRow=0; iRow<iYCount; iRow++) // up to 4 rows to do
      {
      for (iCol=0; iCol<iXCount; iCol++) // up to 4 cols to do
         {
         Y = pY[iCol*2];      // average 4 pixels together
         Y += pY[iCol*2+1];
         Y += pY[iCol*2+8];
         Y += pY[iCol*2+9];
         Y <<= 10;  // scale to level of conversion table
         Cb = pCb[iCol*2];
         Cb += pCb[iCol*2+1];
         Cb += pCb[iCol*2+8];
         Cb += pCb[iCol*2+9];
         Cb >>= 2;
         Cr = pCr[iCol*2];
         Cr += pCr[iCol*2+1];
         Cr += pCr[iCol*2+8];
         Cr += pCr[iCol*2+9];
         Cr >>= 2;
         JPEGPixel(pJPEG, cOutput, iCol, Y, Cb, Cr);
         }
      pY += 16; // skip to next line of source pixels
      pCb += 16;
      pCr += 16;
      cOutput += lsize;
      }

} /* JPEGPutMCU11HALF() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGPutMCU11QUARTER(int, int, int, int*, char *)           *
 *                                                                          *
 *  PURPOSE    : Combine and output a subsampled color macro block.         *
 *                                                                          *
 ****************************************************************************/
static void JPEGPutMCU11QUARTER(PIL_PAGE *inpage, int x, int y, int lsize, signed short *pMCU, unsigned char *cOutput, JPEGDATA *pJPEG)
{
unsigned long Cr,Cb;
signed int Y;
unsigned char *pY, *pCr, *pCb;

   pY  = (unsigned char *)&pMCU[MCU0];
   pCb = (unsigned char *)&pMCU[MCU1];
   pCr = (unsigned char *)&pMCU[MCU2];

   /* Convert YCC pixels into RGB pixels and store in output image */
   cOutput += y*2*lsize;
   if (pJPEG->iOptions & PIL_CONVERT_16BPP)
      cOutput += x<<2;
   else
      cOutput += x*6;
   Y = pY[0] << 12; // scale to level of conversion table
   Cb = pCb[0];
   Cr = pCr[0];
   JPEGPixel(pJPEG, cOutput, 0, Y, Cb, Cr);
   Y = pY[1] << 12; // scale to level of conversion table
   Cb = pCb[1];
   Cr = pCr[1];
   JPEGPixel(pJPEG, cOutput, 1, Y, Cb, Cr);
   cOutput += lsize;
   Y = pY[2] << 12; // scale to level of conversion table
   Cb = pCb[2];
   Cr = pCr[2];
   JPEGPixel(pJPEG, cOutput, 0, Y, Cb, Cr);
   Y = pY[3] << 12; // scale to level of conversion table
   Cb = pCb[3];
   Cr = pCr[3];
   JPEGPixel(pJPEG, cOutput, 1, Y, Cb, Cr);

} /* JPEGPutMCU11QUARTER() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGPutMCU21(int, int, int, int*, char *)                  *
 *                                                                          *
 *  PURPOSE    : Combine and output a subsampled color macro block.         *
 *                                                                          *
 ****************************************************************************/
static void JPEGPutMCU21(PIL_PAGE *inpage, int x, int y, int lsize, signed short *pMCU, unsigned char *cOutput, JPEGDATA *pJPEG)
{
unsigned long Cr,Cb;
signed int Y1, Y2;
int iRow, iCol, iXCount1, iXCount2, iYCount;
unsigned char *pY, *pCr, *pCb;
BOOL bUseOdd1, bUseOdd2; // special case where 24bpp odd sized image can clobber first column
  
   pY  = (unsigned char *)&pMCU[MCU0];
   pCb = (unsigned char *)&pMCU[MCU2];
   pCr = (unsigned char *)&pMCU[MCU3];

   /* Convert YCC pixels into RGB pixels and store in output image */
   cOutput += y*8*lsize;
   if (pJPEG->iOptions & PIL_CONVERT_16BPP)
      cOutput += x<<5;
   else
      cOutput += x*48;
   if (y*8+7 >= inpage->iHeight)  // if last block overflows bottom of image
      iYCount = inpage->iHeight & 7;
   else
      iYCount = 8;
   bUseOdd1 = bUseOdd2 = TRUE; // assume odd column can be used
   if (x*16+15 >= inpage->iWidth)
      {
      iXCount1 = (((inpage->iWidth & 15)+1)>>1);
      if (iXCount1 >= 4)
         {
         iXCount2 = iXCount1 - 4;
         iXCount1 = 4;
         if (inpage->iWidth & 1 && (iXCount2*2)+8+(x*16) > inpage->iWidth)
            bUseOdd2 = FALSE;
         }
      else
         {
         iXCount2 = 0;
         if (inpage->iWidth & 1 && (iXCount1*2)+(x*16) > inpage->iWidth)
            bUseOdd1 = FALSE;
         }
      }
   else
      iXCount1 = iXCount2 = 4;
#ifndef MIPS
#ifdef _WIN32_WCE
#ifdef USE_ARM_ASM
   if ((pJPEG->iOptions & PIL_CONVERT_16BPP) && iYCount == 8 && iXCount1 == 4 && iXCount2 == 4) // draw the full sized 16bpp block with ASM
      ARMDrawMCU21(pMCU, cOutput, lsize);
   else
#endif // ARM ASM
#endif // WCE
#endif // MIPS
   if (0) //pJPEG->pTables && pJPEG->iOptions & PIL_CONVERT_16BPP) // we can do fast RGB565 conversion
      {
      unsigned long usPixel1, usPixel2;
      unsigned long *pul;
      for (iRow=0; iRow<iYCount; iRow++) // up to 8 rows to do
         {
         pul = (unsigned long *)cOutput;
//         _m_prefetch(cOutput + lsize);
         for (iCol=0; iCol<4/*iXCount1*/; iCol++) // up to 4x2 cols to do
            { // left block
            usPixel1 = ((*pCr++ >> 3) << 11);
            usPixel1 |= (((*pCb++ >> 3) & 0x1f) << 6);
            usPixel1 |= ((*pY++ >> 2) & 0x3f);
            usPixel2 = usPixel1 & ~0x3f; // mask off Y1
            usPixel2 |= ((*pY++ >> 2) & 0x3f);
//            usPixel1 =  pJPEG->pTables->pYUV16[usPixel1];
//            usPixel2 =  pJPEG->pTables->pYUV16[usPixel2];
            *pul++ = usPixel1 | (usPixel2 << 16);
            // right block
            usPixel1 = ((pCr[3] >> 3) << 11);
            usPixel1 |= (((pCb[3] >> 3) & 0x1f) << 6);
            usPixel1 |= ((pY[126] >> 2) & 0x3f);
            usPixel2 = usPixel1 & ~0x3f; // mask off Y1
            usPixel2 |= ((pY[127] >> 2) & 0x3f);
//            usPixel1 =  pJPEG->pTables->pYUV16[usPixel1];
//            usPixel2 =  pJPEG->pTables->pYUV16[usPixel2];
            pul[3] = usPixel1 | (usPixel2 << 16);
            } // for col
//         pY += 8; // skip to next line of source pixels
         pCb += 4;
         pCr += 4;
         cOutput += lsize;
         } // for row
      }
   else
      {
      for (iRow=0; iRow<iYCount; iRow++) // up to 8 rows to do
         {
         for (iCol=0; iCol<iXCount1; iCol++) // up to 8 cols to do
            {
            // for left block
            Y1 = pY[iCol*2];
            Y2 = pY[iCol*2+1];
            Y1 <<= 12;  // scale to level of conversion table
            Y2 <<= 12;
            Cb = pCb[iCol];
            Cr = pCr[iCol];
            // left block
            if (bUseOdd1 || iCol != (iXCount1-1)) // only render if it won't go off the right edge
               JPEGPixel2(pJPEG, cOutput, iCol*2, Y1, Y2, Cb, Cr);
            else // can only draw 1 pixel
               JPEGPixel(pJPEG, cOutput, iCol*2, Y1, Cb, Cr);
            // right block
            if (iCol < iXCount2)
               {
               Y1 = pY[iCol*2+DCTSIZE2*2];
               Y2 = pY[iCol*2+1+DCTSIZE2*2];
               Y1 <<= 12;  // scale to level of conversion table
               Y2 <<= 12;
               Cb = pCb[iCol+4];
               Cr = pCr[iCol+4];
               if (bUseOdd2 || iCol != (iXCount2-1))
                  JPEGPixel2(pJPEG, cOutput, iCol*2+8, Y1, Y2, Cb, Cr);
               else // can only draw 1 pixel
                  JPEGPixel(pJPEG, cOutput, iCol*2+8, Y1, Cb, Cr);
               }
            }
         pY += 8; // skip to next line of source pixels
         pCb += 8;
         pCr += 8;
         cOutput += lsize;
         }
      }
} /* JPEGPutMCU21() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGPutMCU21HALF(int, int, int, int*, char *)              *
 *                                                                          *
 *  PURPOSE    : Combine and output a subsampled color macro block.         *
 *                                                                          *
 ****************************************************************************/
static void JPEGPutMCU21HALF(PIL_PAGE *inpage, int x, int y, int lsize, signed short *pMCU, unsigned char *cOutput, JPEGDATA *pJPEG)
{
unsigned long Cr,Cb;
signed int Y1;
int iRow, iCol, iXCount1, iXCount2, iYCount;
unsigned char *pY, *pCr, *pCb;
  
   pY  = (unsigned char *)&pMCU[MCU0];
   pCb = (unsigned char *)&pMCU[MCU2];
   pCr = (unsigned char *)&pMCU[MCU3];

   /* Convert YCC pixels into RGB pixels and store in output image */
   cOutput += y*4*lsize;
   if (pJPEG->iOptions & PIL_CONVERT_16BPP)
      cOutput += x<<4;
   else
      cOutput += x*24;
//   if (y*8+7 >= inpage->iHeight)  // if last block overflows bottom of image
//      iYCount = inpage->iHeight & 7;
//   else
      iYCount = 4;
   if (x*8+7 > (inpage->iWidth/2))
      {
      iXCount1 = ((inpage->iWidth/2) & 7);
      if (iXCount1 >= 4)
         {
         iXCount2 = iXCount1 - 4;
         iXCount1 = 4;
         }
      else
         {
         iXCount2 = 0;
         }
      }
   else
      iXCount1 = iXCount2 = 4;
#ifndef MIPS
#ifdef _WIN32_WCE
#ifdef USE_ARM_ASM
   if (iXCount1 == 4 && iXCount2 == 4 && (pJPEG->iOptions & PIL_CONVERT_16BPP)) // can do it faster in ASM
      ARMDrawMCU21Half(pMCU, cOutput, lsize);
   else
#endif // USE_ARM_ASM
#endif
#endif // MIPS
   {
      for (iRow=0; iRow<iYCount; iRow++) // up to 8 rows to do
         {
         for (iCol=0; iCol<iXCount1; iCol++) // up to 8 cols to do
            {
            // for left block
            Y1 = pY[iCol*2];
            Y1 += pY[iCol*2+1];
            Y1 += pY[iCol*2+8];
            Y1 += pY[iCol*2+9];
            Y1 <<= 10;  // scale to level of conversion table
            Cb = pCb[iCol];
            Cb += pCb[iCol+8];
            Cb >>= 1;
            Cr = pCr[iCol];
            Cr += pCr[iCol+8];
            Cr >>= 1;
            JPEGPixel(pJPEG, cOutput, iCol, Y1, Cb, Cr);
            // for right block
            if (iCol < iXCount2)
               {
               Y1 = pY[iCol*2+DCTSIZE2*2];
               Y1 += pY[iCol*2+1+DCTSIZE2*2];
               Y1 += pY[iCol*2+8+DCTSIZE2*2];
               Y1 += pY[iCol*2+9+DCTSIZE2*2];
               Y1 <<= 10;  // scale to level of conversion table
               Cb = pCb[iCol+4];
               Cb += pCb[iCol+12];
               Cb >>= 1;
               Cr = pCr[iCol+4];
               Cr += pCr[iCol+12];
               Cr >>= 1;
               JPEGPixel(pJPEG, cOutput, iCol+4, Y1, Cb, Cr);
               }
            }
         pY += 16; // skip to next line of source pixels
         pCb += 16;
         pCr += 16;
         cOutput += lsize;
         }
      }

} /* JPEGPutMCU21HALF() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGPutMCU21QUARTER(int, int, int, int*, char *)           *
 *                                                                          *
 *  PURPOSE    : Combine and output a subsampled color macro block.         *
 *                                                                          *
 ****************************************************************************/
static void JPEGPutMCU21QUARTER(PIL_PAGE *inpage, int x, int y, int lsize, signed short *pMCU, unsigned char *cOutput, JPEGDATA *pJPEG)
{
unsigned long Cr,Cb;
signed int Y1, Y2;
unsigned char *pY, *pCr, *pCb;
  
   pY  = (unsigned char *)&pMCU[MCU0];
   pCb = (unsigned char *)&pMCU[MCU2];
   pCr = (unsigned char *)&pMCU[MCU3];

   /* Convert YCC pixels into RGB pixels and store in output image */
   cOutput += y*2*lsize;
   if (pJPEG->iOptions & PIL_CONVERT_16BPP)
      cOutput += x<<3;
   else
      cOutput += x*12;
//#ifndef MIPS
//#ifdef _WIN32_WCE
//#ifdef USE_ARM_ASM
//   if (iXCount1 == 4 && iXCount2 == 4 && (pJPEG->iOptions & PIL_CONVERT_16BPP)) // can do it faster in ASM
//      ARMDrawMCU21Half(pMCU, cOutput, lsize);
//   else
//#endif // USE_ARM_ASM
//#endif
//#endif // MIPS
   {
   // top left block
   Y1 = pY[0] << 12;  // scale to level of conversion table
   Y2 = pY[1] << 12;
   Cb = pCb[0];
   Cr = pCr[0];
   JPEGPixel2(pJPEG, cOutput, 0, Y1, Y2, Cb, Cr);
   // top right block
   Y1 = pY[(DCTSIZE2<<1)] << 12;  // scale to level of conversion table
   Y2 = pY[1+(DCTSIZE2<<1)] << 12;
   Cb = pCb[1];
   Cr = pCr[1];
   JPEGPixel2(pJPEG, cOutput, 2, Y1, Y2, Cb, Cr);
   // second row
   cOutput += lsize;
   // bottom left block
   Y1 = pY[2] << 12;  // scale to level of conversion table
   Y2 = pY[3] << 12;
   Cb = pCb[2];
   Cr = pCr[2];
   JPEGPixel2(pJPEG, cOutput, 0, Y1, Y2, Cb, Cr);
   // bottom right block
   Y1 = pY[2+(DCTSIZE2<<1)] << 12;  // scale to level of conversion table
   Y2 = pY[3+(DCTSIZE2<<1)] << 12;
   Cb = pCb[3];
   Cr = pCr[3];
   JPEGPixel2(pJPEG, cOutput, 2, Y1, Y2, Cb, Cr);
   }

} /* JPEGPutMCU21QUARTER() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : GetJPEGxx(...)                                             *
 *                                                                          *
 *  PURPOSE    : Decode loop for color image.                               *
 *                                                                          *
 ****************************************************************************/
static int GetJPEGxx(PIL_PAGE *inpage, int *iOff, int *iBit, signed short *pMCU, JPEGDATA *pJPEG, unsigned char *cOutput)
{
int cx, cy, x, y, lsize;
int iLum0, iLum1, iLum2, iLum3, iCr, iCb;
signed int iDCPred0, iDCPred1, iDCPred2;
int i, iQuant1, iQuant2, iErr;
unsigned char c;
unsigned long l;
unsigned char cDCTable0, cACTable0, cDCTable1, cACTable1, cDCTable2, cACTable2;

    cDCTable0 = pJPEG->JPCI[0].dc_tbl_no;
    cACTable0 = pJPEG->JPCI[0].ac_tbl_no;
    cDCTable1 = pJPEG->JPCI[1].dc_tbl_no;
    cACTable1 = pJPEG->JPCI[1].ac_tbl_no;
    cDCTable2 = pJPEG->JPCI[2].dc_tbl_no;
    cACTable2 = pJPEG->JPCI[2].ac_tbl_no;

    lsize = mPILCalcSize(inpage->iWidth, inpage->cBitsperpixel);
    if (pJPEG->iOptions & PIL_CONVERT_HALFSIZE)
       lsize = mPILCalcSize((inpage->iWidth+1)/2, inpage->cBitsperpixel);
    else if (pJPEG->iOptions & PIL_CONVERT_QUARTERSIZE)
       lsize = mPILCalcSize((inpage->iWidth+3)/4, inpage->cBitsperpixel);
    iDCPred0 = iDCPred1 = iDCPred2 = 0;
    // luminance values are always in these positions
    iLum0 = MCU0;
    iLum1 = MCU1;
    iLum2 = MCU2;
    iLum3 = MCU3;
    switch (pJPEG->jpegsample) // set up the parameters for the different subsampling options
       {
       case 0x00: // hacked value to handle grayscale
       case 0x11:
          cx = (inpage->iWidth + 7)>>3;  // number of MCU blocks
          cy = (inpage->iHeight + 7)>>3;
          iCr = MCU1;
          iCb = MCU2;
          break;
       case 0x12:
          cx = (inpage->iWidth + 7)>>3;  // number of MCU blocks
          cy = (inpage->iHeight + 15)>>4;
          iCr = MCU2;
          iCb = MCU3;
          break;
       case 0x21:
          cx = (inpage->iWidth + 15)>>4;  // number of MCU blocks
          cy = (inpage->iHeight + 7)>>3;
          iCr = MCU2;
          iCb = MCU3;
          break;
       case 0x22:
          cx = (inpage->iWidth + 15)>>4;  // number of MCU blocks
          cy = (inpage->iHeight + 15)>>4;
          iCr = MCU4;
          iCb = MCU5;
          break;
       default: // to suppress compiler warning
          cx = cy = 0;
          iCr = iCb = 0;
          break;
       }
    iQuant1 = pJPEG->sQuantTable[0];
    iQuant2 = pJPEG->sQuantTable[DCTSIZE2];
    for (y=0; y<cy; y++)
       {
//       if (!(y & 15) && pfnProgress) // call progress function to display current progress
//          {
//          if ((*pfnProgress)(y, cy) == PIL_PROGRESS_CANCEL)
//             return PIL_ERROR_CANCELED;
//          }

       for (x=0; x<cx; x++)
          {
          if (inpage->cState == PIL_PAGE_STATE_OPEN && *iOff > PIL_BUFFER_HIGHWATER) // need to read more data
             PILReadBlock(inpage, iOff); // read the next chunk of data
//          if (*iOff > pJPEG->iDataSize)  // went past end of data
//             return PIL_ERROR_DECOMP;
//          _m_prefetch(&inpage->pData[*iOff + 64]); // prefetch data to save time
          pJPEG->pHuffACFast = pJPEG->huffacFast[cACTable0];
          pJPEG->pHuffDCFast = pJPEG->huffdcFast[cDCTable0];
          // do the first luminance component
          iErr = JPEGDecodeMCU(inpage->pData, iOff, iBit, &pMCU[iLum0], pJPEG, &iDCPred0);
          if (pJPEG->ucMaxACCol == 0) // no AC components, save some time
             {
             c = pJPEG->cRangeTable[((iDCPred0 * iQuant1)>>5) & 0x3ff];
             l = c | ((unsigned long)c << 8) | ((unsigned long)c << 16) | ((unsigned long)c << 24);
             // dct stores byte values
             for (i=0; i<32; i++) // 8x8 bytes = 32 shorts
                pMCU[iLum0+i] = (signed short)l;
             }
          else
             {
//             JPEGFixOrder(&pMCU[iLum0], &pMCU[iLum0 + MCUTEMP]);
             JPEGIDCT(pJPEG, &pMCU[iLum0], 0); // first quantization table
             }
          // do the second luminance component
          if (pJPEG->jpegsample > 0x11) // subsampling
             {
             iErr |= JPEGDecodeMCU(inpage->pData, iOff, iBit, &pMCU[iLum1], pJPEG, &iDCPred0);
             if (pJPEG->ucMaxACCol == 0) // no AC components, save some time
                {
                c = pJPEG->cRangeTable[((iDCPred0 * iQuant1)>>5) & 0x3ff];
                l = c | ((unsigned long)c << 8) | ((unsigned long)c << 16) | ((unsigned long)c << 24);
                // dct stores byte values
                for (i=0; i<32; i++) // 8x8 bytes = 32 shorts
                   pMCU[iLum1+i] = (signed short)l;
                }
             else
                {
   //             JPEGFixOrder(&pMCU[iLum1], &pMCU[iLum1 + MCUTEMP]);
                JPEGIDCT(pJPEG, &pMCU[iLum1], 0); // first quantization table
                }
             if (pJPEG->jpegsample == 0x22)
                {
                iErr |= JPEGDecodeMCU(inpage->pData, iOff, iBit, &pMCU[iLum2], pJPEG, &iDCPred0);
                if (pJPEG->ucMaxACCol == 0) // no AC components, save some time
                   {
                   c = pJPEG->cRangeTable[((iDCPred0 * iQuant1)>>5) & 0x3ff];
                   l = c | ((unsigned long)c << 8) | ((unsigned long)c << 16) | ((unsigned long)c << 24);
                   // dct stores byte values
                   for (i=0; i<32; i++) // 8x8 bytes = 32 shorts
                      pMCU[iLum2+i] = (signed short)l;
                   }
                else
                   {
      //             JPEGFixOrder(&pMCU[iLum2], &pMCU[iLum2 + MCUTEMP]);
                   JPEGIDCT(pJPEG, &pMCU[iLum2], 0); // first quantization table
                   }
                iErr |= JPEGDecodeMCU(inpage->pData, iOff, iBit, &pMCU[iLum3], pJPEG, &iDCPred0);
                if (pJPEG->ucMaxACCol == 0) // no AC components, save some time
                   {
                   c = pJPEG->cRangeTable[((iDCPred0 * iQuant1)>>5) & 0x3ff];
                   l = c | ((unsigned long)c << 8) | ((unsigned long)c << 16) | ((unsigned long)c << 24);
                   // dct stores byte values
                   for (i=0; i<32; i++) // 8x8 bytes = 32 shorts
                      pMCU[iLum3+i] = (signed short)l;
                   }
                else
                   {
      //             JPEGFixOrder(&pMCU[iLum3], &pMCU[iLum3 + MCUTEMP]);
                   JPEGIDCT(pJPEG, &pMCU[iLum3], 0); // first quantization table
                   }
                } // if 2:2 subsampling
             } // if subsampling used
          if (pJPEG->jpegsample) // if color
             {
             // first chroma
             pJPEG->pHuffACFast = pJPEG->huffacFast[cACTable1];
             pJPEG->pHuffDCFast = pJPEG->huffdcFast[cDCTable1];
             iErr |= JPEGDecodeMCU(inpage->pData, iOff, iBit, &pMCU[iCr], pJPEG, &iDCPred1);
             if (pJPEG->ucMaxACCol == 0) // no AC components, save some time
                {
                c = pJPEG->cRangeTable[((iDCPred1 * iQuant2)>>5) & 0x3ff];
                l = c | ((unsigned long)c << 8) | ((unsigned long)c << 16) | ((unsigned long)c << 24);
                // dct stores byte values
                for (i=0; i<32; i++) // 8x8 bytes = 32 shorts
                   pMCU[iCr+i] = (signed short)l;
                }
             else
                {
   //             JPEGFixOrder(&pMCU[iCr], &pMCU[iCr + MCUTEMP]);
                JPEGIDCT(pJPEG, &pMCU[iCr], 1); // second quantization table
                }
             // second chroma
             pJPEG->pHuffACFast = pJPEG->huffacFast[cACTable2];
             pJPEG->pHuffDCFast = pJPEG->huffdcFast[cDCTable2];
             iErr |= JPEGDecodeMCU(inpage->pData, iOff, iBit, &pMCU[iCb], pJPEG, &iDCPred2);
             if (iErr)
                return iErr; // decoding error, stop
             if (pJPEG->ucMaxACCol == 0) // no AC components, save some time
                {
                c = pJPEG->cRangeTable[((iDCPred2 * iQuant2)>>5) & 0x3ff];
                l = c | ((unsigned long)c << 8) | ((unsigned long)c << 16) | ((unsigned long)c << 24);
                // dct stores byte values
                for (i=0; i<32; i++) // 8x8 bytes = 32 shorts
                   pMCU[iCb+i] = (signed short)l;
                }
             else
                {
   //             JPEGFixOrder(&pMCU[iCb], &pMCU[iCb + MCUTEMP]);
                JPEGIDCT(pJPEG, &pMCU[iCb], 1);
                }
             } // if color components present

          switch (pJPEG->jpegsample)
             {
             case 0x00: // grayscale
                JPEGPutMCUGray(inpage, (unsigned char *)pMCU, cOutput, pJPEG, x, y, lsize);
                break;
             case 0x11:
                if (pJPEG->iOptions & PIL_CONVERT_HALFSIZE)
                   JPEGPutMCU11HALF(inpage, x, y, lsize, &pMCU[0], cOutput, pJPEG); // lay down MCU in output image
                else if (pJPEG->iOptions & PIL_CONVERT_QUARTERSIZE)
                   JPEGPutMCU11QUARTER(inpage, x, y, lsize, &pMCU[0], cOutput, pJPEG); // lay down MCU in output image
                else
                   JPEGPutMCU11(inpage, x, y, lsize, &pMCU[0], cOutput, pJPEG); // lay down MCU in output image
                break;
             case 0x12:
                if (pJPEG->iOptions & PIL_CONVERT_HALFSIZE)
                   JPEGPutMCU12HALF(inpage, x, y, lsize, &pMCU[0], cOutput, pJPEG); // lay down MCU in output image
                else if (pJPEG->iOptions & PIL_CONVERT_QUARTERSIZE)
                   JPEGPutMCU12QUARTER(inpage, x, y, lsize, &pMCU[0], cOutput, pJPEG); // lay down MCU in output image
                else
                   JPEGPutMCU12(inpage, x, y, lsize, &pMCU[0], cOutput, pJPEG); // lay down MCU in output image
                break;
             case 0x21:
                if (pJPEG->iOptions & PIL_CONVERT_HALFSIZE)
                   JPEGPutMCU21HALF(inpage, x, y, lsize, &pMCU[0], cOutput, pJPEG); // lay down MCU in output image
                else if (pJPEG->iOptions & PIL_CONVERT_QUARTERSIZE)
                   JPEGPutMCU21QUARTER(inpage, x, y, lsize, &pMCU[0], cOutput, pJPEG); // lay down MCU in output image
                else
                   JPEGPutMCU21(inpage, x, y, lsize, &pMCU[0], cOutput, pJPEG); // lay down MCU in output image
                break;
             case 0x22:
                if (pJPEG->iOptions & PIL_CONVERT_HALFSIZE)
                   JPEGPutMCU22HALF(inpage, x, y, lsize, &pMCU[0], cOutput, pJPEG); // lay down MCU in output image
                else if (pJPEG->iOptions & PIL_CONVERT_QUARTERSIZE)
                   JPEGPutMCU22QUARTER(inpage, x, y, lsize, &pMCU[0], cOutput, pJPEG); // lay down MCU in output image
                else
                   JPEGPutMCU22(inpage, x, y, lsize, &pMCU[0], cOutput, pJPEG); // lay down MCU in output image
                break;
             } // switch on color option
          if (pJPEG->iResInterval)
             {
             if (--pJPEG->iResCount == 0)
                {
                pJPEG->iResCount = pJPEG->iResInterval;
                iDCPred0 = iDCPred1 = iDCPred2 = 0; // reset DC predictors
                if (*iBit & 7) // need to start at the next even byte
                   {
                   *iBit += (8 - (*iBit & 7));  // new restart interval starts on byte boundary
//                   *iBit = 0;
//                   (*iOff)++;
                   }
                }
             }
          }
       }
    return 0;
} /* GetJPEGxx() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGPutMCU12(int, int, int, int*, char *)                  *
 *                                                                          *
 *  PURPOSE    : Combine and output a subsampled color macro block.         *
 *                                                                          *
 ****************************************************************************/
static void JPEGPutMCU12(PIL_PAGE *inpage, int x, int y, int lsize, signed short *pMCU, unsigned char *cOutput, JPEGDATA *pJPEG)
{
unsigned long Cr,Cb;
signed int Y1, Y2;
int iRow, iCol, iXCount, iYCount;
unsigned char *pY, *pCr, *pCb;

   pY  = (unsigned char *)&pMCU[MCU0];
   pCb = (unsigned char *)&pMCU[MCU2];
   pCr = (unsigned char *)&pMCU[MCU3];

   /* Convert YCC pixels into RGB pixels and store in output image */
   cOutput += y*16*lsize; // destination 8x16 block of output image
   if (pJPEG->iOptions & PIL_CONVERT_16BPP)
      cOutput += x<<4;
   else
      cOutput += x*24;
   if (y*16+15 >= inpage->iHeight)  // if last block overflows bottom of image
      iYCount = inpage->iHeight & 15;
   else
      iYCount = 16;
   if (x*8+7 >= inpage->iWidth)
      iXCount = inpage->iWidth & 7;
   else
      iXCount = 8;
   for (iRow=0; iRow<iYCount; iRow+=2) // up to 16 rows to do
      {
      for (iCol=0; iCol<iXCount; iCol++) // up to 8 cols to do
         {
         Y1 = pY[iCol];
         Y2 = pY[iCol+8];
         Y1 <<= 12;  // scale to level of conversion table
         Y2 <<= 12;
         Cb = pCb[iCol];
         Cr = pCr[iCol];
         JPEGPixel(pJPEG, cOutput, iCol, Y1, Cb, Cr);
         JPEGPixel(pJPEG, cOutput+lsize, iCol, Y2, Cb, Cr);
         }
      pY += 16; // skip to next 2 lines of source pixels
      if (iRow == 6) // next MCU block, skip ahead to correct spot
         pY += (128-64);
      pCb += 8;
      pCr += 8;
      cOutput += lsize*2; // next 2 lines of dest pixels
      }

} /* JPEGPutMCU12() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGPutMCU12HALF(int, int, int, int*, char *)              *
 *                                                                          *
 *  PURPOSE    : Combine and output a subsampled color macro block.         *
 *                                                                          *
 ****************************************************************************/
static void JPEGPutMCU12HALF(PIL_PAGE *inpage, int x, int y, int lsize, signed short *pMCU, unsigned char *cOutput, JPEGDATA *pJPEG)
{
unsigned long Cr,Cb;
signed int Y1;
int iRow, iCol, iXCount, iYCount;
unsigned char *pY, *pCr, *pCb;

   pY  = (unsigned char *)&pMCU[MCU0];
   pCb = (unsigned char *)&pMCU[MCU2];
   pCr = (unsigned char *)&pMCU[MCU3];

   /* Convert YCC pixels into RGB pixels and store in output image */
   cOutput += y*8*lsize; // destination 8x16 block of output image
   if (pJPEG->iOptions & PIL_CONVERT_16BPP)
      cOutput += x<<3;
   else
      cOutput += x*24;
   if (y*16+15 >= inpage->iHeight)  // if last block overflows bottom of image
      iYCount = inpage->iHeight & 15;
   else
      iYCount = 16;
   if (x*8+7 >= inpage->iWidth)
      iXCount = (inpage->iWidth & 7)>>1;
   else
      iXCount = 4;
   for (iRow=0; iRow<iYCount; iRow+=2) // up to 16 rows to do
      {
      for (iCol=0; iCol<iXCount; iCol++) // up to 8 cols to do
         {
         Y1 = pY[iCol*2];
         Y1 += pY[iCol*2+1];
         Y1 += pY[iCol*2+8];
         Y1 += pY[iCol*2+9];
         Y1 <<= 10;  // scale to level of conversion table
         Cb = pCb[iCol*2];
         Cb += pCb[iCol*2+1];
         Cb >>= 1;
         Cr = pCr[iCol*2];
         Cr += pCr[iCol*2+1];
         Cr >>= 1;
         JPEGPixel(pJPEG, cOutput, iCol, Y1, Cb, Cr);
         }
      pY += 16; // skip to next line of source pixels
      if (iRow == 6) // skip to next MCU block
         pY += (128-64);
      pCb += 8;
      pCr += 8;
      cOutput += lsize; // next line of dest pixels
      }

} /* JPEGPutMCU12HALF() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGPutMCU12QUARTER(int, int, int, int*, char *)           *
 *                                                                          *
 *  PURPOSE    : Combine and output a subsampled color macro block.         *
 *                                                                          *
 ****************************************************************************/
static void JPEGPutMCU12QUARTER(PIL_PAGE *inpage, int x, int y, int lsize, signed short *pMCU, unsigned char *cOutput, JPEGDATA *pJPEG)
{
unsigned long Cr,Cb;
signed int Y1;
unsigned char *pY, *pCr, *pCb;

   pY  = (unsigned char *)&pMCU[MCU0];
   pCb = (unsigned char *)&pMCU[MCU2];
   pCr = (unsigned char *)&pMCU[MCU3];

   /* Convert YCC pixels into RGB pixels and store in output image */
   cOutput += y*4*lsize; // destination 2x4 block of output image
   if (pJPEG->iOptions & PIL_CONVERT_16BPP)
      cOutput += x<<2;
   else
      cOutput += x*6;
   Y1  = pY[0] << 12; // scale to level of conversion table
   Cb  = pCb[0];
   Cr  = pCr[0];
   JPEGPixel(pJPEG, cOutput, 0, Y1, Cb, Cr);
   Y1  = pY[2] << 12; // scale to level of conversion table
   JPEGPixel(pJPEG, cOutput+lsize, 0, Y1, Cb, Cr);
   Y1  = pY[1] << 12; // scale to level of conversion table
   Cb  = pCb[1];
   Cr  = pCr[1];
   JPEGPixel(pJPEG, cOutput, 1, Y1, Cb, Cr);
   Y1  = pY[3] << 12; // scale to level of conversion table
   JPEGPixel(pJPEG, cOutput+lsize, 1, Y1, Cb, Cr);
   cOutput += lsize*2;
   pY += DCTSIZE2*2;
   Y1  = pY[0] << 12; // scale to level of conversion table
   Cb  = pCb[2];
   Cr  = pCr[2];
   JPEGPixel(pJPEG, cOutput, 0, Y1, Cb, Cr);
   Y1  = pY[2] << 12; // scale to level of conversion table
   JPEGPixel(pJPEG, cOutput+lsize, 0, Y1, Cb, Cr);
   Y1  = pY[1] << 12; // scale to level of conversion table
   Cb  = pCb[3];
   Cr  = pCr[3];
   JPEGPixel(pJPEG, cOutput, 1, Y1, Cb, Cr);
   Y1  = pY[3] << 12; // scale to level of conversion table
   JPEGPixel(pJPEG, cOutput+lsize, 1, Y1, Cb, Cr);

} /* JPEGPutMCU12QUARTER() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGDecodeMCUFast(SPVOBJ *, int *, int *, int *, JPEGDATA *)*
 *                                                                          *
 *  PURPOSE    : Decompress a macro block of JPEG data. Only need DC value  *
 *                                                                          *
 ****************************************************************************/
static int JPEGDecodeMCUFast(unsigned char *pBuf, int *iOff, int *iBit, JPEGDATA *pJPEG, int *iDCPredictor)
{
int iBitNum;
int iIndex;
unsigned long ulCode, ulBits;
unsigned short *pFast;
unsigned long usHuff; // this prevents an unnecessary & 65535 for shorts

   iBitNum = *iBit;
   ulBits = pJPEG->ulBits;
   if (iBitNum > 15) // need to get more data
      {
      iBitNum -= 16;
      ulBits <<= 16;
      ulBits |= JPEGGet16Bits(pBuf, iOff);
      }
 
   // get the DC component
   pFast = (unsigned short *)pJPEG->pHuffDCFast;
   ulCode = (ulBits >> (32 - 12 - iBitNum)) & 0xfff; // get as lower 12 bits
   if (ulCode >= 0xf80) // it's a long code
      ulCode = (ulCode & 0x7f) + 0x40; // point to long table and trim to 7-bits
   else
      ulCode >>= 6; // it's a short code, use first 6 bits only
   usHuff = pFast[ulCode]; // get the length+code
   if (usHuff == 0) // invalid code
      return PIL_ERROR_DECOMP;
   iBitNum += (usHuff >> 8); // add the Huffman length
   usHuff &= 0xff; // get the actual code (SSSS)
   if (usHuff) // if there is a change to the DC value
      { // get the 'extra' bits
      if (iBitNum > 15) // need to grab more bytes to nibble on
         {
         iBitNum -= 16;
         ulBits <<= 16;
         ulBits |= JPEGGet16Bits(pBuf, iOff);
         }
      ulCode = ulBits << iBitNum;
      ulCode >>= (32 - usHuff);
      if (!(ulCode & 0x80000000>>(32-usHuff))) // test for negative
         ulCode -= 0xffffffff>>(32-usHuff);
      iBitNum += usHuff; // add bit length
      (*iDCPredictor) += ulCode;
      }
//   pMCU[iIndex++] = iDCPredictor; // store in MCU[0]
   // Now get the other 63 AC coefficients
//   p = (unsigned short *)pJPEG->pHuffAC;
   pFast = (unsigned short *)pJPEG->pHuffACFast;
   iIndex = 1;
   while (iIndex < 64)
      {
      if (iBitNum > 15) // need to grab more bytes to nibble on
         {
         iBitNum -= 16;
         ulBits <<= 16;
         ulBits |= JPEGGet16Bits(pBuf, iOff);
         }
   // most codes are 8 bits or less, so try faster lookup first
      ulCode = (ulBits >> (32 - 16 - iBitNum)) & 0xffff; // get lower 16 bits
      if (ulCode >= 0xfc00) // if first 6 bits are 1, use long table
         ulCode = (ulCode & 0x7ff); //(ulCode & 0x3ff) + 0x400;
      else
         ulCode >>= 6; // use lower 10 bits for short table
      usHuff = pFast[ulCode]; // faster table can fit entirely on the processor cache
      if (usHuff == 0) // invalid code
         return PIL_ERROR_DECOMP;
      iBitNum += (usHuff >> 8); // add length
      usHuff &= 0xff; // get code (RRRR/SSSS)
      if (usHuff == 0) // no more AC components
         goto clean_exit;
      if (iBitNum > 15) // need to grab more bytes to nibble on
         {
         iBitNum -= 16;
         ulBits <<= 16;
         ulBits |= JPEGGet16Bits(pBuf, iOff);
         }
      iIndex += 1 + (usHuff >> 4); // get the skip amount (RRRR)
      iBitNum += (usHuff & 0xf); // get (SSSS) extra length
      }
clean_exit:
   *iBit = iBitNum;
   pJPEG->ulBits = ulBits;
   return 0;

} /* JPEGDecodeMCUFast() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGDecodeMCU(SPVOBJ *, int *, int *, int *, JPEGDATA *)   *
 *                                                                          *
 *  PURPOSE    : Decompress a macro block of JPEG data.                     *
 *                                                                          *
 ****************************************************************************/
static int JPEGDecodeMCU(unsigned char *pBuf, int *iOff, int *iBit, signed short *pMCU, JPEGDATA *pJPEG, int *iDCPredictor)
{
unsigned long ulCode, ulBits;
unsigned char *pEnd,*pEnd2,*pZig;
unsigned short *pFast;
unsigned long usHuff; // this prevents an unnecessary & 65535 for shorts
int iBitNum;

   if (pJPEG->iOptions & PIL_CONVERT_QUARTERSIZE) // reduced size DCT
      {
      pMCU[1] = pMCU[8] = pMCU[9] = 0;
      pEnd2 = &cZigZag2[5]; // we only need to store the 4 elements we care about
      }
   else
      {
      memset(&pMCU[1], 0, 63*sizeof(short)); // pre-fill with zero since we may skip coefficients
      pEnd2 = &cZigZag2[64];
      }
   pZig = &cZigZag2[1];
   pEnd = &cZigZag2[64];

   iBitNum = *iBit;

   ulBits = pJPEG->ulBits;
   if (iBitNum > 15) // need to get more data
      {
      iBitNum -= 16;
      ulBits <<= 16;
      ulBits |= JPEGGet16Bits(pBuf, iOff);
      }

   // get the DC component
   pFast = (unsigned short *)pJPEG->pHuffDCFast;
   ulCode = (ulBits >> (32 - 12 - iBitNum)) & 0xfff; // get as lower 12 bits
   if (ulCode >= 0xf80) // it's a long code
      ulCode = (ulCode & 0x7f) + 0x40; // point to long table and trim to 7-bits
   else
      ulCode >>= 6; // it's a short code, use first 6 bits only
   usHuff = pFast[ulCode];
   if (usHuff == 0) // invalid code
      return PIL_ERROR_DECOMP;
   iBitNum += (usHuff >> 8); // add the Huffman length
   usHuff &= 0xff; // get the actual code (SSSS)
   if (usHuff) // if there is a change to the DC value
      { // get the 'extra' bits
      if (iBitNum > 15) // need to grab more bytes to nibble on
         {
         iBitNum -= 16;
         ulBits <<= 16;
         ulBits |= JPEGGet16Bits(pBuf, iOff);
         }
      ulCode = ulBits << iBitNum;
      ulCode >>= (32 - usHuff);
      if (!(ulCode & 0x80000000>>(32-usHuff))) // test for negative
         ulCode -= 0xffffffff>>(32-usHuff);
      iBitNum += usHuff; // add bit length
      (*iDCPredictor) += ulCode;
      }
   pMCU[0] = (short)*iDCPredictor; // store in MCU[0]
   // Now get the other 63 AC coefficients
//   p = (unsigned short *)pJPEG->pHuffAC;
   pFast = (unsigned short *)pJPEG->pHuffACFast;
   pJPEG->ucMaxACCol = 0; // keep track of occupied columns (0 is always occupied)
   pJPEG->ucMaxACRow = 0; // keep track of colums with 4 or more occupied rows
   while (pZig < pEnd)
      {
      if (iBitNum > 15) // need to grab more bytes to nibble on
         {
         iBitNum -= 16;
         ulBits <<= 16;
         ulBits |= JPEGGet16Bits(pBuf, iOff);
         }
      ulCode = (ulBits >> (32 - 16 - iBitNum)) & 0xffff; // get as lower 16 bits
      if (ulCode >= 0xfc00) // first 6 bits = 1, use long table
         ulCode = (ulCode & 0x7ff); // (ulCode & 0x3ff) + 0x400;
      else
         ulCode >>= 6; // use lower 10 bits (short table)
      usHuff = pFast[ulCode];
      if (usHuff == 0) // invalid code
         return PIL_ERROR_DECOMP;
      iBitNum += (usHuff >> 8); // add length
      usHuff &= 0xff; // get code (RRRR/SSSS)
      if (usHuff == 0) // no more AC components
         {
         goto mcu_done;
         }
      if (usHuff == 0xf0) // is it ZRL?
         {
         pZig += 16;  // skip the next 16 AC coefficients
         }
      else
         {
         if (iBitNum > 15)
            {
            iBitNum -= 16;
            ulBits <<= 16;
            ulBits |= JPEGGet16Bits(pBuf, iOff);
            }
         if (usHuff & 0xf0)
            {
            pZig += (usHuff >> 4);  // get the skip amount (RRRR)
            }
         usHuff &= 0xf; // get (SSSS) - extra length
         ulCode = ulBits << iBitNum;
         ulCode >>= (32 - usHuff);
         if (!(ulCode & 0x80000000>>(32-usHuff))) // test for negative
            ulCode -= 0xffffffff>>(32-usHuff);
         iBitNum += usHuff; // add (SSSS) extra length
         if (pZig < pEnd2)
            {
            pJPEG->ucMaxACCol |= 1<<(*pZig & 7); // keep track of occupied columns
            if (*pZig >= 0x20) // if more than 4 rows used in a col, mark it
               pJPEG->ucMaxACRow |= 1<<(*pZig & 7); // keep track of the max AC term row
            pMCU[*(pZig++)] = (signed short)ulCode; // store AC coefficient (already reordered)
            }
         else
            pZig++;
         }
      }
mcu_done:
   *iBit = iBitNum;
   pJPEG->ulBits = ulBits;
   return 0;

} /* JPEGDecodeMCU() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGIDCT(int *, int*)                                      *
 *                                                                          *
 *  PURPOSE    : Perform inverse discrete cosine transform on macro block.  *
 *                                                                          *
 ****************************************************************************/
static void JPEGIDCT(JPEGDATA *pJPEG, signed short *pMCUSrc, int iQuantTable)
{
int iCol, iRow;
unsigned char ucColMask, *pOutput, *pRangeTable;
signed short *pQuant;
signed int tmp0,tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7,tmp10,tmp11,tmp12,tmp13;
signed int z5,z10,z11,z12,z13;

// my shortcut method appears to violate patent 20020080052
// but the patent is invalidated by prior art:
// http://netilium.org/~mad/dtj/DTJ/DTJK04/
   pQuant = (signed short *)&pJPEG->sQuantTable[iQuantTable * DCTSIZE2];
   pRangeTable = pJPEG->cRangeTable;
   if (pJPEG->iOptions & PIL_CONVERT_QUARTERSIZE) // special case
      {
      /* Column 0 */
      tmp4 = pMCUSrc[0] * pQuant[0];
      tmp5 = pMCUSrc[8] * pQuant[8];
      tmp0 = tmp4 + tmp5;
      tmp2 = tmp4 - tmp5;
      /* Column 1 */
      tmp4 = pMCUSrc[1] * pQuant[1];
      tmp5 = pMCUSrc[9] * pQuant[9];
      tmp1 = tmp4 + tmp5;
      tmp3 = tmp4 - tmp5;
      /* Pass 2: process 2 rows, store into output array. */
      /* Row 0 */
      pOutput = (unsigned char *)pMCUSrc; // store output pixels back into MCU
      pOutput[0] = pRangeTable[((((tmp0 + tmp1)*7)>>8) & 0x3ff)];
      pOutput[1] = pRangeTable[((((tmp0 - tmp1)*7)>>8) & 0x3ff)];
      /* Row 1 */
      pOutput[2] = pRangeTable[((((tmp2 + tmp3)*7)>>8) & 0x3ff)];
      pOutput[3] = pRangeTable[((((tmp2 - tmp3)*7)>>8) & 0x3ff)];
      return;
      }
   // do columns first
   ucColMask = pJPEG->ucMaxACCol | 1; // column 0 must always be calculated
   for (iCol = 0; iCol < 8 && ucColMask; iCol++)
      {
      if (ucColMask & (1<<iCol)) // column has data in it
         {
         ucColMask &= ~(1<<iCol); // unmark this col after use
         if (!(pJPEG->ucMaxACRow & (1<<iCol))) // simpler calculations if only half populated
            {
            // even part
            tmp10 = pMCUSrc[iCol] * pQuant[iCol];
            tmp1 = pMCUSrc[iCol+16] * pQuant[iCol+16]; // get 2nd row
            tmp12 = ((tmp1*106)>>8); // used to be 362 - 1 (256)
            tmp0 = tmp10 + tmp1;
            tmp3 = tmp10 - tmp1;
            tmp1 = tmp10 + tmp12;
            tmp2 = tmp10 - tmp12;
            // odd part
            tmp4 = pMCUSrc[iCol+8] * pQuant[iCol+8]; // get 1st row
            tmp5 = pMCUSrc[iCol+24];
            if (tmp5) // this value is usually 0
               {
               tmp5 *= pQuant[iCol+24]; // get 3rd row
               tmp7 = tmp4 + tmp5;
               tmp11 = (((tmp4 - tmp5) * 362) >> 8);  // 362>>8 = 1.414213562
               z5 = (((tmp4-tmp5) * 473) >> 8);  // 473>>8 = 1.8477
               tmp12 = ((-tmp5 * -669)>>8) + z5; // -669>>8 = -2.6131259
               tmp6 = tmp12 - tmp7;
               tmp5 = tmp11 - tmp6;
               tmp10 = ((tmp4 * 277)>>8) - z5; // 277>>8 = 1.08239
               tmp4 = tmp10 + tmp5;
               }
            else // simpler case when we only have 1 odd row to calculate
               {
               tmp7 = tmp4;
               tmp5 = (145*tmp4) >> 8;
               tmp6 = (217*tmp4) >> 8;
               tmp4 = (-51*tmp4) >> 8;
               }
            pMCUSrc[iCol] = (short)(tmp0 + tmp7);    // row0
            pMCUSrc[iCol+8] = (short)(tmp1 + tmp6);  // row 1
            pMCUSrc[iCol+16] = (short)(tmp2 + tmp5); // row 2
            pMCUSrc[iCol+24] = (short)(tmp3 - tmp4); // row 3
            pMCUSrc[iCol+32] = (short)(tmp3 + tmp4); // row 4
            pMCUSrc[iCol+40] = (short)(tmp2 - tmp5); // row 5
            pMCUSrc[iCol+48] = (short)(tmp1 - tmp6); // row 6
            pMCUSrc[iCol+56] = (short)(tmp0 - tmp7); // row 7
            }
         else // need to do full column calculation
            {
            // even part
            tmp0 = pMCUSrc[iCol] * pQuant[iCol];
            tmp2 = pMCUSrc[iCol+32]; // get 4th row
            if (tmp2) // 4th row is most likely 0
               {
               tmp2 = tmp2 * pQuant[iCol+32];
               tmp10 = tmp0 + tmp2;
               tmp11 = tmp0 - tmp2;
               }
            else
               {
               tmp10 = tmp11 = tmp0;
               }
            tmp1 = pMCUSrc[iCol+16] * pQuant[iCol+16]; // get 2nd row
            tmp3 = pMCUSrc[iCol+48]; // get 6th row
            if (tmp3) // 6th row is most likely 0
               {
               tmp3 = tmp3 * pQuant[iCol+48];
               tmp13 = tmp1 + tmp3;
               tmp12 = (((tmp1 - tmp3) * 362) >> 8) - tmp13;  // 362>>8 = 1.414213562
               }
            else
               {
               tmp13 = tmp1;
               tmp12 = ((tmp1*362)>>8) - tmp1;
               }
            tmp0 = tmp10 + tmp13;
            tmp3 = tmp10 - tmp13;
            tmp1 = tmp11 + tmp12;
            tmp2 = tmp11 - tmp12;
            // odd part
            tmp5 = pMCUSrc[iCol+24] * pQuant[iCol+24]; // get 3rd row
            tmp6 = pMCUSrc[iCol+40]; // get 5th row
            if (tmp6) // very likely that row 5 = 0
               {
               tmp6 = tmp6 * pQuant[iCol+40];
               z13 = tmp6 + tmp5;
               z10 = tmp6 - tmp5;
               }
            else
               {
               z13 = tmp5;
               z10 = -tmp5;
               }
            tmp4 = pMCUSrc[iCol+8] * pQuant[iCol+8]; // get 1st row
            tmp7 = pMCUSrc[iCol+56]; // get 7th row
            if (tmp7) // very likely that row 7 = 0
               {
               tmp7 = tmp7 * pQuant[iCol+56];
               z11 = tmp4 + tmp7;
               z12 = tmp4 - tmp7;
               }
            else
               {
               z11 = z12 = tmp4;
               }
            tmp7 = z11 + z13;
            tmp11 = (((z11 - z13) * 362) >> 8);  // 362>>8 = 1.414213562
            z5 = (((z10 + z12) * 473) >> 8);  // 473>>8 = 1.8477
            tmp12 = ((z10 * -669)>>8) + z5; // -669>>8 = -2.6131259
            tmp6 = tmp12 - tmp7;
            tmp5 = tmp11 - tmp6;
            tmp10 = ((z12 * 277)>>8) - z5; // 277>>8 = 1.08239
            tmp4 = tmp10 + tmp5;
            pMCUSrc[iCol] = (short)(tmp0 + tmp7);    // row0
            pMCUSrc[iCol+8] = (short)(tmp1 + tmp6);  // row 1
            pMCUSrc[iCol+16] = (short)(tmp2 + tmp5); // row 2
            pMCUSrc[iCol+24] = (short)(tmp3 - tmp4); // row 3
            pMCUSrc[iCol+32] = (short)(tmp3 + tmp4); // row 4
            pMCUSrc[iCol+40] = (short)(tmp2 - tmp5); // row 5
            pMCUSrc[iCol+48] = (short)(tmp1 - tmp6); // row 6
            pMCUSrc[iCol+56] = (short)(tmp0 - tmp7); // row 7
            } // full calculation needed
         } // if column has data in it
      } // for each column
   // now do rows
   pOutput = (unsigned char *)pMCUSrc; // store output pixels back into MCU
   for (iRow=0; iRow<64; iRow+=8) // all rows must be calculated
      {
      // even part
      if (pJPEG->ucMaxACCol < 0x10) // quick and dirty calculation (right 4 columns are all 0's)
         {
         if (pJPEG->ucMaxACCol < 0x04) // very likely case (1 or 2 columns occupied)
            {
            tmp0 = tmp1 = tmp2 = tmp3 = pMCUSrc[iRow+0];
            // odd part
            tmp7 = z11 = pMCUSrc[iRow+1];
            tmp11 = (z11*362)>>8; // * 1.414
            z5 = (z11*473)>>8; // * 1.8477
            tmp10 = ((z11*277)>>8) - z5; // * 1.08239
            tmp12 = z5; // * 2.61312
            tmp6 = tmp12 - z11;
            tmp5 = tmp11 - tmp6;
            tmp4 = tmp10 + tmp5;
            }
         else
            {
            tmp10 = pMCUSrc[iRow+0];
            tmp13 = pMCUSrc[iRow+2];
            tmp12 = ((tmp13 * 106)>>8); // 2-6 * 1.414
            tmp0 = tmp10 + tmp13;
            tmp3 = tmp10 - tmp13;
            tmp1 = tmp10 + tmp12;
            tmp2 = tmp10 - tmp12;
            // odd part
            z13 = pMCUSrc[iRow+3];
            z11 = pMCUSrc[iRow+1];
            tmp7 = z11 + z13;
            tmp11 = ((z11 - z13)*362)>>8; // * 1.414
            z5 = ((z11 - z13)*473)>>8; // * 1.8477
            tmp10 = ((z11*277)>>8) - z5; // * 1.08239
            tmp12 = ((z13*669)>>8) + z5; // * 2.61312
            tmp6 = tmp12 - tmp7;
            tmp5 = tmp11 - tmp6;
            tmp4 = tmp10 + tmp5;
            }
         }
      else // need to do the full calculation
         {
         tmp10 = pMCUSrc[iRow+0] + pMCUSrc[iRow+4];
         tmp11 = pMCUSrc[iRow+0] - pMCUSrc[iRow+4];
         tmp13 = pMCUSrc[iRow+2] + pMCUSrc[iRow+6];
         tmp12 = (((pMCUSrc[iRow+2] - pMCUSrc[iRow+6]) * 362)>>8) - tmp13; // 2-6 * 1.414
         tmp0 = tmp10 + tmp13;
         tmp3 = tmp10 - tmp13;
         tmp1 = tmp11 + tmp12;
         tmp2 = tmp11 - tmp12;
         // odd part
         z13 = pMCUSrc[iRow+5] + pMCUSrc[iRow+3];
         z10 = pMCUSrc[iRow+5] - pMCUSrc[iRow+3];
         z11 = pMCUSrc[iRow+1] + pMCUSrc[iRow+7];
         z12 = pMCUSrc[iRow+1] - pMCUSrc[iRow+7];
         tmp7 = z11 + z13;
         tmp11 = ((z11 - z13)*362)>>8; // * 1.414
         z5 = ((z10 + z12)*473)>>8; // * 1.8477
         tmp10 = ((z12*277)>>8) - z5; // * 1.08239
         tmp12 = ((z10*-669)>>8) + z5; // * 2.61312
         tmp6 = tmp12 - tmp7;
         tmp5 = tmp11 - tmp6;
         tmp4 = tmp10 + tmp5;
         }
   // final output stage - scale down and range limit
      pOutput[0] = pRangeTable[(((tmp0 + tmp7)>>5) & 0x3ff)];
      pOutput[1] = pRangeTable[(((tmp1 + tmp6)>>5) & 0x3ff)];
      pOutput[2] = pRangeTable[(((tmp2 + tmp5)>>5) & 0x3ff)];
      pOutput[3] = pRangeTable[(((tmp3 - tmp4)>>5) & 0x3ff)];
      pOutput[4] = pRangeTable[(((tmp3 + tmp4)>>5) & 0x3ff)];
      pOutput[5] = pRangeTable[(((tmp2 - tmp5)>>5) & 0x3ff)];
      pOutput[6] = pRangeTable[(((tmp1 - tmp6)>>5) & 0x3ff)];
      pOutput[7] = pRangeTable[(((tmp0 - tmp7)>>5) & 0x3ff)];
      pOutput += 8;
      } // for each row

} /* JPEGIDCT() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGPutMCU22(int, int, int, int*, char *)                  *
 *                                                                          *
 *  PURPOSE    : Combine and output a subsampled color macro block.         *
 *                                                                          *
 ****************************************************************************/
static void JPEGPutMCU22(PIL_PAGE *inpage, int x, int y, int lsize, signed short *pMCU, unsigned char *cOutput, JPEGDATA *pJPEG)
{
unsigned long Cr,Cb;
signed int Y1, Y2, Y3, Y4;
int iRow, iCol, iXCount1, iXCount2, iYCount;
unsigned char *pY, *pCr, *pCb;
BOOL bUseOdd1, bUseOdd2; // special case where 24bpp odd sized image can clobber first column
  
   pY  = (unsigned char *)&pMCU[MCU0];
   pCb = (unsigned char *)&pMCU[MCU4];
   pCr = (unsigned char *)&pMCU[MCU5];

   /* Convert YCC pixels into RGB pixels and store in output image */
   cOutput += y*16*lsize;
   if (pJPEG->iOptions & PIL_CONVERT_16BPP)
      cOutput += x<<5;
   else
      cOutput += x*48;
//   if (y*16+15 >= inpage->iHeight)  // if last block overflows bottom of image
//      iYCount = (inpage->iHeight & 15)>>2;
//   else
      iYCount = 4;
   bUseOdd1 = bUseOdd2 = TRUE; // assume odd column can be used
   if ((x*16+15) >= inpage->iWidth)
      {
      iCol = (((inpage->iWidth & 15)+1) >> 1);
      if (iCol >= 4)
         {
         iXCount1 = 4;
         iXCount2 = iCol-4;
         if (inpage->iWidth & 1 && (iXCount2*2)+8+(x*16) > inpage->iWidth)
            bUseOdd2 = FALSE;
         }
      else
         {
         iXCount1 = iCol;
         iXCount2 = 0;
         if (inpage->iWidth & 1 && (iXCount1*2)+(x*16) > inpage->iWidth)
            bUseOdd1 = FALSE;
         }
      }
   else
      iXCount1 = iXCount2 = 4;
#ifndef MIPS
#ifdef _WIN32_WCE
#ifdef USE_ARM_ASM
   if ((pJPEG->iOptions & PIL_CONVERT_16BPP) && iYCount == 4 && iXCount1 == 4 && iXCount2 == 4) // draw the full sized 16bpp block with ASM
      ARMDrawMCU22(pMCU, cOutput, lsize);
   else
#endif // USE_ARM_ASM   
#endif // WCE
#endif // MIPS
   {
   for (iRow=0; iRow<iYCount; iRow++) // up to 4 rows to do
      {
      for (iCol=0; iCol<iXCount1; iCol++) // up to 4 cols to do
         {
         // for top left block
         Y1 = pY[iCol*2];
         Y2 = pY[iCol*2+1];
         Y3 = pY[iCol*2+8];
         Y4 = pY[iCol*2+9];
         Y1 <<= 12;  // scale to level of conversion table
         Y2 <<= 12;
         Y3 <<= 12;
         Y4 <<= 12;
         Cb = pCb[iCol];
         Cr = pCr[iCol];
         if (bUseOdd1 || iCol != (iXCount1-1)) // only render if it won't go off the right edge
            {
            JPEGPixel2(pJPEG, cOutput, (iCol<<1), Y1, Y2, Cb, Cr);
            JPEGPixel2(pJPEG, cOutput+lsize, (iCol<<1), Y3, Y4, Cb, Cr);
            }
         else
            {
            JPEGPixel(pJPEG, cOutput, (iCol<<1), Y1, Cb, Cr);
            JPEGPixel(pJPEG, cOutput+lsize, (iCol<<1), Y3, Cb, Cr);
            }
         // for top right block
         if (iCol < iXCount2)
            {
            Y1 = pY[iCol*2+DCTSIZE2*2];
            Y2 = pY[iCol*2+1+DCTSIZE2*2];
            Y3 = pY[iCol*2+8+DCTSIZE2*2];
            Y4 = pY[iCol*2+9+DCTSIZE2*2];
            Y1 <<= 12;  // scale to level of conversion table
            Y2 <<= 12;
            Y3 <<= 12;
            Y4 <<= 12;
            Cb = pCb[iCol+4];
            Cr = pCr[iCol+4];
            if (bUseOdd2 || iCol != (iXCount2-1)) // only render if it won't go off the right edge
               {
               JPEGPixel2(pJPEG, cOutput, 8+(iCol<<1), Y1, Y2, Cb, Cr);
               JPEGPixel2(pJPEG, cOutput+lsize, 8+(iCol<<1), Y3, Y4, Cb, Cr);
               }
            else
               {
               JPEGPixel(pJPEG, cOutput, 8+(iCol<<1), Y1, Cb, Cr);
               JPEGPixel(pJPEG, cOutput+lsize, 8+(iCol<<1), Y3, Cb, Cr);
               }
            }
         // for bottom left block
         Y1 = pY[iCol*2+DCTSIZE2*4];
         Y2 = pY[iCol*2+1+DCTSIZE2*4];
         Y3 = pY[iCol*2+8+DCTSIZE2*4];
         Y4 = pY[iCol*2+9+DCTSIZE2*4];
         Y1 <<= 12;  // scale to level of conversion table
         Y2 <<= 12;
         Y3 <<= 12;
         Y4 <<= 12;
         Cb = pCb[iCol+32];
         Cr = pCr[iCol+32];
         if (bUseOdd1 || iCol != (iXCount1-1)) // only render if it won't go off the right edge
            {
            JPEGPixel2(pJPEG, cOutput+lsize*8, (iCol<<1), Y1, Y2, Cb, Cr);
            JPEGPixel2(pJPEG, cOutput+lsize*9, (iCol<<1), Y3, Y4, Cb, Cr);
            }
         else
            {
            JPEGPixel(pJPEG, cOutput+lsize*8, (iCol<<1), Y1, Cb, Cr);
            JPEGPixel(pJPEG, cOutput+lsize*9, (iCol<<1), Y3, Cb, Cr);
            }
         // for bottom right block
         if (iCol < iXCount2)
            {
            Y1 = pY[iCol*2+DCTSIZE2*6];
            Y2 = pY[iCol*2+1+DCTSIZE2*6];
            Y3 = pY[iCol*2+8+DCTSIZE2*6];
            Y4 = pY[iCol*2+9+DCTSIZE2*6];
            Y1 <<= 12;  // scale to level of conversion table
            Y2 <<= 12;
            Y3 <<= 12;
            Y4 <<= 12;
            Cb = pCb[iCol+36];
            Cr = pCr[iCol+36];
            if (bUseOdd2 || iCol != (iXCount2-1)) // only render if it won't go off the right edge
               {
               JPEGPixel2(pJPEG, cOutput+lsize*8, 8+(iCol<<1), Y1, Y2, Cb, Cr);
               JPEGPixel2(pJPEG, cOutput+lsize*9, 8+(iCol<<1), Y3, Y4, Cb, Cr);
               }
            else
               {
               JPEGPixel(pJPEG, cOutput+lsize*8, 8+(iCol<<1), Y1, Cb, Cr);
               JPEGPixel(pJPEG, cOutput+lsize*9, 8+(iCol<<1), Y3, Cb, Cr);
               }
            }
         } // for each column
      pY += 16; // skip to next line of source pixels
      pCb += 8;
      pCr += 8;
      cOutput += lsize*2;
      }
   }
} /* JPEGPutMCU22() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGPutMCU22HALF(int, int, int, int*, char *)              *
 *                                                                          *
 *  PURPOSE    : Combine and output a subsampled color macro block.         *
 *                                                                          *
 ****************************************************************************/
static void JPEGPutMCU22HALF(PIL_PAGE *inpage, int x, int y, int lsize, signed short *pMCU, unsigned char *cOutput, JPEGDATA *pJPEG)
{
unsigned long Cr,Cb;
signed int Y1;
int iRow, iCol, iXCount1, iXCount2, iYCount;
unsigned char *pY, *pCr, *pCb;

   pY  = (unsigned char *)&pMCU[MCU0];
   pCb = (unsigned char *)&pMCU[MCU4];
   pCr = (unsigned char *)&pMCU[MCU5];

   /* Convert YCC pixels into RGB pixels and store in output image */
   cOutput += y*8*lsize;
   if (pJPEG->iOptions & PIL_CONVERT_16BPP)
      cOutput += x<<4;
   else
      cOutput += x*24;
   iYCount = 4;
   if (x*8+7 > (inpage->iWidth/2))
      {
      iXCount1 = ((inpage->iWidth/2) & 7);
      if (iXCount1 >= 4)
         {
         iXCount2 = iXCount1 - 4;
         iXCount1 = 4;
         }
      else
         {
         iXCount2 = 0;
         }
      }
   else
      iXCount1 = iXCount2 = 4;
   for (iRow=0; iRow<iYCount; iRow++) // up to 4 rows to do
      {
      for (iCol=0; iCol<iXCount1; iCol++) // up to 4 cols to do
         {
         // for top left block
         // Average the 4 pixels together
         Y1 =  pY[iCol*2];
         Y1 += pY[iCol*2+1];
         Y1 += pY[iCol*2+8];
         Y1 += pY[iCol*2+9];
//         Y1 <<= 12;  // scale to level of conversion table
         Y1 <<= 10;
         Cb = pCb[iCol];
         Cr = pCr[iCol];
         JPEGPixel(pJPEG, cOutput, iCol, Y1, Cb, Cr);
         // for top right block
         // average the 4 pixels together
      if (iCol < iXCount2)
         {
         Y1 =  pY[iCol*2+DCTSIZE2*2];
         Y1 += pY[iCol*2+1+DCTSIZE2*2];
         Y1 += pY[iCol*2+8+DCTSIZE2*2];
         Y1 += pY[iCol*2+9+DCTSIZE2*2];
//         Y1 <<= 12;  // scale to level of conversion table
         Y1 <<= 10;
         Cb = pCb[iCol+4];
         Cr = pCr[iCol+4];
         JPEGPixel(pJPEG, cOutput, 4+iCol, Y1, Cb, Cr);
         }
         // for bottom left block
         Y1 =  pY[iCol*2+DCTSIZE2*4];
         Y1 += pY[iCol*2+1+DCTSIZE2*4];
         Y1 += pY[iCol*2+8+DCTSIZE2*4];
         Y1 += pY[iCol*2+9+DCTSIZE2*4];
//         Y1 <<= 12;  // scale to level of conversion table
         Y1 <<= 10;
         Cb = pCb[iCol+32];
         Cr = pCr[iCol+32];
         JPEGPixel(pJPEG, cOutput+lsize*4, iCol, Y1, Cb, Cr);
         // for bottom right block
         if (iCol < iXCount2)
            {
            Y1 =  pY[iCol*2+DCTSIZE2*6];
            Y1 += pY[iCol*2+1+DCTSIZE2*6];
            Y1 += pY[iCol*2+8+DCTSIZE2*6];
            Y1 += pY[iCol*2+9+DCTSIZE2*6];
   //         Y1 <<= 12;  // scale to level of conversion table
            Y1 <<= 10;
            Cb = pCb[iCol+36];
            Cr = pCr[iCol+36];
            JPEGPixel(pJPEG, cOutput+lsize*4, 4+iCol, Y1, Cb, Cr);
            }
         } // for each column
      pY += 16; // skip to next line of source pixels
      pCb += 8;
      pCr += 8;
      cOutput += lsize;
      }

} /* JPEGPutMCU22HALF() */
/****************************************************************************
 *                                                                          *
 *  FUNCTION   : JPEGPutMCU22QUARTER(int, int, int, int*, char *)           *
 *                                                                          *
 *  PURPOSE    : Combine and output a subsampled color macro block.         *
 *                                                                          *
 ****************************************************************************/
static void JPEGPutMCU22QUARTER(PIL_PAGE *inpage, int x, int y, int lsize, signed short *pMCU, unsigned char *cOutput, JPEGDATA *pJPEG)
{
unsigned long Cr,Cb;
signed int Y1;
unsigned char *pY, *pCr, *pCb;
int iCol, iRow;

   pY  = (unsigned char *)&pMCU[MCU0];
   pCb = (unsigned char *)&pMCU[MCU4];
   pCr = (unsigned char *)&pMCU[MCU5];

   /* Convert YCC pixels into RGB pixels and store in output image */
   cOutput += y*4*lsize;
   if (pJPEG->iOptions & PIL_CONVERT_16BPP)
      cOutput += x<<3;
   else
      cOutput += x*12;
   for (iRow=0; iRow<2; iRow++)
      {
      for (iCol=0; iCol<2; iCol++)
         {
         // top left block
         Y1 =  pY[iCol] << 12; // scale to level of conversion table
         Cb  = pCb[0];
         Cr  = pCr[0];
         JPEGPixel(pJPEG, cOutput, iCol, Y1, Cb, Cr);
         // top right block
         Y1 =  pY[iCol+(DCTSIZE2*2)] << 12; // scale to level of conversion table
         Cb = pCb[1];
         Cr = pCr[1];
         JPEGPixel(pJPEG, cOutput, 2+iCol, Y1, Cb, Cr);
         // bottom left block
         Y1 =  pY[iCol+DCTSIZE2*4] << 12;  // scale to level of conversion table
         Cb = pCb[2];
         Cr = pCr[2];
         JPEGPixel(pJPEG, cOutput+lsize*2, iCol, Y1, Cb, Cr);
         // bottom right block
         Y1 =  pY[iCol+DCTSIZE2*6] << 12; // scale to level of conversion table
         Cb  = pCb[3];
         Cr  = pCr[3];
         JPEGPixel(pJPEG, cOutput+lsize*2, 2+iCol, Y1, Cb, Cr);
         } // for each column
      pY += 2; // skip 2 lines of source pixels
      cOutput += lsize;
      }

} /* JPEGPutMCU22QUARTER() */
/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILTIFFSHORT(char *, BOOL)                                 *
 *                                                                          *
 *  PURPOSE    : Retrieve a short value from a TIFF tag.                    *
 *                                                                          *
 ****************************************************************************/
static unsigned short PILTIFFSHORT(unsigned char *p, BOOL bMotorola)
{
unsigned short s;

if (bMotorola)
   s = *p * 0x100 + *(p+1);
else
   s = *p + *(p+1)*0x100;

return s;
} /* PILTIFFSHORT() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILTIFFLONG(char *, BOOL)                                  *
 *                                                                          *
 *  PURPOSE    : Retrieve a long value from a TIFF tag.                     *
 *                                                                          *
 ****************************************************************************/
static unsigned long PILTIFFLONG(unsigned char *p, BOOL bMotorola)
{
unsigned long l;

if (bMotorola)
   l = *p * 0x1000000 + *(p+1) * 0x10000 + *(p+2) * 0x100 + *(p+3);
else
   l = *p + *(p+1) * 0x100 + *(p+2) * 0x10000 + *(p+3) * 0x1000000;

return l;
} /* PILTIFFLONG() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : PILTIFFVALUE(char *, BOOL)                                 *
 *                                                                          *
 *  PURPOSE    : Retrieve the value from a TIFF tag.                        *
 *                                                                          *
 ****************************************************************************/
static int PILTIFFVALUE(unsigned char *p, BOOL bMotorola)
{
int i, iType;

   iType = PILTIFFSHORT(p+2, bMotorola);
   /* If pointer to a list of items, must be a long */
   if (PILTIFFSHORT(p+4, bMotorola) > 1)
      iType = 4;
   switch (iType)
      {
      case 3: /* Short */
         i = PILTIFFSHORT(p+8, bMotorola);
         break;
      case 4: /* Long */
         i = PILTIFFLONG(p+8, bMotorola);
         break;
      case 2: /* ASCII */
      case 5: /* Unsigned Rational */
      case 10: /* Signed Rational */
         i = PILTIFFLONG(p+8, bMotorola);
         break;
      default: /* to suppress compiler warning */
         i = 0;
         break;
      }
   return i;

} /* PILTIFFVALUE() */
