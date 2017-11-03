/* PIL - portable imaging library */
/* Copyright (c) 2000-2009 BitBank Software, Inc. */
/* Written by Larry Bank */
/* Project started 12/7/2000 */
/* A high speed imaging library designed for */
/* low memory/cpu environments such as Windows CE */
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

#ifndef _PIL_H_
#define _PIL_H_

#define PIL_BUFFER_SIZE    0x20000    // file buffer size
#define PIL_BUFFER_HIGHWATER 0x1f000   // 4K shy of end is a good place to reload

#ifndef _WIN32
typedef void * HANDLE;
#undef _cdecl
#define _cdecl
#undef _inline
#define _inline
#define	TEXT(x) (char *) x
#include <string.h>
#else
#include <windows.h>
#include <tchar.h>
#endif

// Progress indicator function
// returns 2 possible values:
#define PIL_PROGRESS_CONTINUE 0  // keep decoding
#define PIL_PROGRESS_CANCEL   1  // cancel current operation
typedef int (_cdecl *PILPROGRESS)(unsigned long ulCurrent, unsigned long ulTotal);

/* Supported compression types */
enum pilcomps {
PIL_COMP_NONE=0,	// uncompressed (flat bitmap)
PIL_COMP_G31D,	// CCITT Group 3 1-D
PIL_COMP_G32D,	// CCITT Group 3 2-D
PIL_COMP_G4,	// CCITT Group 4
PIL_COMP_MMR,  // IBM's stupid variation of G4
PIL_COMP_PCX,	// Paintshop PCX (run length)
PIL_COMP_TIFFPACKBITS,	// TIFF 4.0 Packbits (modified run length)
PIL_COMP_TIFFHUFFMAN,	// TIFF 4.0 Huffman (modified G3 1-D)
PIL_COMP_LZW,	// Unisys LZW
PIL_COMP_GIF,  // Unisys LZW with small differences
PIL_COMP_JPEG,	// JPEG baseline (DCT)
PIL_COMP_RLE,	// BitBank proprietary run-length encoding
PIL_COMP_PCL,	// HP PCL
PIL_COMP_WINRLE, // Windows BMP RLE
PIL_COMP_FLC,    // Aegis animator format
PIL_COMP_AVMFAX, // Packetized Modified Huffman data for AVM faxes
PIL_COMP_FLATE,  // PNG + PDF zlib flate compression
PIL_COMP_MJPEG,  // Motion JPEG
PIL_COMP_MJPEG_AB, // Motion JPEG A/B (interlaced)
PIL_COMP_CINEPAK,  // Cinepak video compression
PIL_COMP_H263,     // H263 video conferencing codec
PIL_COMP_H264,     // H264 video
PIL_COMP_MPEG,     // MPEG video
PIL_COMP_MPEG4,    // MPEG-4 video
PIL_COMP_MSVC, // Microsoft Video-1
};


/* Supported file types */
enum pilfiletypes
{
PIL_FILE_WINBMP=0,	// Windows BMP
PIL_FILE_OS2BMP,	// OS/2 BMP
PIL_FILE_WINB2P,   // 2 bits per pel bitmap supported by CE
PIL_FILE_PCX,	// Paintshop PCX
PIL_FILE_DCX,	// multipage PCX = DCX
PIL_FILE_TIFF,	// TIFF
PIL_FILE_JFIF,	// JFIF JPEG
PIL_FILE_IOCA,	// IBM IOCA
PIL_FILE_AWD,	// Microsoft AWD Fax
PIL_FILE_TARGA,	// Truevision targa
PIL_FILE_PDF,	// Adobe PDF
PIL_FILE_GIF,	// Compuserve GIF
PIL_FILE_PNG,	// Portable Network Graphic
PIL_FILE_PSEG,	// IBM PSEG (page segment = advanced function printing - AFP)
PIL_FILE_WINFAX,	// Winfax undocumented file
PIL_FILE_BITFAX,	// another undocumented fax file
PIL_FILE_CALS,    // Government imaging format
PIL_FILE_QL2FAX,  // quicklinks fax
PIL_FILE_PPV,     // pocket powerpoint
PIL_FILE_FLC,     // FLI/FLC animation
PIL_FILE_AVI,     // Microsoft RIFF AVI
PIL_FILE_QT,      // Apple QuickTime
PIL_FILE_WF10,    // WinFAX version 10
PIL_FILE_AVMFAX,  // AVM Fritz-Fax
PIL_FILE_CANONRAW,     // Canon RAW format
PIL_FILE_MINOLTARAW,   // Minolta RAW format
PIL_FILE_OLYMPUSRAW,   // Olympus RAW format
PIL_FILE_FUJIRAW,      // Fuji RAW format
PIL_FILE_EFXFAX,       // Everex eFax
PIL_FILE_MPEG,         // MPEG-1
};

#define PIL_PAGEFLAGS_BYTE			1	// word aligned rows
#define PIL_PAGEFLAGS_WORD			2	// word aligned rows
#define PIL_PAGEFLAGS_DWORD		4	// image has DWORD aligned rows
#define PIL_PAGEFLAGS_TOPDOWN		8	// image has row 0 starting at lowest address
#define PIL_PAGEFLAGS_BOTTOMUP	16	// image has row 0 starting at highest address
#define PIL_PAGEFLAGS_PREDICTOR  32 // lzw prediction
//#define PIL_PAGEFLAGS_BWGRAY		64	// image has a black-to-white (0-ff) grayscale palette
//#define PIL_PAGEFLAGS_WBGRAY		128	// image has a white-to-black (0-ff) grayscale palette


#define PIL_BITDIR_MSB_FIRST     0
#define PIL_BITDIR_LSB_FIRST     1

#define PIL_VIEWFLAGS_NONE		   	0	// no filtering
#define PIL_VIEWFLAGS_LIGHT			1	// bilevel density set to light
#define PIL_VIEWFLAGS_DARK		   	2	// bilevel density set to dark
#define PIL_VIEWFLAGS_SCALEGRAY		4	// scale bilevel to 4bpp gray
#define PIL_VIEWFLAGS_AVERAGE       8  // use pixel averaging when scaling down

#define PIL_FILE_STATE_CLOSED			1	// image file is closed
#define PIL_FILE_STATE_OPEN   		2	// file is still open
#define PIL_FILE_STATE_LOADED       3  // image data is loaded into pData of PIL_FILE

#define PIL_PAGE_STATE_LOADED       1     // page is completely in memory
#define PIL_PAGE_STATE_OPEN         2     // currently open file

/* Conversion options */
#define PIL_CONVERT_16BPP        1   // load JPEG as 16bpp
#define PIL_CONVERT_ANIMATE      2   // retain original buffer for animation
#define PIL_CONVERT_IGNORE_ERRORS 4  // Allow partial load of bad images
#define PIL_CONVERT_NOALLOC      8   // don't allocate output image buffer (for animation/video)
#define PIL_CONVERT_PROGRESSIVE 16   // progressive JPEG flag
#define PIL_CONVERT_HALFSIZE    32   // load JPEG as 1/2 size (really 1/4)
#define PIL_CONVERT_QUARTERSIZE 64   // load JPEG as 1/4 size (really 1/16)
#define PIL_CONVERT_EIGHTHSIZE  128  // load JPEG as 1/8 size (really 1/64)
#define PIL_CONVERT_THUMBNAIL   256  // load EXIF thumbnail or as 1/8 size if not present
#define PIL_CONVERT_QUALITY_HIGHEST  0  // highest quality encoding
#define PIL_CONVERT_QUALITY_HIGH     1  // high quality encoding
#define PIL_CONVERT_QUALITY_MED      2  // medium quality encoding
#define PIL_CONVERT_QUALITY_LOW      3  // low quality encoding
#define PIL_CONVERT_QUALITY_SUBSAMPLE 4  // subsample the color values (4:2:2)

/* Photometric interpretation */
#define PIL_PHOTOMETRIC_BLACKISZERO	0
#define PIL_PHOTOMETRIC_WHITEISZERO 1

/* Modification options */
enum pilmodifyops
{
PIL_MODIFY_ROTATE=1,
PIL_MODIFY_COLORS,
PIL_MODIFY_SIZE,
PIL_MODIFY_INVERT,
PIL_MODIFY_FLIPH,
PIL_MODIFY_FLIPV,
PIL_MODIFY_GRAY,
PIL_MODIFY_DESPECK,
PIL_MODIFY_LIGHTEN,
PIL_MODIFY_DARKEN,
PIL_MODIFY_FIXRES, // fix resolution of fax images
};

// Multithreaded operations
#define PIL_THREAD_EXIT 1
#define PIL_THREAD_READ 2
#define PIL_THREAD_WRITE 3
#define PIL_THREAD_CONVERT 4

/* Color conversion options */
#define PIL_COLORS_BEST       0
#define PIL_COLORS_DITHER     1
#define PIL_COLORS_ERRORDIFF  2

#define PIL_OPTION_APPEND     1

// structure for outputting variable length codes
typedef struct pil_code_tag
{
unsigned char *pOut; // current output pointer
int iLen;            // length of data in accumulator
unsigned long ulAcc; // code accumulator (holds codes until at least 16-bits ready to write
} PIL_CODE;

/* JPEG color component info */
typedef struct _jpegcompinfo
{
// These values are fixed over the whole image
// For compression, they must be supplied by the user interface
// for decompression, they are read from the SOF marker.
unsigned char component_needed;  /*  do we need the value of this component? */
unsigned char component_id;     /* identifier for this component (0..255) */
unsigned char component_index;  /* its index in SOF or cinfo->comp_info[] */
unsigned char h_samp_factor;    /* horizontal sampling factor (1..4) */
unsigned char v_samp_factor;    /* vertical sampling factor (1..4) */
unsigned char quant_tbl_no;     /* quantization table selector (0..3) */
// These values may vary between scans
// For compression, they must be supplied by the user interface
// for decompression, they are read from the SOS marker.
unsigned char dc_tbl_no;        /* DC entropy table selector (0..3) */
unsigned char ac_tbl_no;        /* AC entropy table selector (0..3) */
// These values are computed during compression or decompression startup
int true_comp_width;  /* component's image width in samples */
int true_comp_height; /* component's image height in samples */
// the above are the logical dimensions of the downsampled image
// These values are computed before starting a scan of the component
int MCU_width;        /* number of blocks per MCU, horizontally */
int MCU_height;       /* number of blocks per MCU, vertically */
int MCU_blocks;       /* MCU_width * MCU_height */
int downsampled_width; /* image width in samples, after expansion */
int downsampled_height; /* image height in samples, after expansion */
// the above are the true_comp_xxx values rounded up to multiples of
// the MCU dimensions; these are the working dimensions of the array
// as it is passed through the DCT or IDCT step.  NOTE: these values
// differ depending on whether the component is interleaved or not!!
// This flag is used only for decompression.  In cases where some of the
// components will be ignored (eg grayscale output from YCbCr image),
// we can skip IDCT etc. computations for the unused components.
} JPEGCOMPINFO;

#define DCTSIZE2  64
#define HUFF_TABLEN  273   // length of a Huffman table
#define MAX_COMPS_IN_SCAN  4 // maximum number of color components
#define MCU0 0
#define MCU1 64
#define MCU2 128
#define MCU3 192
#define MCU4 256
#define MCU5 320
//#define MCU6 384
//#define MCU7 448
//#define MCUTEMP 512

/* JPEG data structure */
typedef struct _jpegdata
{
unsigned long ulBits; // cached bits
int *pHuffAC, *pHuffDC;
int *pHuffACFast, *pHuffDCFast; // current huff table in use
unsigned char ucMaxACCol, ucMaxACRow; // maximum AC term stored in the current MCU
unsigned char ucQuantTable; // current quantization table used by JPEGDecodeMCU()
unsigned char ucDummy1; // to keep pMCUs dword aligned
short pMCUs[DCTSIZE2*6]; // MCU buffers - need up to 6 for 2:2 color subsampling
unsigned short sQuantTable[4*DCTSIZE2];
unsigned char cRangeTable[1024]; // for DCT
unsigned char cRangeTable2[1024]; // for YCC to RGB conversion
unsigned short usRangeTableR[1024]; // for direct 16bpp conversion
unsigned short usRangeTableG[1024];
unsigned short usRangeTableB[1024];
unsigned char ucHuffVals[HUFF_TABLEN*8];
unsigned char ucHuffTableUsed[8];
unsigned char ucHuffACDCBuf[4*0x180 + 4*0x1000]; // pre-allocate the maximum space for the Huffman decode tables
unsigned char *pPixel; // pointer to output pixels
int *huffdc[4], *huffac[4];
int *huffdcFast[4], *huffacFast[4]; // faster table for codes <= 9 bits
int iResInterval;
int iResCount;
int iOptions;
int iDataSize;
int iEXIF;        // Offset to JPEG EXIF info
int xdpi, ydpi;
int cx, cy;
int iScan, iScanStart, iScanEnd; // for progressive jpeg
int iOffset; // current offset into file
int iBit; // current bit position
int iDataEnd; // Ending offset of data
int iScanOffset[16]; // offset to start of each scan (progressive)
int iHuffOffset[16]; // offset to start of huffman table for this scan
JPEGCOMPINFO JPCI[4]; /* Max color components */
char cApproxBitsLow, cApproxBitsHigh;
unsigned char ucNumComponents;
unsigned char ucComponentsInScan;
unsigned char jpegsample;
char szComment[128]; // JPEG comment
} JPEGDATA;

/* Structure which holds a page of graphics data */
typedef struct _pilpage {
int iSize;        // size of the PIL_PAGE structure for version checking
long     lUser;            // user defined
int iWidth, iHeight; // page size in pixels
int iXres, iYres;	// Resolution in dots per inch
int iDataSize;		// Size of the data
int iX, iY;         // offsets to handle GIF properly
int iGIFDelay;    // display delay in hundredths of seconds and EXIF subIFD offset
void * iHandle;      // open file handle for read access
int iFilePos;     // current file read position (next read would start here)
unsigned char cGIFBits; // GIF packed fields
unsigned char cTransparent; // GIF transparent color
unsigned char cBackground; // GIF background color
int iPageWidth, iPageHeight; // GIF page size & JPEG EXIF true size
unsigned char *pData;		// pointer to image data
int iOffset;		// current file offset - for multipage files
unsigned char *pPalette;	// pointer to color palette for 4 & 8 bpp images
int      iPitch;           // for BMP data, the pitch of a row
// Strip info
int  iStripCount;          // Number of strips in the page
long *plStrips;            // Pointer to strip offsets
long *plStripSize;         // Pointer to strip size info
JPEGDATA *pJPEG;           // used for motion-jpeg - need to keep this structure around
int  iRowCount;            // Rows per strip
unsigned long ulIOThreadID;  // for multithreaded operations
int iDataAvailable;        // pre-loaded data for multithreaded operations
int iFileSize;             // needed to know when to stop reading
//long *pOffsets;            // Offsets to each data block

// Exif info
int iShutter;              // shutter speed
int iMetering;             // metering mode
int iFStop;                // F stop
int iExposure;             // exposure compensation
int iExposureProgram;      // aperture priority, full auto, etc.
int iISO;                  // ISO equivalent
int iFocalLength;
int iOriginalWidth;
int iOriginalHeight;
int iFlash;                // flash status bits
int iWhiteBalance;
int iOrientation;          // EXIF orientation: 1 = normal, 8 = 90 deg right, 3 = 180 deg, 6 = 270 deg
char cCompression;			// Compression type
char cPhotometric;			// photometric interpretation (or brightness for JPEG)
char cBitsperpixel;			// Supported values = {1,4,8,16,24}
char cPlanes;              // needed to properly support PCX images
char cFlags;			   	// includes alignment, top/bottom, etc.
char cBitDir;              // bit direction
char cState;				   // current state of this page
char cSpecial;             // special flag indicating if it is a video or has an audio note
char szDateTime[32];       // ASCII date and time
char szComment[128];       // Comment
char szInfo1[128];         // Text info 1
char szInfo2[128];         // Text info 2
} PIL_PAGE;

#define MAX_PAGES 4096  // maximum supported pages
/* Errors */
#define PIL_ERROR_SUCCESS     0   // no error
#define PIL_ERROR_MEMORY      -1  // out of memory
#define PIL_ERROR_FILENF      -2  // file not found
#define PIL_ERROR_IO          -3  // I/O error
#define PIL_ERROR_DECOMP      -4  // decompression error
#define PIL_ERROR_BITDEPTH    -5  // unsupported pixel bit depth
#define PIL_ERROR_INVPARAM    -6  // invalid parameter
#define PIL_ERROR_UNSUPPORTED -7  // unsupported feature
#define PIL_ERROR_BADHEADER   -8  // bad data encountered in header
#define PIL_ERROR_UNKNOWN     -9  // unknown file type
#define PIL_ERROR_PAGENF      -10 // page not found
#define PIL_ERROR_CANCELED    -11 // operation was cancelled
#define PIL_ERROR_LICENSE     -12 // unlicensed operation
#define PIL_ERROR_EMPTY_PAGE  -13 // an empty (no data) page
#define PIL_ERROR_AUDIO_CHAN_NOT_SUPPORTED -14	// # of audio channels not supported
#define PIL_ERROR_AUDIO_BITS_NOT_SUPPORTED -15	// Bit depth of audio stream not supported
#define PIL_ERROR_AUDIO_HW_NOT_SET		   -16	// Audio hardware parameters not set
#define PIL_ERROR_AUDIO_TYPE_UNKNOWN	   -17	// Unknown audio stream in video file (no CODEC)

#define INTELSHORT(p) *p + *(p+1) * 0x100
#define INTELLONG(p) (*p + *(p+1) * 0x100 + *(p+2) * 0x10000 + *(p+3) * 0x1000000)
#define MOTOSHORT(p) (((*(p))<<8) + *(p+1))
#define MOTOLONG(p) (((*p)<<24) + ((*(p+1))<<16) + ((*(p+2))<<8) + *(p+3))
#define MOTO24(p) ((*(p))<<16) + ((*(p+1))<<8) + *(p+2)

#define WRITEPATTERN32(p, o, l) p[o] |= (unsigned char)(l >> 24); p[o+1] |= (unsigned char)(l >> 16); p[o+2] |= (unsigned char)(l >> 8); p[o+3] |= (unsigned char)l;
#define WRITEMOTO32(p, o, l) p[o] = (unsigned char)(l >> 24); p[o+1] = (unsigned char)(l >> 16); p[o+2] = (unsigned char)(l >> 8); p[o+3] = (unsigned char)l;
#define WRITEMOTO16(p, o, l) p[o] = (unsigned char)(l >> 8); p[o+1] = (unsigned char)l;

/* Structure defining a view of a page of graphics data */
typedef struct _pilview {
int iSize;                 // size of the PIL_VIEW structure for version checking
int iWinX, iWinY;		   	// offset of view in document pixels
int iScaleX, iScaleY;		// scaling factor in multiples of 1/256
int iWidth, iHeight;		   // destination bitmap size
int iPitch;                // destination bytes per line
int iOrientation;          // View angle = 0,90,180,270
unsigned char *pBitmap;		// destination bitmap pointer
char cFilter;			   	// filtering options (e.g. scale to gray)
} PIL_VIEW;


typedef struct _tifftag
    {
    unsigned short sTagID;     /* TIFF Tag */
    unsigned short sDataType;  /* Tag data type */
    long  lNumVals;   /* Number of values */
    long  lValue;     /* Tag value */
    } TIFFTAG;

/* IBM PSEG image cell descriptor */
typedef struct _imgcell
    {
    int curx, cury;     /* Cell position */
    int cellx, celly;   /* Cell size */
    int fillx, filly;   /* Fill rectangle size of current image block */
    } IMGCELL;

/* Structure which holds current file info */
typedef struct _pilfile {
int iSize;                 // size of the PIL_FILE structure for version checking
long     lUser;            // user defined
void *	iFile;	   		// file handle
int      iFileSize;        // size of source file
unsigned char *pData;		// pointer to memory mapped file
int *pPageList;				// list of page offsets (e.g. for TIFF performance)
int *pSoundList;           // list of sound chunk offsets (video and audio)
int *pPageLens;            // Length of each page
int *pSoundLens;           // Length of each sound chunk
char *pKeyFlags;           // flags indicating key frames of video
JPEGDATA *pJPEG;           // Precalc'd tables for JPEG + video files
int iPage, iPageTotal;		// current page and total pages
int iSoundTotal;           // number of sound chunks
int iSampleFreq;           // sound sample frequency
int iSoundLen;             // total size of sound data
int iADPCMBlock;           // block size of ADPCM data
int iFrameRate;            // Video frame rate
int  iX, iY;               // Page size (video)
char cSoundBits;           // for video
char cBpp;                 // for video
char cCompression;         // for video
char cAudioCodec;          // for video
char cAudioChannels;       // for video
char cFileType;				// file type
char cState;				   // current state of this file

} PIL_FILE;

#ifdef __cplusplus
extern "C" {
#endif
/*--- Functions of the PIL ---*/
extern int mPILCalcSize(int x, int bpp);
extern int mPILDraw(PIL_PAGE *pPage, PIL_VIEW *pView, BOOL bTopDown, void *pGammaBrightness);
extern int mPILOpen(char *szFileName, PIL_FILE *pFile, int iOptions);
extern int mPILRead(PIL_FILE *pFile, PIL_PAGE *pPage, int iRequestedPage, int iOptions);
extern int mPILClose(PIL_FILE *pFile);
extern int mPILCreate(char *szFileName, PIL_FILE *pFile, int iOptions, int iFileType);
extern int mPILWrite(PIL_FILE *pFile, PIL_PAGE *pPage, int iFlags);
extern int mPILConvert(PIL_PAGE *pInPage, PIL_PAGE *pOutPage, int iOptions);
extern int mPILFree(PIL_PAGE *pPage);
extern int mPILResize(PIL_PAGE *pInPage, PIL_PAGE *pOutPage, int iNewX, int iNewY);
extern int mPILCrop(PIL_PAGE *pPage, PIL_VIEW *pView);
extern int mPILModify(PIL_PAGE *pPage, int iOperation, int iParam1, int iParam2);
extern int mPILFlipv(PIL_PAGE *pPage);
extern int mPILAnimate(PIL_PAGE *pPage, PIL_PAGE *pAnimatePage);
extern BOOL mPILTest(char *szFileName);
extern JPEGDATA * mPILPrepJPEGStruct(void);

#ifdef __cplusplus
}
#endif

#endif // #ifndef _PIL_H_
