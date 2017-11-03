/****************************************************************************
 *                                                                          *
 * MODULE:  MINI_IO.C                                                       *
 *                                                                          *
 * DESCRIPTION: Generic IO module for Portable Imaging Library              *
 *                                                                          *
 * FUNCTIONS:                                                               *
 *            MiniIOOpen - Open a file for reading or writing                *
 *            MiniIOCreate - Create a file for writing                       *
 *            MiniIOClose - Close a file                                     *
 *            MiniIORead - Read a block of data from a file                  *
 *            MiniIOWrite - write a block of data to a file                  *
 *            MiniIOSeek - Seek to a specific section in a file              *
 *            MiniIODate - Provide date and time in TIFF 6.0 format          *
 *            MiniIOAlloc - Allocate a block of memory                       *
 *            MiniIOFree - Free a block of memory                            *
 *            MiniIOSignalThread - Send command to sub-thread                *
 *            MiniIOMsgBox - Display a message box                           *
 * COMMENTS:                                                                *
 *            Created the module  12/9/2000  - Larry Bank                   *
 *            added multithread support 3/27/2008                           *
 ****************************************************************************/
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
#include <string.h>
#include <stdlib.h>

#include "mini_io.h"
#include "mini_pil.h"

#define MAX_SIZE 0x400000 /* 4MB is good */

int MiniIOCheckSum(char *pString)
{
int c = 0;

   while(*pString)
   {
      c += (char)*pString;
	  pString++;
   }
   return c;

} /* Checksum() */

int MiniIOMsgBox(char *szMsg, char *szTitle)
{
#ifdef _WIN32
int iSum;

   iSum = MiniIOCheckSum(szMsg) + MiniIOCheckSum(szTitle);
   MessageBox(HWND_DESKTOP, szMsg, szTitle, MB_OK | MB_ICONSTOP);
   return iSum;
#else
	// Do nothing if it's not under Windows
	return -1;
#endif
} /* MiniIOMsgBox() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : MiniIODelete(char *)                                        *
 *                                                                          *
 *  PURPOSE    : Delete a file.                                             *
 *                                                                          *
 *  PARAMETERS : filename                                                   *
 *                                                                          *
 *  RETURNS    : 0 if successful, -1 if failure.                            *
 *                                                                          *
 ****************************************************************************/
int MiniIODelete(char *szFile)
{
      return remove(szFile);
} /* MiniIODelete() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : MiniIORename(char *, char *)                                *
 *                                                                          *
 *  PURPOSE    : Rename a file.                                             *
 *                                                                          *
 *  PARAMETERS : src filename, dest filename                                *
 *                                                                          *
 *  RETURNS    : 0 if successful, -1 if failure.                            *
 *                                                                          *
 ****************************************************************************/
int MiniIORename(char *szSrc, char *szDest)
{
#ifdef _WIN32
   if (MoveFile(szSrc, szDest))
      return 0;
   else
      return -1;
#else
   return -1;
#endif
} /* MiniIORename() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : MiniIOExists(char *)                                        *
 *                                                                          *
 *  PURPOSE    : Verify if a file exists or not.                            *
 *                                                                          *
 *  PARAMETERS : filename                                                   *
 *                                                                          *
 *  RETURNS    : BOOL - TRUE if exists, FALSE if not.                       *
 *                                                                          *
 ****************************************************************************/
BOOL MiniIOExists(char *szName)
{
void *ihandle;

	ihandle = (void *)fopen((char *)szName, "rb");

	if (ihandle == NULL)
	{
		return FALSE;
	}
	else
	{
    		fclose((FILE *)ihandle);
		return TRUE;
	}
} /* MiniIOExists() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : MiniIOOpenRO(char *)                                        *
 *                                                                          *
 *  PURPOSE    : Opens a file for reading only.                             *
 *                                                                          *
 *  PARAMETERS : filename                                                   *
 *                                                                          *
 *  RETURNS    : Handle to file if successful, -1 if failure                *
 *                                                                          *
 ****************************************************************************/
void * MiniIOOpenRO(char * fname)
{
   void * ihandle;

   ihandle = (void *)fopen((char *)fname, "rb");
   if (ihandle == NULL)
      {
      return (void *)-1;
      }
   else
      return ihandle;

} /* MiniIOOpenRO() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : MiniIOOpen(char *)                                          *
 *                                                                          *
 *  PURPOSE    : Opens a file for reading or writing                        *
 *                                                                          *
 *  PARAMETERS : filename                                                   *
 *                                                                          *
 *  RETURNS    : Handle to file if successful, -1 if failure                *
 *                                                                          *
 ****************************************************************************/
void * MiniIOOpen(char * fname)
{
   void *ihandle;

   ihandle = (void *)fopen((char *)fname, "r+b");
   if (ihandle == NULL)
      ihandle = MiniIOOpenRO(fname); /* Try readonly */
   return ihandle;
} /* MiniIOOpen() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : MiniIOCreate(char *)                                        *
 *                                                                          *
 *  PURPOSE    : Creates and opens a file for writing                       *
 *                                                                          *
 *  PARAMETERS : filename                                                   *
 *                                                                          *
 *  RETURNS    : Handle to file if successful, -1 if failure                *
 *                                                                          *
 ****************************************************************************/
void * MiniIOCreate(char * fname)
{
void *ohandle;

   ohandle = (void *)fopen((char *)fname, "w+b");
   if (ohandle == 0) // NULL means failure
   {
      ohandle = (void *)-1;
   }
   return ohandle;
} /* MiniIOCreate() */

unsigned long MiniIOSize(void * iHandle)
{
unsigned int ulSize;
unsigned int ulStart;

    ulStart = ftell((FILE *)iHandle);
	fseek((FILE *)iHandle, 0L, SEEK_END);
	ulSize = ftell((FILE *)iHandle);
	fseek((FILE *)iHandle, ulStart, SEEK_SET);
    return ulSize;
   
} /* MiniIOSize() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : MiniIOSeek(void *, signed long, int)                           *
 *                                                                          *
 *  PURPOSE    : Seeks within an open file                                  *
 *                                                                          *
 *  PARAMETERS : File Handle                                                *
 *               Offset                                                     *
 *               Method - 0=from beginning, 1=from current spot, 2=from end *
 *                                                                          *
 *  RETURNS    : New offset within file.                                    *
 *                                                                          *
 ****************************************************************************/
unsigned long MiniIOSeek(void * iHandle, signed long lOffset, int iMethod)
{
	   int iType;
	   long ulNewPos;

	   if (iMethod == 0) iType = SEEK_SET;
	   else if (iMethod == 1) iType = SEEK_CUR;
	   else iType = SEEK_END;

	   fseek((FILE *)iHandle, lOffset, iType);
	   ulNewPos = fgetpos((FILE *)iHandle, (fpos_t *)&ulNewPos);
	   return (unsigned int)ulNewPos;
} /* MiniIOSeek() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : MiniIORead(void *, void *, int)                                *
 *                                                                          *
 *  PURPOSE    : Read a block from an open file                             *
 *                                                                          *
 *  PARAMETERS : File Handle                                                *
 *               Buffer pointer                                             *
 *               Number of bytes to read                                    *
 *                                                                          *
 *  RETURNS    : Number of bytes read                                       *
 *                                                                          *
 ****************************************************************************/
unsigned int MiniIORead(void * iHandle, void * lpBuff, unsigned int iNumBytes)
{
unsigned int iBytes;

	iBytes = (int)fread(lpBuff, 1, iNumBytes, (FILE *)iHandle);
	return iBytes;

} /* MiniIORead() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : MiniIOWrite(void *, void *, int)                               *
 *                                                                          *
 *  PURPOSE    : Write a block from an open file                            *
 *                                                                          *
 *  PARAMETERS : File Handle                                                *
 *               Buffer pointer                                             *
 *               Number of bytes to write                                   *
 *                                                                          *
 *  RETURNS    : Number of bytes written                                    *
 *                                                                          *
 ****************************************************************************/
unsigned int MiniIOWrite(void * iHandle, void * lpBuff, unsigned int iNumBytes)
{
unsigned int iBytes;

	iBytes = (int)fwrite(lpBuff, 1, iNumBytes, (FILE *)iHandle);
	return iBytes;
} /* MiniIOWrite() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : MiniIOClose(void *)                                            *
 *                                                                          *
 *  PURPOSE    : Close a file                                               *
 *                                                                          *
 *  PARAMETERS : File Handle                                                *
 *                                                                          *
 *  RETURNS    : NOTHING                                                    *
 *                                                                          *
 ****************************************************************************/
void MiniIOClose(void * iHandle)
{
	fflush((FILE *)iHandle);
	fclose((FILE *)iHandle);

} /* MiniIOClose() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : MiniIOAlloc(long)                                           *
 *                                                                          *
 *  PURPOSE    : Allocate a block of writable memory.                       *
 *                                                                          *
 ****************************************************************************/
void * MiniIOAlloc(unsigned long size)
{

   return calloc(size, 1);
   
} /* MiniIOAlloc() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : MiniIOAllocNoClear(long)                                    *
 *                                                                          *
 *  PURPOSE    : Allocate a block of writable memory, no zero-fill.         *
 *                                                                          *
 ****************************************************************************/
void * MiniIOAllocNoClear(unsigned long size)
{

	return malloc(size);
   
} /* MiniIOAllocNoClear() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : MiniIOAllocOutbuf(long)                                     *
 *                                                                          *
 *  PURPOSE    : Allocate a block of writable memory.                       *
 *                                                                          *
 ****************************************************************************/
void * MiniIOAllocOutbuf(void)
{
   return malloc(MAX_SIZE);
} /* MiniIOAllocOutbuf() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : MiniIOFree(void *)                                          *
 *                                                                          *
 *  PURPOSE    : Free a block of writable memory.                           *
 *                                                                          *
 ****************************************************************************/
void MiniIOFree(void *p)
{
	free(p);
} /* MiniIOFree() */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : MiniIOFreeOutbuf(void *)                                    *
 *                                                                          *
 *  PURPOSE    : Free a block of writable memory.                           *
 *                                                                          *
 ****************************************************************************/
void MiniIOFreeOutbuf(void *p)
{
	free(p);
} /* MiniIOFree() */
