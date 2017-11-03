/************************************************************/
/*--- Generic I/O and memory routines                    ---*/
/* Copyright (c) 2000-2017 BitBank Software, Inc.           */
/************************************************************/
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

#ifndef _MINI_IO_H_
#define _MINI_IO_H_

#ifdef __cplusplus
extern "C" {
#endif

#ifndef BOOL
typedef int BOOL;
#define TRUE 1
#define FALSE 0
#endif // BOOL

extern BOOL MiniIOExists(char *szName);
extern unsigned long MiniIOSize(void * iHandle);
extern void * MiniIOOpen(char *);
extern void * MiniIOOpenRO(char *);
extern void * MiniIOCreate(char *);
extern int MiniIODelete(char *);
extern int MiniIORename(char *, char *);
extern unsigned long MiniIOSeek(void *, signed long, int);
extern unsigned int MiniIORead(void *, void *, unsigned int);
extern unsigned int MiniIOWrite(void *, void *, unsigned int);
extern void MiniIOClose(void *);
extern void * MiniIOAlloc(unsigned long);
extern void * MiniIOAllocNoClear(unsigned long);
extern void MiniIOFree(void *);
extern void * MiniIOAllocOutbuf(void);
extern void MiniIOFreeOutbuf(void *);

#ifdef __cplusplus
}
#endif

#endif // #ifndef _MINI_IO_H_
