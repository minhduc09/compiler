/*************************************************************
* File name: buffer.h
* Compiler: MS Visual Studio 2019
* Author: Dao Minh Duc Hoang, 040981141
* Course: ST 8152 – Compilers, Lab Section: 014
* Assignment: 1
* Date: 2/06/2021
* Professor: Abdulah Kadri, Paulo Sousa
* Purpose: Define all constants, definitions, typedef and declare functions
* Function list: bCreate, bAddCh, bClean, bFree, bIsFull, bGetAddChOffset, bGetAddChOffset, bGetSize, bGetMode, 
		bGetMarkOffset, bSetMarkOffset, bFinish, bDisplay, bLoad, bIsEmpty, bGetCh, bRewind, bRetract, bRestore, 
		bGetChOffset, bGetIncrement,bGetContent,bufferAddCPosition,bGetFlags
*/


#ifndef BUFFER_H_
#define BUFFER_H_

/*#pragma warning(1:4001) *//*to enforce C89 type comments  - to make //comments an warning */

/*#pragma warning(error:4001)*//* to enforce C89 comments - to make // comments an error */

/* standard header files */
#include <stdio.h>  /* standard input/output */
#include <malloc.h> /* for dynamic memory allocation*/
#include <limits.h> /* implementation-defined data type ranges and limits */

/* constant definitions */
#define RT_FAIL_1 (-1)			
#define RT_FAIL_2 (-2)		
#define LOAD_FAIL (-2)			

#define DEFAULT_SIZE 200        
#define DEFAULT_INCREMENT 15  
#define FIXMODE_INCREMENT 0

/* You should add your own constant definitions here */
#define MAX_SIZE SHRT_MAX-1   /* maximum capacity*/ 

/* Buffer Modes */
#define FIXMODE 0        
#define ADDMODE 1         
#define MULMODE (-1)         

/* Add your bit-masks constant definitions here */
#define DEFAULT_FLAGS	0x3FFF 	// 0011.1111 1111.1111
#define SET_EOB			0x8000  // 1000.0000.0000.0000
#define RESET_EOB		0x7FFF  // 0111.1111.1111.1111
#define CHECK_EOB		0x8000  // 1000.0000.0000.0000
#define SET_R_FLAG		0x4000  // 0100.0000.0000.0000
#define RESET_R_FLAG	0xBFFF  // 1011.1111.1111.1111
#define CHECK_R_FLAG	0x4000  // 0100.0000.0000.0000

/* Constants used in buffer */
#define MAXINCREMENT 100 	
#define MAX_VALUE SHRT_MAX-1 		// 32767
#define RT_INC_FAIL 0x100		

/* user data type declarations */
typedef struct Buffer {
	char* content;         /* pointer to the beginning of character array (character buffer) */
	unsigned short size;            /* current dynamic memory size (in bytes) allocated to  buffer */
	char  increment;       /* character array increment factor */
	char  mode;            /* operational mode indicator*/
	short addCOffset;      /* the offset (in chars) to the add-character location */
	short getCOffset;      /* the offset (in chars) to the get-character location */
	short markOffset;      /* the offset (in chars) to the mark location */
	unsigned short flags;  /* contains character array reallocation and end-of-buffer flag */
} bStructure, * bPointer;

/* Function declarations */ 
bPointer bCreate(short, unsigned char, char);
bPointer bAddCh(bPointer const, char);
int bClean(bPointer const);
int bFree(bPointer const);
int bIsFull(bPointer const);
short bGetAddChOffset(bPointer const);
short bGetSize(bPointer const);
int bGetMode(bPointer const);
short bGetMarkOffset(bPointer const);
short bSetMarkOffset(bPointer const, short);
bPointer bFinish(bPointer const, char);
int bDisplay(bPointer const, char);
int bLoad(bPointer const, FILE* const);
int bIsEmpty(bPointer const);
char bGetCh(bPointer const);
int bRewind(bPointer const);
bPointer bRetract(bPointer const);
short bRestore(bPointer const);
short bGetChOffset(bPointer const);
size_t bGetIncrement(bPointer const);
char* bGetContent(bPointer const, short);
short bufferAddCPosition(bStructure* const);
unsigned short bGetFlags(bPointer const);

#endif 
