/*************************************************************
* COMPILERS COURSE - Algonquin College
* Code version: Fall, 2020
* This Code is EXCLUSIVE for professors (must not be shared)
* Author: Svillen Ranev - Paulo Sousa - Abdulah.
*************************************************************
* File name: buffer.c
* Compiler: MS Visual Studio 2019
* Author: Dao Minh Duc Hoang
* Course: CST 8152 – Compilers, Lab Section: 014
* Assignment: A1.
* Date: Feb 02 2021
* Professor: Paulo Sousa / Abdulah
* Purpose: Contains all functions to create and work on a buffer
* Function list: bCreate, bAddCh, bClean, bFree, bIsFull, bGetAddChOffset, bGetAddChOffset, bGetSize, bGetMode,
		bGetMarkOffset, bSetMarkOffset, bFinish, bDisplay, bLoad, bIsEmpty, bGetCh, bRewind, bRetract, bRestore,
		bGetChOffset, bGetIncrement,bGetContent,bufferAddCPosition,bGetFlags
*************************************************************/



#include "buffer.h"

/************************************************************
* Function name: bCreate
* Purpose: This function creates a new buffer in memory
* Author: Dao Minh Duc Hoang
* History/ Version: 1 2021-02-06
* Called functions: calloc(), malloc(), free()
* Parameters: short size, unsigned char increment, char mode
* Return value: bPointer
* Algorithm: create a new buffer with given size, increment and mode. Check all conditions for 3 modes.
*
**************************************************************/
bPointer bCreate(short size, unsigned char increment, char mode) {

	// Check if size within range
	if (size < 0 || size > MAX_SIZE) {
		return NULL;
	}
	// Create a buffer with default size and increment if size = 0
	if (size == 0) {
		size = DEFAULT_SIZE;
		increment = DEFAULT_INCREMENT;
	}

	// Create a new buffer in memory
	bPointer b = NULL;
	b = (bStructure*)calloc(1, sizeof(bStructure));
	if (!b) {
		return NULL;
	}

	//increment = (unsigned char)increment;
	// FIXMODE
	if (mode == 'f' || increment == '\0') {
		increment = FIXMODE_INCREMENT;
		b->mode = FIXMODE;
	}

	// ADDMODE
	else if (mode == 'a') {
		if (increment < 1 || increment > 255) {
			return NULL;
		}
		b->mode = ADDMODE;
	}
	// MULMODE
	else if (mode == 'm') {
		if (increment < 1 || increment > MAXINCREMENT) {
			return NULL;
		}
		b->mode = MULMODE;
	}
	else {
		free(b);
		return NULL;
	}

	// Create content of buffer with correct size
	b->content = (char*)malloc(size * sizeof(char));
	if (!b->content)
		return NULL;
	//if (!b->addCOffset) {
	//	printf("Error");
	//	free(b);
	//	return NULL;
	//}
	b->size = size;
	b->increment = increment;
	b->flags = DEFAULT_FLAGS;
	return b;
}

/************************************************************
* Function name: bAddCh
* Purpose:  The function adds (appends) the character ch to the buffer content, changes the value of addCOffset by 1, and saves the newly calculated size
* Author: Dao Minh Duc Hoang
* History/ Version: 1 2021-02-06
* Called functions: realloc()
* Parameters: bPointer const pBuffer, char ch
* Return value: bPointer
* Algorithm: Check if the size is enough to add the character. If not, expand it (depends on mode) and append the character
*
**************************************************************/

bPointer bAddCh(bPointer const pBuffer, char ch) {
	short availSpace, new_incre, new_size;
	char* new_cont;

	if (!pBuffer)
		return NULL;
	// reset the flags field r_flag bit to 0
	pBuffer->flags &= RESET_R_FLAG;

	// The buffer is full at max size or size exceeds max size
	if ((pBuffer->size == MAX_SIZE && pBuffer->size == pBuffer->addCOffset) || pBuffer->size > MAX_SIZE) {
		return NULL;
	}
	// The character buffer is already full
	if (pBuffer->size == pBuffer->addCOffset) {
		if (pBuffer->mode == FIXMODE) {
			return NULL;
		}
		else if (pBuffer->mode == ADDMODE) {
			new_size = pBuffer->size + (unsigned char)pBuffer->increment;

			// new size is positive and does not exceed the Max Value
			if (new_size >= 0 && new_size <= MAX_VALUE) {
				pBuffer->size = new_size;
			}
			// new size equal to Max Value
			else if (new_size == MAX_VALUE) {
				pBuffer->size = MAX_VALUE - 1;
			}
			// new size is negative
			else {
				return NULL;
			}
		}
		else if (pBuffer->mode == MULMODE) {
			availSpace = MAX_SIZE - pBuffer->size;
			new_incre = (short int)(availSpace * ((pBuffer->increment) / 100.0));
			new_size = pBuffer->size + new_incre;
			// current size cannot be incremented
			if (pBuffer->size == new_size && pBuffer->size < MAX_VALUE) {
				new_size = MAX_VALUE;
				
			}
			
			
			
		}
		new_cont = (char*)realloc(pBuffer->content, new_size * sizeof(char));
		if (!new_cont) {
			return NULL;
		}

		if (new_cont != pBuffer->content)
			pBuffer->flags |= SET_R_FLAG;

		pBuffer->content = new_cont;
		pBuffer->size = new_size;
	}
	pBuffer->content[pBuffer->addCOffset++] = ch;
	return pBuffer;
}


/************************************************************
* Function name: bClean
* Purpose:  The function retains the memory space currently allocated to the buffer, but reinitializes all appropriate data members of the given buffer structure
* Author: Dao Minh Duc Hoang
* History/ Version: 1 2021-02-06
* Called functions:
* Parameters: bPointer const pBuffer
* Return value: int for success or fail value
* Algorithm: Reset everything in buffer to the beginning
*
**************************************************************/

int bClean(bPointer const pBuffer) {
	if (!pBuffer)
		return RT_FAIL_1;

	pBuffer->addCOffset = 0;
	pBuffer->getCOffset = 0;
	pBuffer->markOffset = 0;
	pBuffer->flags = DEFAULT_FLAGS;
	return 0;
}
/************************************************************
* Function name: bFree
* Purpose:  The function de-allocates (frees) the memory occupied by the character buffer
and the buffer structure
* Author: Dao Minh Duc Hoang
* History/ Version: 1 2021-02-06
* Called functions: free()
* Parameters: bPointer const pBuffer
* Return value: int for success value
* Algorithm: Free the memory of the buffer
*
**************************************************************/
int bFree(bPointer const pBuffer) {
	free(pBuffer->content);
	free(pBuffer);
	return 0;
}
/************************************************************
* Function name: bIsFull
* Purpose:  The function returns 1 if the character buffer is full; it returns 0 otherwise
* Author: Dao Minh Duc Hoang
* History/ Version: 1 2021-02-06
* Called functions: 
* Parameters: bPointer const pBuffer
* Return value: int for success or fail value 
* Algorithm: Check if buffer is full by comparing size and addCOffSet
*
**************************************************************/

int bIsFull(bPointer const pBuffer) {
	if (!pBuffer)
		return RT_FAIL_1;
	if (pBuffer->addCOffset == pBuffer->size)
		return 1;
	return 0;
}
/************************************************************
* Function name: bGetAddChOffset
* Purpose:  The function returns the current addCOffset
* Author: Dao Minh Duc Hoang
* History/ Version: 1 2021-02-06
* Called functions:
* Parameters: bPointer const pBuffer
* Return value: short for the current addCOffSet or fail value
* Algorithm: 
*
**************************************************************/

short bGetAddChOffset(bPointer const pBuffer) {
	if (!pBuffer)
		return RT_FAIL_1;
	return pBuffer->addCOffset;
}
/************************************************************
* Function name: bGetSize
* Purpose:   The function returns the current size of the character buffer
* Author: Dao Minh Duc Hoang
* History/ Version: 1 2021-02-06
* Called functions:
* Parameters: bPointer const pBuffer
* Return value: short for the current size or fail value
* Algorithm:
*
**************************************************************/

short bGetSize(bPointer const pBuffer) {
	if (!pBuffer) {
		return RT_FAIL_1;
	}
	return pBuffer->size;
}
/************************************************************
* Function name: bGetMode
* Purpose:   The function returns the value of mode to the calling function
* Author: Dao Minh Duc Hoang
* History/ Version: 1 2021-02-06
* Called functions:
* Parameters: bPointer const pBuffer
* Return value: int for the mode or fail value
* Algorithm:
*
**************************************************************/

int bGetMode(bPointer const pBuffer) {
	if (!pBuffer) {
		return RT_FAIL_2;
	}
	return pBuffer->mode;
}
/************************************************************
* Function name: bGetMarkOffset
* Purpose:   The function returns the value of markOffset to the calling function
* Author: Dao Minh Duc Hoang
* History/ Version: 1 2021-02-06
* Called functions:
* Parameters: bPointer const pBuffer
* Return value: short for the markOffSet or fail value
* Algorithm:
*
**************************************************************/

short bGetMarkOffset(bPointer const pBuffer) {
	if (!pBuffer)
		return RT_FAIL_1;
	return pBuffer->markOffset;
}
/************************************************************
* Function name: bSetMarkOffset
* Purpose:   The function sets mark to markOffse
* Author: Dao Minh Duc Hoang
* History/ Version: 1 2021-02-06
* Called functions:
* Parameters: bPointer const pBuffer, short mark
* Return value: short for the new markOffSet or fail value
* Algorithm:
*
**************************************************************/

short bSetMarkOffset(bPointer const pBuffer, short mark) {
	if (!pBuffer || mark < 0 || mark > pBuffer->addCOffset)
		return RT_FAIL_1;
	pBuffer->markOffset = mark;
	return pBuffer->markOffset;
}
/************************************************************
* Function name: bSetMarkOffset
* Purpose:   The function shrinks the buffer to a new size
* Author: Dao Minh Duc Hoang
* History/ Version: 1 2021-02-06
* Called functions: realloc()
* Parameters: bPointer const pBuffer, char ch
* Return value: bPointer
* Algorithm: Reset r_flag, then increase the size by 1 and add the ch to the end
*
**************************************************************/

bPointer bFinish(bPointer const pBuffer, char ch) {
	if (!pBuffer || !pBuffer->content || !ch)
		return NULL;
	short new_size = 0;
	char* new_cont;
	pBuffer->flags &= RESET_R_FLAG;
	new_size = pBuffer->addCOffset + 1;
	new_cont = (char*)realloc(pBuffer->content, new_size * sizeof(char));
	if (!new_cont)
		return NULL;
	if (new_cont != pBuffer->content)
		pBuffer->flags |= SET_R_FLAG;
	pBuffer->size = new_size;
	pBuffer->content = new_cont;
	pBuffer->content[pBuffer->addCOffset++] = ch;
	return pBuffer;
}
/************************************************************
* Function name: bDisplay
* Purpose:   This function is intended to print the content of buffer
* Author: Dao Minh Duc Hoang
* History/ Version: 1 2021-02-06
* Called functions: bGetCh()
* Parameters: bPointer const pBuffer, char nl
* Return value: int as number of characters
* Algorithm: Print out each character till the end of buffer content 
*
**************************************************************/

int bDisplay(bPointer const pBuffer, char nl) {
	char ch;
	short numOfChars = 0;
	if (!pBuffer || !pBuffer->content) {
		return RT_FAIL_1;
	}
	ch = bGetCh(pBuffer);
	while (!(pBuffer->flags & CHECK_EOB)) {
		numOfChars++;
		printf("%c", ch);
		ch = bGetCh(pBuffer);
	}
	if (nl != 0)
		printf("\n");
	return numOfChars;
}
/************************************************************
* Function name: bLoad
* Purpose:   The function loads (reads) an open input file specified by fi into a buffer
specified by pBuffer
* Author: Dao Minh Duc Hoang
* History/ Version: 1 2021-02-06
* Called functions: fgetc(), bAddCh()
* Parameters: bPointer const pBuffer, FILE* const fi
* Return value: int as number of characters
* Algorithm: Take in each character till the end of file
*
**************************************************************/

int bLoad(bPointer const pBuffer, FILE* const fi) {
	int numOfChars = 0;
	char ch;
	if (!pBuffer || !fi) {
		return RT_FAIL_1;
	}
	ch = (char)fgetc(fi);
	while (!feof(fi)) {
		if (!bAddCh(pBuffer, ch)) {
			ungetc(ch, fi);
			return LOAD_FAIL;
		}
		ch = (char)fgetc(fi);
		numOfChars++;
	}
	return numOfChars;
}
/************************************************************
* Function name: bIsEmpty
* Purpose:   This functions checks if buffer is empty or not
* Author: Dao Minh Duc Hoang
* History/ Version: 1 2021-02-06
* Called functions: 
* Parameters: bPointer const pBuffer
* Return value: int as success or failure
* Algorithm: Compare addCOffSet to 0 to see if buffer is empty
*
**************************************************************/

int bIsEmpty(bPointer const pBuffer) {
	if (!pBuffer) {
		return RT_FAIL_1;
	}
	if (pBuffer->addCOffset == 0)
		return 1;
	return 0;
}

/************************************************************
* Function name: bGetCh
* Purpose:   This function is used to read the buffer
* Author: Dao Minh Duc Hoang
* History/ Version: 1 2021-02-06
* Called functions:
* Parameters: bPointer const pBuffer
* Return value: character that read from the buffer content
* Algorithm: Compare getCOffSet and addCOffSet to set r_flag. Return the character and increment getCOffSet
*
**************************************************************/

char bGetCh(bPointer const pBuffer) {
	if (!pBuffer) {
		return RT_FAIL_1;
	}
	if (pBuffer->getCOffset == pBuffer->addCOffset) {
		pBuffer->flags |= SET_EOB;
		return 0;
	}
	else {
		pBuffer->flags &= RESET_EOB;
	}
	return pBuffer->content[pBuffer->getCOffset++];
}

/************************************************************
* Function name: bRewind
* Purpose:   The function set the getCOffset and markOffset to 0, so that the buffer can be
re-read again
* Author: Dao Minh Duc Hoang
* History/ Version: 1 2021-02-06
* Called functions:
* Parameters: bPointer const pBuffer
* Return value: int for success or failure
* Algorithm: 
*
**************************************************************/

int bRewind(bPointer const pBuffer) {
	if (!pBuffer)
		return RT_FAIL_1;
	pBuffer->getCOffset = 0;
	pBuffer->markOffset = 0;
	return 0;
}

/************************************************************
* Function name: bRetract
* Purpose: The function decrements getCOffset by 1
* Author: Dao Minh Duc Hoang
* History/ Version: 1 2021-02-06
* Called functions:
* Parameters: bPointer const pBuffer
* Return value: int for success or failure
* Algorithm:
*
**************************************************************/

bPointer bRetract(bPointer const pBuffer) {
	if (!pBuffer)
		return NULL;
	pBuffer->getCOffset--;
	return pBuffer;
}
/************************************************************
* Function name: bRestore
* Purpose: The function sets getCOffset to the value of the current markOffset.
* Author: Dao Minh Duc Hoang
* History/ Version: 1 2021-02-06
* Called functions:
* Parameters: bPointer const pBuffer
* Return value: short for getCOffSet or failure value
* Algorithm: 
*
**************************************************************/


short bRestore(bPointer const pBuffer) {
	if (!pBuffer)
		return RT_FAIL_1;
	pBuffer->getCOffset = pBuffer->markOffset;
	return pBuffer->getCOffset;
}
/************************************************************
* Function name: bGetChOffset
* Purpose:The function returns getCOffset to the calling function
* Author: Dao Minh Duc Hoang
* History/ Version: 1 2021-02-06
* Called functions:
* Parameters: bPointer const pBuffer
* Return value: short for getCOffSet or failure value
* Algorithm:
*
**************************************************************/

short bGetChOffset(bPointer const pBuffer) {
	if (!pBuffer)
		return RT_FAIL_1;
	return pBuffer->getCOffset;
}

/************************************************************
* Function name: bGetIncrement
* Purpose:The function returns the non-negative value of increment to the calling
function
* Author: Dao Minh Duc Hoang
* History/ Version: 1 2021-02-06
* Called functions:
* Parameters: bPointer const pBuffer
* Return value: size_t of increment
* Algorithm:
*
**************************************************************/

size_t bGetIncrement(bPointer const pBuffer) {
	if (!pBuffer || pBuffer->increment > MAXINCREMENT)
		return RT_INC_FAIL;
	return (size_t)(unsigned char)pBuffer->increment;
}

/************************************************************
* Function name: bGetContent
* Purpose: The function returns a pointer to the location of the character buffer indicated
by pos that is the distance (measured in chars) from the beginning of the
character array (content)
* Author: Dao Minh Duc Hoang
* History/ Version: 1 2021-02-06
* Called functions:
* Parameters: bPointer const pBuffer, short chPosition
* Return value: char* 
* Algorithm:
*
**************************************************************/

char* bGetContent(bPointer const pBuffer, short chPosition) {
	if (!pBuffer || chPosition < 0 || chPosition > pBuffer->addCOffset)
		return NULL;
	return pBuffer->content+chPosition;
}

/************************************************************
* Function name: bufferAddCPosition
* Purpose:  TThe function returns getCOffset to the calling function.
* Author: Dao Minh Duc Hoang
* History/ Version: 1 2021-02-06
* Called functions:
* Parameters: bPointer const pBuffer
* Return value: short for getCOffSet or failure value
* Algorithm:
*
**************************************************************/

short bufferAddCPosition(bStructure* const pBuffer) {
	if (!pBuffer)
		return RT_FAIL_1;
	return pBuffer->getCOffset;
}

/************************************************************
* Function name: bGetFlags
* Purpose:  The function returns the flag field from buffer
* Author: Dao Minh Duc Hoang
* History/ Version: 1 2021-02-06
* Called functions:
* Parameters: bPointer const pBuffer
* Return value: unsigned short for flags or failure value
* Algorithm:
*
**************************************************************/

#define FLAGS_
#undef FLAGS_
#ifndef FLAGS_
unsigned short bGetFlags(bPointer const pBuffer) {
	if (!pBuffer)
		return RT_FAIL_1;
	return pBuffer->flags;
}
#else
#define bGetFlags(pBuffer) ((!pBuffer) ? return RT_FAIL_1 : return pBuffer->flags)
#endif
