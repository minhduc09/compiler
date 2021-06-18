/*************************************************************
 * File name: scanner.c
 * Compiler: MS Visual Studio 2019
 * Author: Dao Minh Duc Hoang, 040981141
 * Course: ST 8152 – Compilers, Lab Section: 014
 * Assignment: 2
 * Date: 3/19/2021
 * Professor: Abdulah Kadri, Paulo Sousa
 * Purpose: Functions implementing a Lexical Analyzer (Scanner)
 * Function list: startScanner, tokenizer, nextClass, nextState, funcAVID, funcSVID, funcIL, funcFPL, funcSL, funcErr, isKeyword
 */


 /* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
  * to suppress the warnings about using "unsafe" functions like fopen()
  * and standard sting library functions defined in string.h.
  * The define does not have any effect in Borland compiler projects.
  */
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

  /*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
extern bPointer stringLiteralTable;		/* String literal table */
int line;								/* current line number of the source code */
extern int errorNumber;					/* defined in platy_st.c - run-time error number */

static char debugMode = 0;				/* optional for debugging */

/* Local(file) global objects - variables */
static bPointer lexemeBuffer;			/* pointer to temporary lexeme buffer */
static bPointer sourceBuffer;			/* pointer to input source buffer */
/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */
static int nextClass(char c);			/* character class function */
static int nextState(int, char);		/* state machine function */
static int isKeyword(char* kw_lexeme);	/* keywords lookup function */


/************************************************************
* Function name: startScanner
* Purpose: initializes the scanner using defensive programming.
* Author: Dao Minh Duc Hoang
* Called functions: bIsEmpty(), bRewind(), bClean
* Parameters: bPointer psc_buf
* Return value: int (SUCESS or FAILURE)
**************************************************************/

int startScanner(bPointer psc_buf) {
	if (bIsEmpty(psc_buf))
		return EXIT_FAILURE; /*1*/
	/* in case the buffer has been read previously  */
	bRewind(psc_buf);
	bClean(stringLiteralTable);
	line = 1;
	sourceBuffer = psc_buf;
	return EXIT_SUCCESS; /*0*/
}

/************************************************************
* Function name: tokenizer
* Purpose: Main function of buffer, responsible to classify a char (or sequence of chars).
* Author: Dao Minh Duc Hoang
* Called functions: bGetCh(), isspace(), bRetract(), bSetMarkOffset(), bGetChOffset(), bRestore(), nextState(), bCreate(), strcpy(), bFinish(), bFree()
* Parameters: void
* Return value: currentToken (Token)
* Algorithm: In the first part, a specific sequence is detected (reading
 *		from buffer). In the second part, a pattern (defined by Regular Expression)
 *		is recognized and the appropriate function is called (related to final states 
 *		in the Transition Diagram).
**************************************************************/

Token tokenizer(void) {
	Token currentToken = { 0 }; /* token to return after pattern recognition. Set all structure members to 0 */
	unsigned char c;	/* input symbol */
	int state = 0;		/* initial state of the FSM */
	short lexStart;		/* start offset of a lexeme in the input char buffer (array) */
	short lexEnd;		/* end offset of a lexeme in the input char buffer (array)*/

	int lexLength;		/* token length */
	int i;				/* counter */
	unsigned char newc;	/* new char */

	while (1) { /* endless loop broken by token returns it will generate a warning */
		c = bGetCh(sourceBuffer);

		/* ------------------------------------------------------------------------
			Part 1: Implementation of token driven scanner.
			Every token is possessed by its own dedicated code
			-----------------------------------------------------------------------
		*/
		// Ignore white space, when discovering a new line, we must increment line
		if (isspace(c)) {  
			if (c == '\n')
				line++;
			continue;
		}
		
		switch (c) {
		case '%':
			newc = bGetCh(sourceBuffer);
			if (newc == '%') {
				//we know that we have a valid comment
				// continute reading until finding '\n' (test also SEOF)
				do {
					newc = bGetCh(sourceBuffer);
				} while (newc != '\n' && newc != CHARSEOF0 && newc != CHARSEOF255);
				if ((newc == CHARSEOF0) || (newc == CHARSEOF255)) {
					currentToken.code = SEOF_T;
					currentToken.attribute.seofType = SEOF_0;
					return currentToken;
				}
				// When discovering a new line, we must increment line
				line++;
				break;
			}
			else { 
				// consider an error - raise ERT_T and retract the last char
				bRetract(sourceBuffer);
				currentToken.code = ERR_T;
				currentToken.attribute.errLexeme[0] = '%';
				currentToken.attribute.errLexeme[1] = '\0';
				return currentToken;
			}
		case CHARSEOF0:	// Source end-of-file (SEOF) sentinel symbol
			currentToken.code = SEOF_T;
			currentToken.attribute.seofType = SEOF_0;
			return currentToken;
		case CHARSEOF255:	// Source end-of-file (SEOF) sentinel symbol
			currentToken.code = SEOF_T;
			currentToken.attribute.seofType = SEOF_EOF;
			return currentToken;
		case '(':		// Left parenthesis
			currentToken.code = LPR_T;
			return currentToken;
		case ')':		// Right parenthesis
			currentToken.code = RPR_T;
			return currentToken;
		case '{':		// Left brace
			currentToken.code = LBR_T;
			return currentToken;
		case '}':		// Right brace
			currentToken.code = RBR_T;
			return currentToken;
		case ',':		// Comma
			currentToken.code = COM_T;
			return currentToken;
		case ';':		// End of statement (semicolon)
			currentToken.code = EOS_T;
			return currentToken;
		case '+':		
			newc = bGetCh(sourceBuffer);
			if (newc == '+') { // String concatenation operator
				currentToken.code = SCC_OP_T;
				return currentToken;
			}
			//retract
			bRetract(sourceBuffer);
			// Addition operator 
			currentToken.code = ART_OP_T;
			currentToken.attribute.codeType = ADD;
			return currentToken;
		case '-':		// Substraction operator
			currentToken.code = ART_OP_T;
			currentToken.attribute.codeType = SUB;
			return currentToken;
		case '*':		// Multiplication operator
			currentToken.code = ART_OP_T;
			currentToken.attribute.codeType = MUL;
			return currentToken;
		case '/':		// Devision operator
			currentToken.code = ART_OP_T;
			currentToken.attribute.codeType = DIV;
			return currentToken;
		case '<':		// Less than relational operator
			currentToken.code = REL_OP_T;
			currentToken.attribute.codeType = LT;
			return currentToken;
		case '>':		// Greater than relational operator
			currentToken.code = REL_OP_T;
			currentToken.attribute.codeType = GT;
			return currentToken;
		case '=':		// Equal relational operator
			newc = bGetCh(sourceBuffer);
			if (newc == '=') {
				currentToken.code = REL_OP_T;
				currentToken.attribute.codeType = EQ;
				return currentToken;
			}
			//retract
			bRetract(sourceBuffer);
			currentToken.code = ASS_OP_T;
			return currentToken;
		case '!':		// Not equal relational operator
			newc = bGetCh(sourceBuffer);
			if (newc == '=') {
				currentToken.code = REL_OP_T;
				currentToken.attribute.codeType = NE;
				return currentToken;
			}
			// retract
			bRetract(sourceBuffer);
			// consider as an error token
			currentToken.code = ERR_T;
			currentToken.attribute.errLexeme[0] = '!';
			currentToken.attribute.errLexeme[1] = '\0';
			return currentToken;
		case '.':
			// set the current mark
			bSetMarkOffset(sourceBuffer, bGetChOffset(sourceBuffer));
			c = bGetCh(sourceBuffer);
			
			// if 'AND' or 'OR' or "NOT" 
			if (c == 'A' && bGetCh(sourceBuffer) == 'N' && bGetCh(sourceBuffer) == 'D' && bGetCh(sourceBuffer) == '.') {
				currentToken.code = LOG_OP_T;
				currentToken.attribute.logicalOperator = AND;
				return currentToken;
			}
			else if (c == 'O' && bGetCh(sourceBuffer) == 'R' && bGetCh(sourceBuffer) == '.') {
				currentToken.code = LOG_OP_T;
				currentToken.attribute.logicalOperator = OR;
				return currentToken;
			}
			else if (c == 'N' && bGetCh(sourceBuffer) == 'O' && bGetCh(sourceBuffer) == 'T' && bGetCh(sourceBuffer) == '.') {
				currentToken.code = LOG_OP_T;
				currentToken.attribute.logicalOperator = NOT;
				return currentToken;
			}
			else {
				bRestore(sourceBuffer);
				// consider as an error token
				currentToken.code = ERR_T;
				currentToken.attribute.errLexeme[0] = '.';
				currentToken.attribute.errLexeme[1] = '\0';
				return currentToken;
			}
			
		

		/* ------------------------------------------------------------------------
			Part 2: Implementation of Finite State Machine (DFA)
					   or Transition Table driven Scanner
					   Note: Part 2 must follow Part 1 to catch the illegal symbols
			-----------------------------------------------------------------------
		*/
			
		default: // general case
			state = nextState(state, c);
			// Appropiate values for the beginning and the end of lexeme
			// Continue the code...
			//  Set lexStart (use bGetChOffSet)
			//  Because of risk of retract, call bSetMarkOffset at lexStart
			lexStart = bSetMarkOffset(sourceBuffer, bGetChOffset(sourceBuffer)-1);
			while (stateType[state] == NOAS) {
				// Continue reading and going to the next state
				c = bGetCh(sourceBuffer);
				state = nextState(state, c);
			}
			if (stateType[state] == ASWR) {
				// Call the retract function from buffer
				bRetract(sourceBuffer);
			}
			// Set lexEnd (bGetChOffSet)
			lexEnd = bGetChOffset(sourceBuffer);
			// Calculate the lexLenght (use lexStart and lexEnd)
			lexLength = lexEnd - lexStart;
			// Allocate a lexemeBuffer using the appopriate size and using fixed mode
			lexemeBuffer = bCreate(lexLength, 0, 'f');
			// Test the lexemeBuffer and in case of problem EXIT
			if (!lexemeBuffer) {
				currentToken.code = RTE_T;
				errorNumber = 1;
				strcpy(currentToken.attribute.errLexeme, "RUN TIME ERROR: ");
				return currentToken;
			}
			// Call bRestore to adjust getCOffSet
			// sets getCOffset to the value of the current markOffset
			bRestore(sourceBuffer);
			// Use a loop to copy the content of sourceBuffer to lexemeBuffer and end it with \0
			while (bGetChOffset(sourceBuffer) < lexEnd) {
				c = bGetCh(sourceBuffer);
				bAddCh(lexemeBuffer, c);
			}
			bFinish(lexemeBuffer, '\0');
			//printf("CONTENT %s - STATE %d\n", bGetContent(lexemeBuffer, 0), state);
			currentToken = (*finalStateTable[state])(bGetContent(lexemeBuffer, 0));
			
			// Free the lexemeBuffer
			bFree(lexemeBuffer);
			return currentToken;
		} // switch

	} //while

} // tokenizer


/* DO NOT MODIFY THE CODE / COMMENT OF THIS FUNCTION */
/*************************************************************
 * Get Next State
	The assert(int test) macro can be used to add run-time diagnostic to programs
	and to "defend" from producing unexpected results.
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	(*) assert() is a macro that expands to an if statement;
	if test evaluates to false (zero) , assert aborts the program
	(by calling abort()) and sends the following message on stderr:
	(*) Assertion failed: test, file filename, line linenum.
	The filename and linenum listed in the message are the source file name
	and line number where the assert macro appears.
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	If you place the #define NDEBUG directive ("no debugging")
	in the source code before the #include <assert.h> directive,
	the effect is to comment out the assert statement.
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	The other way to include diagnostics in a program is to use
	conditional preprocessing as shown bellow. It allows the programmer
	to send more details describing the run-time problem.
	Once the program is tested thoroughly #define DEBUG is commented out
	or #undef DEBUF is used - see the top of the file.
 ************************************************************/

int nextState(int state, char c) {
	int col;
	int next;
	col = nextClass(c);
	next = transitionTable[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif
	assert(next != IS);
#ifdef DEBUG
	if (next == IS) {
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif
	return next;
}


/************************************************************
* Function name: nextClass
* Purpose: Get Next Token Class
* Author: Dao Minh Duc Hoang
* Called functions: isalpha(), isdigit()
* Parameters: char c (input character)
* Return value: int (column in the transition table)
* Algorithm: Create a function to return the column number in the transition table:
* Considering an input char c, you can identify the "class".
* For instance, a letter should return the column for letters, etc.
**************************************************************/

int nextClass(char c) {
	int val = -1;
	/*				[A-z](0),	[0-9](1),	.(2),	$(3),	'(4),	SEOF(5),	other(6) */
	if (isalpha(c)) {
		val = 0;
	}
	else if (isdigit(c)) {
		val = 1;
	}
	else {
		switch (c) {
		case '.':
			val = 2;
			break;
		case '$':
			val = 3;
			break;
		case '\'':
			val = 4;
			break;
		case CHARSEOF0:
			val = 5;
			break;
		default:
			val = 6;
		}
	}
	return val;
}

/************************************************************
* Function name: funcAVID
* Purpose: Acceptance State Function AVID
* Author: Dao Minh Duc Hoang
* Called functions: isKeyword(), strncpy(), strlen()
* Parameters: char* lexeme
* Return value: currentToken (Token)
* Algorithm: In this function, the pattern for AVID must be recognized.
 *		Since keywords obey the same pattern, is required to test if 
 *		the current lexeme matches with KW from language.
**************************************************************/

Token funcAVID(char* lexeme) {
	
	Token currentToken = { 0 };
	// Test if the lexeme is a keyword (use isKeyword)
	int i, kwIndex = isKeyword(lexeme);
	if (kwIndex != -1) {
		// If it is, set the code to KW_T
		currentToken.code = KW_T;
		//  At the same time, atribute codType must be the index of this keyword
		currentToken.attribute.keywordIndex = kwIndex;
		return currentToken;
	}
	// Else, adjust the code to AVID_T
	currentToken.code = AVID_T;
	
	// If the lengh exceed the limite defined for lexemes
	if (strlen(lexeme) > VID_LEN) {
		// make the first 8 characters significant
		strncpy(currentToken.attribute.vidLexeme, lexeme, (VID_LEN));
		// end it with \0
		currentToken.attribute.vidLexeme[VID_LEN] = '\0';
	}
	else {
		// Copy this content to "vidLexeme" and end it with \0
		strcpy(currentToken.attribute.vidLexeme, lexeme);
		currentToken.attribute.vidLexeme[strlen(lexeme)] = '\0';
	}	
	
	return currentToken;
}

/************************************************************
* Function name: funcSVID
* Purpose: Acceptance State Function SVID
* Author: Dao Minh Duc Hoang
* Called functions: strncpy(), strlen(), strcpy
* Parameters: char* lexeme
* Return value: currentToken (Token)
* Algorithm: In this function, the pattern for SVID must be recognized.
**************************************************************/


Token funcSVID(char* lexeme) {
	Token currentToken = { 0 };
	int i;
	currentToken.code = SVID_T;
	// If the lengh exceed the limite defined for lexemes
	if (strlen(lexeme) > VID_LEN) {
		// make the first 8 characters significant
		// copy 7 first characters from lexeme
		strncpy(currentToken.attribute.vidLexeme, lexeme, (VID_LEN - 1));
		// the last character is $ 
		currentToken.attribute.vidLexeme[VID_LEN - 1] = '$';
		// end the token with '\0'
		currentToken.attribute.vidLexeme[VID_LEN] = '\0';
	}
	else {
		// Copy this content to "vidLexeme" and end it with \0
		strcpy(currentToken.attribute.vidLexeme, lexeme);
		currentToken.attribute.vidLexeme[strlen(lexeme)] = '\0';
	}
	return currentToken;
}

/************************************************************
* Function name: funcIL
* Purpose: Function responsible to identify IL (integer literals).
* Author: Dao Minh Duc Hoang
* Called functions: atoi(), funcErr()
* Parameters: char* lexeme
* Return value: currentToken (Token)
* Algorithm:  
* - It is necessary respect the limit (ex: 2-byte integer in C).
 * - In the case of larger lexemes, error shoul be returned.
 * - Only first ERR_LEN characters are accepted and eventually, 
 *   additional three dots (...) should be put in the output.
**************************************************************/

Token funcIL(char* lexeme) {
	Token currentToken = { 0 };
	int num = atoi(lexeme);
	// num in the range 2-byte integer
	if ((num < SHRT_MIN) || (num > SHRT_MAX)) {
		currentToken = funcErr(lexeme);
	}
	else {
		currentToken.code = INL_T;
		currentToken.attribute.intValue = num;
	}
	return currentToken;
}

/************************************************************
* Function name: funcFPL
* Purpose: Function responsible to identify FPL (float point literals).
* Author: Dao Minh Duc Hoang
* Called functions: strtof(), funcErr(), strlen(), 
* Parameters: char* lexeme
* Return value: currentToken (Token)
* Algorithm:
* - It is necessary respect the limit (ex: 4-byte integer in C).
 * - In the case of larger lexemes, error shoul be returned.
 * - Only first ERR_LEN characters are accepted and eventually, 
 *   additional three dots (...) should be put in the output.
**************************************************************/

Token funcFPL(char* lexeme) {
	Token currentToken = { 0 };
	float num = strtof(lexeme, NULL);
	// num in range 4-byte float
	if (((num >= 0 && strlen(lexeme) > 7) && (num < FLT_MIN || num > FLT_MAX)) || (num < 0)) {
		currentToken = funcErr(lexeme);
	}
	else {
		currentToken.code = FPL_T;
		currentToken.attribute.floatValue = num;
	}
	return currentToken;
}

/************************************************************
* Function name: funcSL
* Purpose: Function responsible to identify SL (string literals).
* Author: Dao Minh Duc Hoang
* Called functions: bGetAddChOffset(), bAddCh(), strlen(), strcpy()
* Parameters: char* lexeme
* Return value: currentToken (Token)
* Algorithm:
 * - The lexeme must be stored in the String Literal Table
 *   (stringLiteralTable). You need to include the literals in
 *   this structure, using offsets. Remember to include \0 to
 *   separate the lexemes. Remember also to incremente the line.
**************************************************************/

Token funcSL(char* lexeme) {
	Token currentToken = { 0 };
	int length = strlen(lexeme)-2, i;
	
// Adjust attribute at contentString to correct position (use getAddChOffset over stringLiteralTable)
	currentToken.code = STR_T;
	currentToken.attribute.contentString = bGetAddChOffset(stringLiteralTable);
	
	if (length <= 0) {
		if (!bAddCh(stringLiteralTable, '\0')) {
			currentToken.code = RTE_T;
			errorNumber = 1;
			strcpy(currentToken.attribute.errLexeme, "RUN TIME ERROR: ");
			return currentToken;
		}
	}
	//  Copy lexeme to stringLiteralTable
	//  Eventual error (from bAddCh) should be treated as RTE_T
	else {
		for (i = 1; i <= length; i++) {
			if (!bAddCh(stringLiteralTable, lexeme[i])) {
				currentToken.code = RTE_T;
				errorNumber = 1;
				strcpy(currentToken.attribute.errLexeme, "RUN TIME ERROR: ");
				return currentToken;
			}
		}
		bAddCh(stringLiteralTable, '\0');
	}
	return currentToken;
}

/************************************************************
* Function name: funcErr
* Purpose: Acceptance State Function Error
* Author: Dao Minh Duc Hoang
* Called functions: trncpy(), strlen(), strcpy()
* Parameters: char* lexeme
* Return value: currentToken (Token)
* Algorithm: This function uses the errLexeme, respecting the limit given
 *   by ERR_LEN. If necessary, use three dots (...) to use the
 *   limit defined. The error lexeme contains line terminators,
 *   so remember to increment line.
**************************************************************/

Token funcErr(char* lexeme){
	Token currentToken = { 0 };
	// Set code to ERR_T
	currentToken.code = ERR_T;
	int length = strlen(lexeme);
	// Check the limit(use ERR_LEN)
	if (length > ERR_LEN) {
		// Copy lexeme to attribute errLexeme
		strncpy(currentToken.attribute.errLexeme, lexeme, ERR_LEN - 3);
		currentToken.attribute.errLexeme[ERR_LEN - 3] = '.';
		currentToken.attribute.errLexeme[ERR_LEN - 2] = '.';
		currentToken.attribute.errLexeme[ERR_LEN - 1] = '.';
		currentToken.attribute.errLexeme[ERR_LEN] = '\0';
	}
	else {
		// Copy lexeme to attribute errLexeme
		strcpy(currentToken.attribute.errLexeme, lexeme);
		currentToken.attribute.errLexeme[length] = '\0';
	}
	return currentToken;
}

/************************************************************
* Function name: isKeyword
* Purpose: This function checks if one specific lexeme is a keyword.
* Author: Dao Minh Duc Hoang
* Called functions: strcmp()
* Parameters: char* lexeme
* Return value: int (index of keyword table)
**************************************************************/

int isKeyword(char* lexeme) {
	int i = -1;
	
	if (lexeme == NULL) {
		return -1;
	}
	// check if the lexeme matches with keywordTable
	for (i = 0; i < KWT_SIZE; i++) {
		if (strcmp(keywordTable[i], lexeme) == 0) {
			return i;
		}
	}
	return -1;
}

