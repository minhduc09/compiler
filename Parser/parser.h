/*************************************************************
* COMPILERS COURSE - Algonquin College
* Code version: Winter, 2021
*************************************************************
* File name: parser.h
* Compiler: MS Visual Studio 2019
* Author: Dao Minh Duc Hoang, 040981141
* Course: CST 8152 – Compilers, Lab Section: 014
* Assignment: A3
* Date: April 18 2020
* Purpose: This file is the main header for Parser (.h)
* Function list: program(), optionalStatements(), inputStatement(), statements(), statement(), statementsPrime(), variableList(), variableIdentifier(), variableListPrime()
* assignmentStatement(), assignmentExpression(), selectionStatement(), outputStatement(), outputStatementPrime(), optVariableList(), arithmeticExpression(), unaryArithmeticExpression(),
* additiveArithmeticExpression(), multiplicativeArithmeticExpression(), primaryArithmeticExpression(), multiplicativeArithmeticExpressionPrime(), additiveArithmeticExpressionPrime(), stringExpression(), stringExpressionPrime(),
* primaryStringExpression(), conditionalExpression(), logicalORExpression(), logicalORExpressionPrime(),logicalANDExpression(), logicalANDExpressionPrime(),logicalNOTExpression(), relationalExpression(),relationalA_Expression(),
* relationalA_ExpressionPrime(), primaryA_Expression(), relationalS_Expression(), relationalS_ExpressionPrime(), primaryS_Expression()
*************************************************************/


/* Inclusion section */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "token.h"
#include "buffer.h"

/* Global vars */
static Token lookahead;
int syntaxErrorNumber = 0;
extern bStructure* stringLiteralTable;
extern int line;
extern Token tokenizer();
extern char* keywordTable[];


/* Constants */
#define	NO_ATTR	-1
#define MAIN 	0
#define IF		1
#define THEN	2
#define	ELSE	3
#define WHILE	4
#define DO		5
#define READ	6
#define WRITE	7
#define TRUE	8
#define FALSE	9
// Continue the code

/* Function definitions */
void startParser(void);
void matchToken(int, int);
void syncErrorHandler(int);
void printError();
//void printMessage(char*);
void program(void);
void optionalStatements(void);
void inputStatement(void);
void statements(void);
void statement(void);
void statementsPrime(void);
void variableList(void);
void variableIdentifier(void);
void variableListPrime(void);
void assignmentStatement(void); 
void assignmentExpression(void);
void selectionStatement(void);
void outputStatement(void);
void outputStatementPrime(void);
void optVariableList(void);
void arithmeticExpression(void);
void unaryArithmeticExpression(void);
void additiveArithmeticExpression(void);
void multiplicativeArithmeticExpression(void);
void primaryArithmeticExpression(void);
void multiplicativeArithmeticExpressionPrime(void);
void additiveArithmeticExpressionPrime(void);
void stringExpression(void);
void stringExpressionPrime(void);
void primaryStringExpression(void);
void conditionalExpression(void);
void logicalORExpression(void);
void logicalORExpressionPrime(void);
void logicalANDExpression(void);
void logicalANDExpressionPrime(void);
void logicalNOTExpression(void);
void relationalExpression(void);
void relationalA_Expression(void);
void relationalA_ExpressionPrime(void);
void primaryA_Expression(void);
void relationalS_Expression(void);
void relationalS_ExpressionPrime(void);
void primaryS_Expression(void);