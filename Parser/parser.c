/*************************************************************
* COMPILERS COURSE - Algonquin College
* Code version: Winter, 2021
*************************************************************
* File name: parser.c
* Compiler: MS Visual Studio 2019
* Author: Dao Minh Duc Hoang, 040981141
* Course: CST 8152 – Compilers, Lab Section: 014
* Assignment: A3.
* Date: April 18 2021
* Purpose: This file is the main file for Parser (.c)
* Function list: program(), optionalStatements(), inputStatement(), statements(), statement(), statementsPrime(), variableList(), variableIdentifier(), variableListPrime()
* assignmentStatement(), assignmentExpression(), selectionStatement(), outputStatement(), outputStatementPrime(), optVariableList(), arithmeticExpression(), unaryArithmeticExpression(),
* additiveArithmeticExpression(), multiplicativeArithmeticExpression(), primaryArithmeticExpression(), multiplicativeArithmeticExpressionPrime(), additiveArithmeticExpressionPrime(), stringExpression(), stringExpressionPrime(), 
* primaryStringExpression(), conditionalExpression(), logicalORExpression(), logicalORExpressionPrime(),logicalANDExpression(), logicalANDExpressionPrime(),logicalNOTExpression(), relationalExpression(),relationalA_Expression(), 
* relationalA_ExpressionPrime(), primaryA_Expression(), relationalS_Expression(), relationalS_ExpressionPrime(), primaryS_Expression()
*************************************************************/


#include "parser.h"

/*************************************************************
* Process Parser
* Function name: startParser
* Author: Dao Minh Duc Hoang
* Called functions: tokenizer(), program(), matchToken(), printf()
 ************************************************************/
void startParser(void) {
	lookahead = tokenizer();
	program();
	matchToken(SEOF_T, NO_ATTR);
	printf("%s\n", "PLATY: Source file parsed");
}

/*************************************************************
 * Match Token
 * Function name: matchToken
* Author: Dao Minh Duc Hoang
* Called functions: tokenizer(), syncErrorHandler(), printError()
 ************************************************************/

void matchToken(int tokenCode, int tokenAttribute) {
	int matchFlag = 1;
	switch (lookahead.code) {
	case KW_T:
		if (lookahead.code != tokenCode || lookahead.attribute.keywordIndex != tokenAttribute) {
			matchFlag = 0;
		}
		break;
	case REL_OP_T:
		if (lookahead.code != tokenCode || lookahead.attribute.relationalOperator != tokenAttribute) {
			matchFlag = 0;
		}
		break;
	case ART_OP_T:
		if (lookahead.code != tokenCode || lookahead.attribute.arithmeticOperator != tokenAttribute) {
			matchFlag = 0;
		}
		break;
	case LOG_OP_T:
		if (lookahead.code != tokenCode || lookahead.attribute.logicalOperator != tokenAttribute) {
			matchFlag = 0;
		}	
		break;
	default:
		if (lookahead.code != tokenCode) {
			matchFlag = 0;
		}

	}
	if (matchFlag && lookahead.code == SEOF_T) {
		return;
	}
		
	if (matchFlag) {
		lookahead = tokenizer();
		if (lookahead.code == ERR_T) {
			printError();
			lookahead = tokenizer();
			syntaxErrorNumber++;
			return;
		}
	}
	else {
		syncErrorHandler(tokenCode);
		return;
	}
}

/*************************************************************
 * Syncronize Error Handler
 * Function name: syncErrorHandler
* Author: Dao Minh Duc Hoang
* Called functions: tokenizer(), exit(), printf()
 ************************************************************/
void syncErrorHandler(int syncTokenCode) {
	printError();
	syntaxErrorNumber++;
	while (lookahead.code != syncTokenCode) {
		lookahead = tokenizer();

		if (lookahead.code == SEOF_T) {
			exit(syntaxErrorNumber);
			return;
		}
	}
	if (lookahead.code != SEOF_T) {
		lookahead = tokenizer();
		return;
	}
}


/*************************************************************
 * Print Error
 * Function name: printError
* Author: Dao Minh Duc Hoang
* Called functions: printf()
 ************************************************************/

void printError() {
	Token t = lookahead;
	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code) {
	case ERR_T:
		printf("%s\n", t.attribute.errLexeme);
		break;
	case SEOF_T:
		printf("NA\n");
		break;
	case AVID_T:
	case SVID_T:
		printf("%s\n", t.attribute.vidLexeme);
		break;
	case FPL_T:
		printf("%5.1f\n", t.attribute.floatValue);
		break;
	case INL_T:
		printf("%d\n", t.attribute.intValue);
		break;
	case STR_T:
		printf("%s\n", bGetContent(stringLiteralTable, t.attribute.contentString));
		break;
	case SCC_OP_T:
		printf("NA\n");
		break;
	case ASS_OP_T:
		printf("NA\n");
		break;
	case ART_OP_T:
		printf("%d\n", t.attribute.arithmeticOperator);
		break;
	case REL_OP_T:
		printf("%d\n", t.attribute.relationalOperator);
		break;
	case LOG_OP_T:
		printf("%d\n", t.attribute.logicalOperator);
		break;
	case LPR_T:
	case RPR_T:
	case LBR_T:
	case RBR_T:
		printf("NA\n");
		break;
	case KW_T:
		printf("%s\n", keywordTable[t.attribute.keywordIndex]);
		break;
	case COM_T:
		printf("NA\n");
		break;
	case EOS_T:
		printf("NA\n");
		break;
	case RTE_T:
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}
}


/*************************************************************
 * Program statement
 * Function name: program
* Author: Dao Minh Duc Hoang
* Called functions: matchToken(), optionalStatements(), printf()
 * BNF: <program> -> MAIN { <opt_statements> }
 * FIRST(<program>)= {KW_T (MAIN)}.
 ************************************************************/
void program(void) {
	matchToken(KW_T, MAIN);
	matchToken(LBR_T, NO_ATTR);
	optionalStatements();
	matchToken(RBR_T, NO_ATTR);
	printf("%s\n", "PLATY: Program parsed");
}




/*************************************************************
 * Optional statement
 * Function name: optionalStatements
* Author: Dao Minh Duc Hoang
* Called functions: statements(), printf()
 * <optionalStatements> -> <statements> | ϵ
 * FIRST(<optionalStatements>) = {AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE) }
 ************************************************************/
void optionalStatements(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		statements();
		break;
	case KW_T:
		if (lookahead.attribute.codeType == IF
			|| lookahead.attribute.codeType == WHILE
			|| lookahead.attribute.codeType == READ
			|| lookahead.attribute.codeType == WRITE) {
			statements();
			break;
		}
	default:
		;
	}
	printf("%s\n", "PLATY: Optional statements parsed");
}


/*************************************************************
 * Input Statement
  * Function name: inputStatement
* Author: Dao Minh Duc Hoang
* Called functions: matchToken(), printf(), variableList()
 * <inputStatement> -> READ (<variable list>)
 * FIRST(<inputStatement>) = { KW_T(READ) }
 ************************************************************/
void inputStatement(void) {
	matchToken(KW_T, READ);
	matchToken(LPR_T, NO_ATTR);
	variableList();
	matchToken(RPR_T, NO_ATTR);
	matchToken(EOS_T, NO_ATTR);
	printf("%s\n", "PLATY: Input statement parsed");
}


/*************************************************************
 * Statements
 * Function name: statements
* Author: Dao Minh Duc Hoang
* Called functions: statement(), statementsPrime(), printf()
 * <statements> -> <statement><statementsPrime>
 * FIRST(<statement>) = { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE) }
 ************************************************************/
void statements(void) {
	statement();
	statementsPrime();
	printf("%s\n", "PLATY: Statements parsed");
}

/*************************************************************
 * Statement
  * Function name: statement
* Author: Dao Minh Duc Hoang
* Called functions: assignmentStatement(), selectionStatement(), iterationStatement(), outputStatement(), printError()
 * <statement> -> <assignmentStatement> | <selectionStatement> | <iterationStatement> | <inputStatement> | <outputStatement>
 * FIRST(<statement>) = { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE) }
 ************************************************************/
void statement(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		assignmentStatement();
		printf("%s\n", "PLATY: Statement parsed");
		break;

	case KW_T:
		/* look for the keyword token */
		if (lookahead.attribute.keywordIndex == IF)
			selectionStatement();
		else if (lookahead.attribute.keywordIndex == WHILE)
			iterationStatement();
		else if (lookahead.attribute.keywordIndex == READ)
			inputStatement();
		else if (lookahead.attribute.keywordIndex == WRITE)
			outputStatement();
		printf("%s\n", "PLATY: Statement parsed");
		break;

	default:
		printError();
		printf("%s\n", "PLATY: Statement parsed");
	}
}

/*************************************************************
 * statementsPrime
  * Function name: statementsPrime
* Author: Dao Minh Duc Hoang
* Called functions: statement(), statementsPrime(),
 * <statementsPrime> -> <statement><statementsPrime> | ϵ
 * FIRST(<statementsPrime>) = { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE) }
 ************************************************************/
void statementsPrime(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		statement();
		statementsPrime();
		break;

	case KW_T:
		if (lookahead.attribute.keywordIndex == IF
			|| lookahead.attribute.keywordIndex == WHILE
			|| lookahead.attribute.keywordIndex == READ
			|| lookahead.attribute.keywordIndex == WRITE) {
			statement();
			statementsPrime();
			break;
		}

	default:
		break;
	}
}

/*************************************************************
 * Variable List
  * Function name: variableList
* Author: Dao Minh Duc Hoang
* Called functions: variableIdentifier(), variableListPrime(),
 * <variableList> -> <variableIdentifier><variableListPrime>
 * FIRST(<variableList>) = {AVID_T, SVID_T}
 ************************************************************/
void variableList(void) {
	variableIdentifier();
	variableListPrime();
	printf("%s\n", "PLATY: Variable list parsed");
}

/*************************************************************
 * Variable Indentifier
  * Function name: variableIdentifier
* Author: Dao Minh Duc Hoang
* Called functions: matchToken(), printError(),
 * <variableIdentifier> -> AVID_T | SVID_T
 * FIRST(<variableIdentifier>) = {AVID_T, SVID_T}
 ************************************************************/
void variableIdentifier(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		matchToken(lookahead.code, NO_ATTR);
		printf("%s\n", "PLATY: Variable identifier parsed");
		break;
	default:
		printError();
		printf("%s\n", "PLATY: Variable identifier parsed");
		break;
	}
}

/*************************************************************
 * Variable List Prime
 * Function name: variableListPrime
* Author: Dao Minh Duc Hoang
* Called functions: matchToken(), variableIdentifier(),variableListPrime(),
 * <variableListPrime> -> ,<variable indentifier><variableListPrime> | ϵ
 * FIRST(<variableIdentifier>) = { COM_T }
 ************************************************************/
void variableListPrime(void) {
	if (lookahead.code == COM_T) {
		matchToken(COM_T, NO_ATTR);
		variableIdentifier();
		variableListPrime();
	}
}

/*************************************************************
 * Assignment Statement
 * Function name: assignmentStatement
* Author: Dao Minh Duc Hoang
* Called functions: matchToken(), assignmentExpression()
 * <assignmentStatement> -> <assignmentExpression>;
 * FIRST(<assignmentStatement>) = { AVID_T, SVID_T }
 ************************************************************/
void assignmentStatement(void) {
	assignmentExpression();
	matchToken(EOS_T, NO_ATTR);
	printf("%s\n", "PLATY: Assignment statement parsed");
}

/*************************************************************
 * Assignment Expression
  * Function name: assignmentExpression
* Author: Dao Minh Duc Hoang
* Called functions: matchToken(), arithmeticExpression(), stringExpression()
 * <assignmentExpression> -> AVID = <arithmeticExpression> | SVID = <stringExpression>
 * FIRST(<assignmentExpression>) = { AVID_T, SVID_T }
 ************************************************************/
void assignmentExpression(void) {
	switch (lookahead.code) {
	case AVID_T:
		matchToken(AVID_T, NO_ATTR);
		matchToken(ASS_OP_T, EQ);
		arithmeticExpression();
		printf("%s\n", "PLATY: Assignment expression parsed");
		break;
	case SVID_T:
		matchToken(SVID_T, NO_ATTR);
		matchToken(ASS_OP_T, EQ);
		stringExpression();
		printf("%s\n", "PLATY: Assignment expression parsed");
		break;

	default:
		printError();
	}
}

/*************************************************************
 * Selection Statement
  * Function name: selectionStatement
* Author: Dao Minh Duc Hoang
* Called functions: matchToken(), preCondition(), conditionalExpression(), optionalStatements()
 * <selectionStatement> -> IF <preCondition> (<conditionalExpression>) THEN { <optionalStatements> } ELSE { <optionalStatements> };
 * FIRST(<selectionStatement>) = { KW_T(IF) }
 ************************************************************/
void selectionStatement(void) {
	matchToken(KW_T, IF);
	preCondition();
	matchToken(LPR_T, NO_ATTR);
	conditionalExpression();
	matchToken(RPR_T, NO_ATTR);

	matchToken(KW_T, THEN);
	matchToken(LBR_T, NO_ATTR);
	optionalStatements();
	matchToken(RBR_T, NO_ATTR);

	matchToken(KW_T, ELSE);
	matchToken(LBR_T, NO_ATTR);
	optionalStatements();
	matchToken(RBR_T, NO_ATTR);
	matchToken(EOS_T, NO_ATTR);

	printf("%s\n", "PLATY: Selection statement parsed");
}

/*************************************************************
 * Iteration Statement
  * Function name: iterationStatement
* Author: Dao Minh Duc Hoang
* Called functions: matchToken(), preCondition(), conditionalExpression(), optionalStatements()
 * <iterationStatement> -> WHILE <preCondition> (<conditionalExpression>) DO { <statements> } ;
 * FIRST(<selectionStatement>) = { KW_T(WHILE) }
 ************************************************************/
void iterationStatement(void) {
	matchToken(KW_T, WHILE);
	preCondition();
	matchToken(LPR_T, NO_ATTR);
	conditionalExpression();
	matchToken(RPR_T, NO_ATTR);

	matchToken(KW_T, DO);
	matchToken(LBR_T, NO_ATTR);
	statements();
	matchToken(RBR_T, NO_ATTR);
	matchToken(EOS_T, NO_ATTR);

	printf("%s\n", "PLATY: Iteration statement parsed");
}

/*************************************************************
 * Pre-condition
  * Function name: preCondition
* Author: Dao Minh Duc Hoang
* Called functions: matchToken(), printError()
 * <preCondition> -> TRUE | FALSE
 * FIRST(<selectionStatement>) = { KW_T(TRUE), KW_T(FALSE) }
 ************************************************************/
void preCondition(void) {
	switch (lookahead.attribute.keywordIndex) {
	case TRUE:
		matchToken(KW_T, TRUE);
		printf("%s\n", "PLATY: Pre-condition parsed");
		break;
	case FALSE:
		matchToken(KW_T, FALSE);
		printf("%s\n", "PLATY: Pre-condition parsed");
		break;
	default:
		printError();
	}
}

/*************************************************************
 * Output Statement
  * Function name: outputStatement
* Author: Dao Minh Duc Hoang
* Called functions: matchToken(), printError(), outputStatementPrime()
 * <outputStatement> -> WRITE(<outputStatementPrime>);
 * FIRST(<outputStatement>) = { KW_T(WRITE) }
 ************************************************************/
void outputStatement(void) {
	matchToken(KW_T, WRITE);
	matchToken(LPR_T, NO_ATTR);
	outputStatementPrime();
	matchToken(RPR_T, NO_ATTR);
	matchToken(EOS_T, NO_ATTR);
	printf("%s\n", "PLATY: Output statement parsed");
}

/*************************************************************
 * Output Statement Prime
   * Function name: outputStatementPrime
* Author: Dao Minh Duc Hoang
* Called functions: matchToken(), optVariableList()
 * <outputStatementPrime> -> <optVariableList> | STR_T
 * FIRST(<outputStatementPrime>) = { AVID_T, SVID_T, STR_T }
 ************************************************************/
void outputStatementPrime(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		optVariableList();
		printf("%s\n", "PLATY: Output variable list parsed");
		break;
	case STR_T:
		matchToken(STR_T, NO_ATTR);
		printf("%s\n", "PLATY: Output variable list parsed");
		break;
	default:
		printf("%s\n", "PLATY: Output variable list parsed");

	
	}
}

/*************************************************************
 * Optional Variable List
   * Function name: optVariableList
* Author: Dao Minh Duc Hoang
* Called functions: variableList()
 * <optVariableList> -> <variableList> | ϵ
 * FIRST(<optVariableList>) = { AVID_T, SVID_T }
 ************************************************************/
void optVariableList(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		variableList();
		break;
	default:
		;
	}
}

/*************************************************************
 * Arithmetic Expression
   * Function name: arithmeticExpression
* Author: Dao Minh Duc Hoang
* Called functions: unaryArithmeticExpression()
 * <arithmeticExpression> -> <unaryArithmeticExpression> | <additiveArithmeticExpression>
 * FIRST(<arithmeticExpression>) = { -, +, AVID_T, FPL_T, INL_T, LRT_T }
 ************************************************************/
void arithmeticExpression(void) {
	switch (lookahead.code) {
	case ART_OP_T:
		/* the attribute should be MULT adn DIV */
		switch (lookahead.attribute.arithmeticOperator) {
		case ADD:
		case SUB:
			unaryArithmeticExpression();
			break;

		default:
			printError();
			break;
		}
		printf("%s\n", "PLATY: Arithmetic expression parsed");
		break;

	case AVID_T:
	case FPL_T:
	case INL_T:
	case LPR_T:
		additiveArithmeticExpression();
		printf("%s\n", "PLATY: Arithmetic expression parsed");
		break;

	default:
		printError();
		break;
	}
}

/*************************************************************
 * Unary Arithmetic Expression
 * Function name: unaryArithmeticExpression
* Author: Dao Minh Duc Hoang
* Called functions: primaryArithmeticExpression(), matchToken()
 * <unaryArithmeticExpression> -> - <primaryArithmeticExpression> | + <primaryArithmeticExpression>
 * FIRST(<unaryArithmeticExpression>) = { -, +}
 ************************************************************/
void unaryArithmeticExpression(void) {
	switch (lookahead.code) {
	case ART_OP_T:
		/* the attribute should be PLUS and MINUS */
		switch (lookahead.attribute.arithmeticOperator) {
		case ADD:
			matchToken(ART_OP_T, ADD);
			primaryArithmeticExpression();
			//printf("%s\n", "PLATY: Unary arithmetic expression parsed");
			break;

		case SUB:
			matchToken(ART_OP_T, SUB);
			primaryArithmeticExpression();
			//printf("%s\n", "PLATY: Unary arithmetic expression parsed");
			break;

		default:
			printError();
			break;
		}

	default:
		break;
	}
	
}

/*************************************************************
 * Additive Arithmetic Expression
 * Function name: additiveArithmeticExpression
* Author: Dao Minh Duc Hoang
* Called functions: multiplicativeArithmeticExpression(), additiveArithmeticExpressionPrime()
 * <additiveArithmeticExpression> -> <multiplicativeArithmeticExpression><additiveArithmeticExpressionPrime>
 * FIRST(<additiveArithmeticExpression>) = { AVID_T, FPL_T, INL_T, LRT_T}
 ************************************************************/
void additiveArithmeticExpression(void) {
	multiplicativeArithmeticExpression();
	additiveArithmeticExpressionPrime();
	printf("%s\n", "PLATY: Additive arithmetic expression parsed");
}

/*************************************************************
 * Multiplicative Arithmetic Expression
 * Function name: multiplicativeArithmeticExpression
* Author: Dao Minh Duc Hoang
* Called functions: primaryArithmeticExpression(), multiplicativeArithmeticExpressionPrime()
 * <multiplicativeArithmeticExpression> -> <primaryArithmeticExpression><multiplicativeArithmeticExpressionPrime>
 * FIRST(<multiplicativeArithmeticExpression>) = { AVID_T, FPL_T, INL_T, LRT_T}
 ************************************************************/
void multiplicativeArithmeticExpression(void) {
	primaryArithmeticExpression();
	multiplicativeArithmeticExpressionPrime();
	printf("%s\n", "PLATY: Multiplicative arithmetic expression parsed");
}

/*************************************************************
 * Primary Arithmetic Expression
 * Function name: primaryArithmeticExpression
* Author: Dao Minh Duc Hoang
* Called functions: matchToken(), arithmeticExpression()
 * <primaryArithmeticExpression> -> AVID_T | FPL_T | INL_T | (<arithmeticExpression>)
 * FIRST(<primaryArithmeticExpression>) = { AVID_T, FPL_T, INL_T, LRT_T}
 ************************************************************/
void primaryArithmeticExpression(void) {
	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T:
		matchToken(lookahead.code, NO_ATTR);
		break;

	case LPR_T:
		matchToken(LPR_T, NO_ATTR);
		arithmeticExpression();
		matchToken(RPR_T, NO_ATTR);
		break;
	}
	printf("%s\n", "PLATY: Primary arithmetic expression parsed");
}

/*************************************************************
 * Multiplicative Arithmetic Expression
 * Function name: multiplicativeArithmeticExpressionPrime
* Author: Dao Minh Duc Hoang
* Called functions: matchToken(), primaryArithmeticExpression(), multiplicativeArithmeticExpressionPrime()
 * <multiplicativeArithmeticExpressionPrime> -> *<primaryArithmeticExpression><multiplicativeArithmeticExpressionPrime> | /<primaryArithmeticExpression><multiplicativeArithmeticExpressionPrime>
 * FIRST(<multiplicativeArithmeticExpressionPrime>) = { *, /}
 ************************************************************/
void multiplicativeArithmeticExpressionPrime(void) {
	switch (lookahead.code) {
	case ART_OP_T:
		switch (lookahead.attribute.arithmeticOperator) {
		case MUL:
			matchToken(ART_OP_T, MUL);
			primaryArithmeticExpression();
			multiplicativeArithmeticExpressionPrime();
			//printf("%s\n", "PLATY: Multiplicative arithmetic expression parsed");
			break;

		case DIV:
			matchToken(ART_OP_T, DIV);
			primaryArithmeticExpression();
			multiplicativeArithmeticExpressionPrime();
			//printf("%s\n", "PLATY: Multiplicative arithmetic expression parsed");
			break;
		}
	}
}

/*************************************************************
 * Additive Arithmetic Expression
 * Function name: additiveArithmeticExpressionPrime
* Author: Dao Minh Duc Hoang
* Called functions: matchToken(), multiplicativeArithmeticExpression(), additiveArithmeticExpressionPrime()
 * <additiveArithmeticExpressionPrime> -> +<multiplicativeArithmeticExpression><additiveArithmeticExpressionPrime> | -<multiplicativeArithmeticExpression><additiveArithmeticExpressionPrime>
 * FIRST(<additiveArithmeticExpressionPrime>) = { +, -}
 ************************************************************/
void additiveArithmeticExpressionPrime(void) {
	switch (lookahead.code) {
	case ART_OP_T:
		/* the attribute should be PLUS and MINUS */
		switch (lookahead.attribute.arithmeticOperator) {
		case ADD:
			matchToken(ART_OP_T, ADD);
			multiplicativeArithmeticExpression();
			additiveArithmeticExpressionPrime();
			break;

		case SUB:
			matchToken(ART_OP_T, SUB);
			multiplicativeArithmeticExpression();
			additiveArithmeticExpressionPrime();
			break;

		default:
			printError();
			break;
		}
	}
}

/*************************************************************
 * String Expression
 * Function name: stringExpression
* Author: Dao Minh Duc Hoang
* Called functions: primaryStringExpression(), stringExpressionPrime()
 * <stringExpression> -> <primaryStringExpression> | <stringExpressionPrime>
 * FIRST(<additiveArithmeticExpressionPrime>) = { SVID_T, STR_T}
 ************************************************************/
void stringExpression(void) {
	primaryStringExpression();
	stringExpressionPrime();
	printf("%s\n", "PLATY: String expression parsed");
}

/*************************************************************
 * String Expression Prime
 * Function name: stringExpressionPrime
* Author: Dao Minh Duc Hoang
* Called functions: matchToken(), primaryStringExpression(), stringExpressionPrime()
 * <stringExpressionPrime> -> ++ <primaryStringExpression><stringExpressionPrime> | ϵ
 * FIRST(<stringExpressionPrime>) = { ++ }
 ************************************************************/
void stringExpressionPrime(void) {
	switch (lookahead.code) {
	case SCC_OP_T:
		matchToken(SCC_OP_T, NO_ATTR);
		primaryStringExpression();
		stringExpressionPrime();
		break;

	default:
		break;
	}
}

/*************************************************************
 * Primary String Expression
* Function name: primaryStringExpression
* Author: Dao Minh Duc Hoang
* Called functions: matchToken()
 * <primaryStringExpression> -> SVID_T | STR_T
 * FIRST(<primaryStringExpression>) = { SVID_T, STR_T}
 ************************************************************/
void primaryStringExpression(void) {
	switch (lookahead.code) {
	case SVID_T:
		matchToken(SVID_T, NO_ATTR);
		break;

	case STR_T:
		matchToken(STR_T, NO_ATTR);
		break;
	}
	printf("%s\n", "PLATY: Primary string expression parsed");
}

/*************************************************************
 * Conditional Expression
* Function name: conditionalExpression
* Author: Dao Minh Duc Hoang
* Called functions: logicalORExpression()
 * <conditionalExpression> -> <logicalORExpression>
 * FIRST(<conditionalExpression>) = { .NOT. , AVID_T, FPL_T, INL_T, SVID_T, STR_T}
 ************************************************************/
void conditionalExpression(void) {
	logicalORExpression();
	printf("%s\n", "PLATY: Conditional expression parsed");
}

/*************************************************************
 * Logical OR Expression
* Function name: logicalORExpression
* Author: Dao Minh Duc Hoang
* Called functions: logicalANDExpression(), logicalORExpressionPrime()
 * <logicalORExpression> -> <logicalANDExpression><logicalORExpressionPrime>
 * FIRST(<logicalORExpression>) = { .NOT. , AVID_T, FPL_T, INL_T, SVID_T, STR_T}
 ************************************************************/
void logicalORExpression(void) {
	logicalANDExpression();
	logicalORExpressionPrime();
	printf("%s\n", "PLATY: Logical Or Expression parsed");
}
/*************************************************************
 * Logical OR Expression Prime
 * Function name: logicalORExpressionPrime
* Author: Dao Minh Duc Hoang
* Called functions: matchToken(), logicalANDExpression(), logicalORExpressionPrime()
 * <logicalORExpressionPrime> -> .OR. <logicalANDExpression><logicalORExpressionPrime>
 * FIRST(<logicalORExpressionPrime>) = { .OR.}
 ************************************************************/
void logicalORExpressionPrime(void) {
	switch (lookahead.code) {
	case LOG_OP_T:
		/* the attribute logical operator should be AND */
		switch (lookahead.attribute.logicalOperator) {
		case OR:
			matchToken(LOG_OP_T, OR);
			logicalANDExpression();
			logicalORExpressionPrime();
			printf("%s\n", "PLATY: Logical OR expression parsed");
			break;

		default:
			break;
		}
	default:
		break;
	}
}

/*************************************************************
 * Logical AND Expression
 * Function name: logicalANDExpression
* Author: Dao Minh Duc Hoang
* Called functions: logicalNOTExpression(), logicalANDExpressionPrime()
 * <logicalANDExpression> -> <logicalNOTExpression><logicalANDExpressionPrime>
 * FIRST(<logicalANDExpression>) = { .NOT. , AVID_T, FPL_T, INL_T, SVID_T, STR_T}
 ************************************************************/
void logicalANDExpression(void) {
	logicalNOTExpression();
	logicalANDExpressionPrime();
	printf("%s\n", "PLATY: Logical And Expression parsed");
}

/*************************************************************
 * Logical AND Expression Prime
  * Function name: logicalANDExpressionPrime
* Author: Dao Minh Duc Hoang
* Called functions: logicalNOTExpression(), logicalANDExpressionPrime(), matchToken()
 * <logicalANDExpressionPrime> -> .AND. <logicalNOTExpression><logicalANDExpressionPrime>
 * FIRST(<logicalANDExpressionPrime>) = { .AND.}
 ************************************************************/
void logicalANDExpressionPrime(void) {
	switch (lookahead.code) {
	case LOG_OP_T:
		/* the attribute logical operator should be AND */
		switch (lookahead.attribute.logicalOperator) {
		case AND:
			//printf("Hello\n");
			matchToken(LOG_OP_T, AND);
			logicalNOTExpression();
			logicalANDExpressionPrime();
			printf("%s\n", "PLATY: Logical AND expression parsed");
			break;

		default:
			//printf("Hello1\n");
			break;
		}
	default:
		//printf("Hello2\n");
		break;
	}
}

/*************************************************************
 * Logical NOT Expression
 * <logicalNOTExpression> -> .NOT. <relationalExpression> | <relationalExpression>
 * FIRST(<logicalNOTExpression>) = { .NOT. , AVID_T, FPL_T, INL_T, SVID_T, STR_T}
 ************************************************************/
void logicalNOTExpression(void) {
	switch (lookahead.code) {
	case LOG_OP_T:
		switch (lookahead.attribute.logicalOperator) {
		case NOT:
			matchToken(LOG_OP_T, NOT);
			relationalExpression();
			printf("%s\n", "PLATY: Logical Not Expression parsed");
			break;

		default:
			break;
		}
	case AVID_T:
	case FPL_T:
	case INL_T:
	case SVID_T:
	case STR_T:
		relationalExpression();
		printf("%s\n", "PLATY: Logical Not Expression parsed");
		break;
	default:
		break;
	}
}

/*************************************************************
 * Relational Expression
 * <relationalExpression> -> <relationalA_Expression> | <relationalS_Expression>
 * FIRST(<relationalExpression>) = { AVID_T, FPL_T, INL_T, SVID_T, STR_T}
 ************************************************************/
void relationalExpression(void) {
	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T:
		relationalA_Expression();
		printf("%s\n", "PLATY: Relational expression parsed");
		break;
	case SVID_T:
	case STR_T:
		relationalS_Expression();
		printf("%s\n", "PLATY: Relational expression parsed");
		break;
	default:
		break;
	}
}

/*************************************************************
 * Relational Arithmetic Expression
 * <relationalA_Expression> -> <primaryA_Expression><relationalA_ExpressionPrime>
 * FIRST(<relationalA_Expression>) = { AVID_T, FPL_T, INL_T}
 ************************************************************/
void relationalA_Expression(void) {
	primaryA_Expression();   
	relationalA_ExpressionPrime();
	printf("%s\n", "PLATY: Relational arithmetic expression parsed");
}

/*************************************************************
 * Relational Arithmetic Expression Prime
 * <relationalA_ExpressionPrime> -> == <primaryA_Expression> | != <primaryA_Expression> | < <primaryA_Expression> | > <primaryA_Expression>
 * FIRST(<relationalA_ExpressionPrime>) = { ==, !=, <, > }
 ************************************************************/
void relationalA_ExpressionPrime(void) {
	switch (lookahead.code) {
	case REL_OP_T:
		matchToken(REL_OP_T, lookahead.attribute.relationalOperator);
		primaryA_Expression();
		printf("%s\n", "PLATY: Relational arithmetic operator parsed");
		break;
	default:
		;
	}
	
}

/*************************************************************
 * Primary Arithmetic Relational Expression
 * <primaryA_Expression> -> AVID_T | FPL_T | INL_T
 * FIRST(<primaryA_Expression>) = { AVID_T, FPL_T, INL_T}
 ************************************************************/
void primaryA_Expression(void){
	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T:
		matchToken(lookahead.code, NO_ATTR);
		break;

	default: 
		;
	}
	printf("%s\n", "PLATY: Primary relational arithmetic expression parsed");
}

/*************************************************************
 * Relational String Expression
 * <relationalS_Expression> -> <primaryS_Expression><relationalS_ExpressionPrime>
 * FIRST(<relationalS_Expression>) = {SVID_T, STR_T}
 ************************************************************/
void relationalS_Expression(void) {
	primaryS_Expression();
	relationalS_ExpressionPrime();
	printf("%s\n", "PLATY: Relational string expression parsed");
}

/*************************************************************
 * Relational String Expression Prime
 * <relationalS_ExpressionPrime> -> ==<primaryS_Expression> | !=<primaryS_Expression> | > <primaryS_Expression> | < <primaryS_Expression>
 * FIRST(<relationalS_ExpressionPrime>) = {==, !=, <, >}
 ************************************************************/
void relationalS_ExpressionPrime(void) {
	switch (lookahead.code) {
	case REL_OP_T:
		matchToken(REL_OP_T, lookahead.attribute.relationalOperator);
		
		primaryS_Expression();
		printf("%s\n", "PLATY: Relational string operator parsed");
		break;
	default:
		printf("%s\n", "PLATY: Relational string operator parsed");

		
	}
	
}
/*************************************************************
 * Primary Arithmetic Relational Expression
 * <primaryS_Expression> -> <primaryStringExpression>
 * FIRST(<primaryS_Expression>) = { AVID_T, FPL_T, INL_T}
 ************************************************************/
void primaryS_Expression(void) {
	primaryStringExpression();
	printf("%s\n", "PLATY: Primary relational string expression parsed");
}