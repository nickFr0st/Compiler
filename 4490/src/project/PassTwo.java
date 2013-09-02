package project;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 8/31/13
 * Time: 10:30 AM
 */
public class PassTwo {
    private static final String ILLEGAL_EXPRESSION = "Illegal expression.";
    private static final String ILLEGAL_NEW_DECLARATION = "Illegal new_declaration";
    private static final String ILLEGAL_NEW_OPERATION = "Illegal new operation.";
    private static final String ILLEGAL_ATOI_OPERATION = "Illegal atoi operation.";
    private static final String ILLEGAL_ITOA_OPERATION = "Illegal itoa operation.";

    private static final String INVALID_ARGUMENT_LIST = "Invalid argument list.";
    private static final String INVALID_FUNCTION = "Invalid function.";
    private static final String INVALID_TYPE = "Invalid type.";
    private static final String INVALID_STATEMENT = "Invalid statement.";
    private static final String MISSING_ARRAY_CLOSE = "Array element missing array close.";

    private static final String MISSING_CLOSING_PARENTHESIS = "Missing closing parenthesis.";
    private static final String MISSING_OPENING_PARENTHESIS = "Missing opening parenthesis.";
    private static final String LINE = " Line: ";

    private static final String OPERATION = " operation.";
    private static final String ARGUMENT_LIST = " argument_list.";
    private static final String EXPRESSION = " expression.";
    private static final int ELEM_SIZE = 1;

    private String scope = "g.";
    private int variableId = 100;

    private StackHandler stackHandler;
    private LinkedHashMap<String, Symbol> symbolTable;
    private LexicalAnalyzer lexicalAnalyzer;
    private String errorList = "";

    public PassTwo(StackHandler stackHandler, LinkedHashMap<String, Symbol> symbolTable, LexicalAnalyzer lexicalAnalyzer) {
        this.stackHandler = stackHandler;
        this.symbolTable = symbolTable;
        this.lexicalAnalyzer = lexicalAnalyzer;
    }

    public LexicalAnalyzer getLexicalAnalyzer() {
        return lexicalAnalyzer;
    }

    public String getErrorList() {
        return errorList;
    }

    public void evaluate() {
        // pass one
        if (!compilation_unit()) {
            System.out.print(errorList);
            System.exit(0);
        }
        System.out.println("Semantic Analysis Successful!");
    }

    private boolean new_declaration() {
        if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
            return false;
        }

        if (lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name())) {

            // check format: "(" [argument_list] ")"
            lexicalAnalyzer.nextToken();
            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                errorList += ILLEGAL_NEW_DECLARATION + OPERATION + " " + MISSING_CLOSING_PARENTHESIS + LINE + lexicalAnalyzer.peekPreviousToken().getLineNum() + "\n";
                return false;
            }

            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                lexicalAnalyzer.nextToken();
                return true;
            }

            String errorCheck = errorList;

            if (!argument_list()) {
                if (!errorCheck.equals(errorList)) {
                    errorList += ILLEGAL_NEW_DECLARATION + ARGUMENT_LIST + LINE + lexicalAnalyzer.peekPreviousToken().getLineNum() + "\n";
                    return false;
                }
            }

            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple || !lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                errorList += ILLEGAL_NEW_DECLARATION + OPERATION + " " + MISSING_CLOSING_PARENTHESIS + LINE + lexicalAnalyzer.peekPreviousToken().getLineNum() + "\n";
                return false;
            }

            lexicalAnalyzer.nextToken();
            return true;
        }

        if (lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.ARRAY_BEGIN.name())) {

            // check format: "[" expression "]"
            lexicalAnalyzer.nextToken();
            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                errorList += ILLEGAL_NEW_DECLARATION + OPERATION + LINE + lexicalAnalyzer.peekPreviousToken().getLineNum() + "\n";
                return false;
            }

            if (!expression()) {
                errorList += ILLEGAL_NEW_DECLARATION + EXPRESSION + LINE + lexicalAnalyzer.peekPreviousToken().getLineNum() + "\n";
                return false;
            }

            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple || !lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.ARRAY_END.name())) {
                errorList += ILLEGAL_NEW_DECLARATION + OPERATION + ", " + MISSING_CLOSING_PARENTHESIS + LINE + lexicalAnalyzer.peekPreviousToken().getLineNum() + "\n";
                return false;
            }

            lexicalAnalyzer.nextToken();
            return true;
        }

        return false;
    }

    public boolean assignment_expression() {
        if (lexicalAnalyzer.getToken().getType().equals(KeyConst.NEW.getKey())) {
            // todo: need to do new operation
            // check format: "new" type new_declaration
            lexicalAnalyzer.nextToken();
            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple || !type(lexicalAnalyzer.getToken().getType())) {
                errorList += ILLEGAL_NEW_OPERATION + " " + INVALID_TYPE + LINE + lexicalAnalyzer.peekPreviousToken().getLineNum() + "\n";
                return false;
            }

            lexicalAnalyzer.nextToken();
            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple || !new_declaration()) {
                errorList += ILLEGAL_NEW_OPERATION + LINE + lexicalAnalyzer.peekPreviousToken().getLineNum() + "\n";
                return false;
            }

            return true;
        } else if (lexicalAnalyzer.getToken().getType().equals(KeyConst.ATOI.getKey())) {
            // todo: need to do atoi operation
            // check format: "atoi" "(" expression ")"
            lexicalAnalyzer.nextToken();
            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                errorList += ILLEGAL_ATOI_OPERATION + " " + MISSING_OPENING_PARENTHESIS + LINE + lexicalAnalyzer.peekPreviousToken().getLineNum() + "\n";
                return false;
            }

            if (!lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name())) {
                errorList += ILLEGAL_ATOI_OPERATION + " " + MISSING_OPENING_PARENTHESIS + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            lexicalAnalyzer.nextToken();
            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                errorList += ILLEGAL_ATOI_OPERATION + " " + MISSING_CLOSING_PARENTHESIS + LINE + lexicalAnalyzer.peekPreviousToken().getLineNum() + "\n";
                return false;
            }

            if (!expression()) {
                errorList += ILLEGAL_ATOI_OPERATION + " Invalid Expression." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                errorList += ILLEGAL_ATOI_OPERATION + " atoi can only contain one parameter. " + MISSING_CLOSING_PARENTHESIS + LINE + lexicalAnalyzer.peekPreviousToken().getLineNum() + "\n";
                return false;
            }

            if (!lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                errorList += ILLEGAL_ATOI_OPERATION + " atoi can only contain one parameter. " + MISSING_CLOSING_PARENTHESIS + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            lexicalAnalyzer.nextToken();
            return true;
        } else if (lexicalAnalyzer.getToken().getType().equals(KeyConst.ITOA.getKey())) {
            // todo: need to do itoa operation
            // check format: "itoa" "(" expression ")"
            lexicalAnalyzer.nextToken();
            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                errorList += ILLEGAL_ITOA_OPERATION + " " + MISSING_OPENING_PARENTHESIS + LINE + lexicalAnalyzer.peekPreviousToken().getLineNum() + "\n";
                return false;
            }

            if (!lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name())) {
                errorList += ILLEGAL_ITOA_OPERATION + " " + MISSING_OPENING_PARENTHESIS + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            lexicalAnalyzer.nextToken();
            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                errorList += ILLEGAL_ITOA_OPERATION + " " + MISSING_CLOSING_PARENTHESIS + LINE + lexicalAnalyzer.peekPreviousToken().getLineNum() + "\n";
                return false;
            }

            if (!expression()) {
                errorList += ILLEGAL_ITOA_OPERATION + " Invalid Expression." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                errorList += ILLEGAL_ITOA_OPERATION + " itoa can only contain one parameter. " + MISSING_CLOSING_PARENTHESIS + LINE + lexicalAnalyzer.peekPreviousToken().getLineNum() + "\n";
                return false;
            }

            if (!lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                errorList += ILLEGAL_ITOA_OPERATION + " itoa can only contain one parameter. " + MISSING_CLOSING_PARENTHESIS + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            lexicalAnalyzer.nextToken();
            return true;
        } else if (lexicalAnalyzer.getToken().getType().equals(KeyConst.THIS.getKey())) {
            // todo: need to do this operation
            lexicalAnalyzer.nextToken();
            return true;
        } else {
            if (!expression()) {
                errorList += "Invalid assignment expression." + LINE + lexicalAnalyzer.peekPreviousToken().getLineNum() + "\n";
                return false;
            }
            return true;
        }
    }

    public boolean expressionz() {
        if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
            return false;
        }

        Tuple token = lexicalAnalyzer.getToken();
        if (!(token.getType().equals(LexicalAnalyzer.tokenTypesEnum.ASSIGNMENT_OPR.name()) || isLogicalConnectiveExpression(token.getType()) || isBooleanExpression(token.getType()) || isMathematicalExpression(token.getType()))) {
            errorList += "Invalid expressionz." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
            return false;
        }

        lexicalAnalyzer.nextToken();
        if (lexicalAnalyzer.getToken() instanceof NullTuple || isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
            errorList += "expressionz missing right hand expression." + LINE + lexicalAnalyzer.peekPreviousToken().getLineNum() + "\n";
            lexicalAnalyzer.previousToken();
            return false;
        }

        if (token.getType().equals(LexicalAnalyzer.tokenTypesEnum.ASSIGNMENT_OPR.name())) {
            if (!assignment_expression()) {
                errorList += "Invalid assignment expression." + LINE + lexicalAnalyzer.peekPreviousToken().getLineNum() + "\n";
                return false;
            }
        } else {
            if (!expression()) {
                errorList += "Invalid expressionz expression." + LINE + lexicalAnalyzer.peekPreviousToken().getLineNum() + "\n";
                return false;
            }
        }

        return true;
    }

    public boolean expression() {
        if (lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name())) {
            // todo: need to do this
            // check format: "(" expression ")" [expressionz]
            lexicalAnalyzer.nextToken();
            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple || !expression()) {
                errorList += ILLEGAL_EXPRESSION + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple || !lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                errorList += ILLEGAL_EXPRESSION + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            lexicalAnalyzer.nextToken();
            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                return true;
            }

            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.ASSIGNMENT_OPR.name()) || isLogicalConnectiveExpression(lexicalAnalyzer.getToken().getType()) || isBooleanExpression(lexicalAnalyzer.getToken().getType()) || isMathematicalExpression(lexicalAnalyzer.getToken().getType())) {
                return expressionz();
            }

            return true;

        } else if (lexicalAnalyzer.getToken().getType().equals(KeyConst.TRUE.getKey()) || lexicalAnalyzer.getToken().getType().equals(KeyConst.FALSE.getKey()) || lexicalAnalyzer.getToken().getType().equals(KeyConst.NULL.getKey()) || lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.NUMBER.name()) || lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.CHARACTER.name())) {
            // check format: "value" [expressionz]
            String type;
            if (lexicalAnalyzer.getToken().getType().equals(KeyConst.TRUE.getKey()) || lexicalAnalyzer.getToken().getType().equals(KeyConst.FALSE.getKey())) {
                type = KeyConst.BOOL.name();
            } else if (lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.NUMBER.name())) {
                type = KeyConst.INT.name();
            } else if (lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.CHARACTER.name())) {
                type = KeyConst.CHAR.name();
            } else {
                type = "null";
            }

            stackHandler.literalPush(new Literal_SAR(lexicalAnalyzer.getToken(), type));
            lexicalAnalyzer.nextToken();

            if (lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.ASSIGNMENT_OPR.name()) || isLogicalConnectiveExpression(lexicalAnalyzer.getToken().getType()) || isBooleanExpression(lexicalAnalyzer.getToken().getType()) || isMathematicalExpression(lexicalAnalyzer.getToken().getType())) {
                return expressionz();
            }

            return true;

        } else if (lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name())) {
            // check format: identifier [ fn_arr_member ] [ member_refz ] [ expressionz ]
            stackHandler.identifierPush(new Identifier_SAR(lexicalAnalyzer.getToken(), scope));
            if (!stackHandler.identifierExist()) {
                return false;
            }

            errorList += stackHandler.getErrorList();

            lexicalAnalyzer.nextToken();

            String errCheck = errorList;

            fn_arr_member();
            if (!errCheck.equals(errorList)) {
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                return true;
            }

            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            member_refz();
            if (!errCheck.equals(errorList)) {
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                return true;
            }

            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.ASSIGNMENT_OPR.name()) || isLogicalConnectiveExpression(lexicalAnalyzer.getToken().getType()) || isBooleanExpression(lexicalAnalyzer.getToken().getType()) || isMathematicalExpression(lexicalAnalyzer.getToken().getType())) {
                return expressionz();
            }

            return true;
        } else if (lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.UNKNOWN.name())) {
            return !isUnknownSymbol(lexicalAnalyzer.getToken().getType());
        }
        return false;
    }

    private boolean argument_list() {
        if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
            return false;
        }

        // check format: expression { "," expression}
        if (!expression()) {
            errorList += INVALID_ARGUMENT_LIST + LINE + lexicalAnalyzer.peekPreviousToken().getLineNum() + "\n";
            return false;
        }

        while (!(lexicalAnalyzer.getToken() instanceof NullTuple) && lexicalAnalyzer.getToken().getName().equals(",")) {
            lexicalAnalyzer.nextToken();
            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (!expression()) {
                errorList += INVALID_ARGUMENT_LIST + LINE + lexicalAnalyzer.peekPreviousToken().getLineNum() + "\n";
                return false;
            }
        }
        return true;
    }

    private boolean fn_arr_member() {
        if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
            return false;
        }

        // check format: "(" [ argument_list ] ")" | "[" expression "]"
        if (lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name())) {

            //check format: "(" [ argument_list ] ")"
            lexicalAnalyzer.nextToken();
            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                errorList += INVALID_FUNCTION + " " + MISSING_CLOSING_PARENTHESIS + LINE + lexicalAnalyzer.peekPreviousToken().getLineNum() + "\n";
                return false;
            }

            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                lexicalAnalyzer.nextToken();
                return true;
            }

            if (!argument_list()) {
                errorList += INVALID_ARGUMENT_LIST + " in function parameter." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple || !lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                errorList += INVALID_FUNCTION + LINE + lexicalAnalyzer.peekPreviousToken().getLineNum() + "\n";
                return false;
            }

            lexicalAnalyzer.nextToken();
            return true;

        } else if (lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.ARRAY_BEGIN.name())) {

            //check format: "[" expression "]"
            lexicalAnalyzer.nextToken();
            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple || !expression()) {
                errorList += "Invalid array expression." + LINE + lexicalAnalyzer.peekPreviousToken().getLineNum() + "\n";
                return false;
            }

            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple || !lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.ARRAY_END.name())) {
                errorList += MISSING_ARRAY_CLOSE + LINE + lexicalAnalyzer.peekPreviousToken().getLineNum() + "\n";
                return false;
            }

            lexicalAnalyzer.nextToken();
            return true;
        }
        return false;
    }

    private boolean member_refz() {
        if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
            return false;
        }

        // check format: "." identifier [ fn_arr_member ] [ member_refz ]
        if (!lexicalAnalyzer.getToken().getName().equals(".")) {
            return false;
        }

        lexicalAnalyzer.nextToken();
        if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
            return false;
        }

        if (lexicalAnalyzer.getToken() instanceof NullTuple || !lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name())) {
            errorList += "Invalid member ref." + LINE + lexicalAnalyzer.peekPreviousToken().getLineNum() + "\n";
            return false;
        }

        lexicalAnalyzer.nextToken();
        if (lexicalAnalyzer.getToken() instanceof NullTuple) {
            return true;
        }

        if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
            return false;
        }

        String errCheck = errorList;

        fn_arr_member();
        if (!errCheck.equals(errorList)) {
            return false;
        }

        if (lexicalAnalyzer.getToken() instanceof NullTuple) {
            return true;
        }

        if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
            return false;
        }

        member_refz();
        return errCheck.equals(errorList);
    }

    public boolean statement() {
        if (lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.BLOCK_BEGIN.name())) {
            // todo: need to do this
            // check format: "{" {statement} "}"
            String errorCheck = errorList;

            lexicalAnalyzer.nextToken();
            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            while (statement()) {
                lexicalAnalyzer.nextToken();
                if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                    return false;
                }
            }

            if (!errorCheck.equals(errorList)) {
                return false;
            }

            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple || !lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.BLOCK_END.name())) {
                errorList += INVALID_STATEMENT + " Missing a closing block." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            return true;

        } else if (lexicalAnalyzer.getToken().getName().equals(KeyConst.IF.getKey())) {
            // todo: need to do this
            // check format: "if" "(" expression ")" statement [ "else" statement ]
            lexicalAnalyzer.nextToken();
            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple || !lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name())) {
                errorList += INVALID_STATEMENT + " " + MISSING_OPENING_PARENTHESIS + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            lexicalAnalyzer.nextToken();
            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple || !expression()) {
                errorList += "'if' statement requires a valid expression." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple || !lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                errorList += INVALID_STATEMENT + " " + MISSING_CLOSING_PARENTHESIS + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            lexicalAnalyzer.nextToken();
            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (!statement()) {
                errorList += INVALID_STATEMENT + " 'if' statement." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            if (!(lexicalAnalyzer.peek() instanceof NullTuple) && lexicalAnalyzer.peek().getName().equals(KeyConst.ELSE.getKey())) {

                lexicalAnalyzer.nextToken();
                lexicalAnalyzer.nextToken();

                if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                    return false;
                }

                if (lexicalAnalyzer.getToken() instanceof NullTuple || !statement()) {
                    errorList += INVALID_STATEMENT + " a valid statement is required after an else statement." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                    return false;
                }
            }

            return true;

        } else if (lexicalAnalyzer.getToken().getName().equals(KeyConst.WHILE.getKey())) {
            // todo: need to do this
            // check format: "while" "(" expression ")" statement
            lexicalAnalyzer.nextToken();
            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple || !lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name())) {
                errorList += INVALID_STATEMENT + " " + MISSING_OPENING_PARENTHESIS + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            lexicalAnalyzer.nextToken();
            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple || !expression()) {
                errorList += "'while' statement requires a valid expression." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple || !lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                errorList += INVALID_STATEMENT + " " + MISSING_CLOSING_PARENTHESIS + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            lexicalAnalyzer.nextToken();
            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (!statement()) {
                errorList += INVALID_STATEMENT + " 'while' statement." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            return true;

        } else if (lexicalAnalyzer.getToken().getName().equals(KeyConst.RETURN.getKey())) {
            // todo: need to do this
            // check format: "return" [ expression ] ";"
            lexicalAnalyzer.nextToken();
            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                errorList += INVALID_STATEMENT + " 'return' statement must end with a ';'." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.EOT.name())) {
                return true;
            }

            if (!expression()) {
                errorList += "Invalid 'return' statement expression." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple || !lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.EOT.name())) {
                errorList += INVALID_STATEMENT + " 'return' statement must end with a ';'." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            return true;

        } else if (lexicalAnalyzer.getToken().getName().equals(KeyConst.COUT.getKey())) {
            // todo: need to do this
            // check format: "cout" "<<" expression ";"
            lexicalAnalyzer.nextToken();
            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple || !lexicalAnalyzer.getToken().getName().equals("<<")) {
                errorList += INVALID_STATEMENT + "'cout' statement missing extraction operator." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            lexicalAnalyzer.nextToken();
            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple || !expression()) {
                errorList += "'cout' statement requires a valid expression." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple || !lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.EOT.name())) {
                errorList += INVALID_STATEMENT + " 'cout' statement must end with a ';'." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            return true;

        } else if (lexicalAnalyzer.getToken().getName().equals(KeyConst.CIN.getKey())) {
            // todo: need to do this
            // check format: "cin" ">>" expression ";"
            lexicalAnalyzer.nextToken();
            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple || !lexicalAnalyzer.getToken().getName().equals(">>")) {
                errorList += INVALID_STATEMENT + "'cin' statement missing extraction operator." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            lexicalAnalyzer.nextToken();
            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple || !expression()) {
                errorList += "'cin' statement requires a valid expression." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple || !lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.EOT.name())) {
                errorList += INVALID_STATEMENT + " 'cin' statement must end with a ';'." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            return true;

        } else {
            // check format: expression ";"
            if (!expression()) {
                return false;
            }

            return stackHandler.EOE();
        }
    }

    public boolean parameter(List<String> parameterNames) {
        if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
            return false;
        }

        // check format: type identifier ["[" "]"]

        if (lexicalAnalyzer.getToken() instanceof NullTuple || !type(lexicalAnalyzer.getToken().getType())) {
            errorList += "Parameter declarations must start with a valid type." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
            return false;
        }

        String type = lexicalAnalyzer.getToken().getName();

        lexicalAnalyzer.nextToken();
        if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
            return false;
        }

        if (lexicalAnalyzer.getToken() instanceof NullTuple || !lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name())) {
            errorList += "Parameter declarations require a valid identifier." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
            return false;
        }

        String name = lexicalAnalyzer.getToken().getName();

        lexicalAnalyzer.nextToken();
        if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
            return false;
        }

        if (lexicalAnalyzer.getToken() instanceof NullTuple || !lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.ARRAY_BEGIN.name())) {
            parameterNames.add("P" + variableId);
            symbolTable.put("P" + variableId, new Symbol(scope, "P" + variableId++, name, "param", new VariableData(type, "private"), ELEM_SIZE));
            return true;
        }

        lexicalAnalyzer.getToken();
        if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
            return false;
        }

        if (lexicalAnalyzer.getToken() instanceof NullTuple || !lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.ARRAY_END.name())) {
            errorList += "Invalid parameter declaration. Missing closing array bracket." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
            return false;
        }

        parameterNames.add("P" + variableId);
        symbolTable.put("P" + variableId, new Symbol(scope, "P" + variableId++, name, "param", new VariableData(type, "private"), ELEM_SIZE));
        return true;
    }

    public boolean parameter_list(List<String> parameters) {
        if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
            return false;
        }

        // check format: parameter { "," parameter}
        if (!parameter(parameters)) {
            errorList += "Invalid parameter_list" + LINE + lexicalAnalyzer.peekPreviousToken().getLineNum() + "\n";
            return false;
        }

        if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
            return false;
        }

        while (!(lexicalAnalyzer.getToken() instanceof NullTuple) && lexicalAnalyzer.getToken().getName().equals(",")) {
            lexicalAnalyzer.nextToken();

            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (!parameter(parameters)) {
                errorList += "Invalid parameter_list" + LINE + lexicalAnalyzer.peekPreviousToken().getLineNum() + "\n";
                return false;
            }
        }
        return true;
    }

    public boolean variable_declaration() {
        // check format: type identifier ["[" "]"] ["=" assignment_expression ] ";"
        stackHandler.typePush(new Type_SAR(lexicalAnalyzer.getToken(), scope));
        if (!stackHandler.typeExists()) {
            errorList += stackHandler.getErrorList();
            return false;
        }

        String type = lexicalAnalyzer.getToken().getName();

        lexicalAnalyzer.nextToken();
        stackHandler.variablePush(new Variable_SAR(lexicalAnalyzer.getToken(), scope, "V" + variableId++, type));

        lexicalAnalyzer.nextToken();
        if (lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.ARRAY_BEGIN.name())) {

            // todo: need to handle arrays
            lexicalAnalyzer.getToken();
            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple || !lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.ARRAY_END.name())) {
                errorList += "Invalid parameter declaration. Missing closing array bracket." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            lexicalAnalyzer.nextToken();
            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                errorList += "Invalid variable declaration. Missing semi-colon at end." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }
        }

        if (lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.ASSIGNMENT_OPR.name())) {
            stackHandler.operatorPush(new Opr_SAR(lexicalAnalyzer.getToken()));

            lexicalAnalyzer.nextToken();
            if (!assignment_expression()) {
                errorList += "Invalid variable declaration. Invalid assignment expression." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }
        }

        if (!stackHandler.EOE()) {
            return false;
        }

        lexicalAnalyzer.nextToken();
        return true;
    }

    public boolean method_body() {
        // check format: "{" {variable_declaration} {statement} "}"
        lexicalAnalyzer.nextToken();

        if (lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.BLOCK_END.name())) {
            lexicalAnalyzer.nextToken();
            return true;
        }

        String errorCheck = errorList;

        if (type(lexicalAnalyzer.getToken().getType())) {
            while (variable_declaration()) {
                if (!type(lexicalAnalyzer.getToken().getType())) break;
            }

            errorList += stackHandler.getErrorList();

            if (!errorCheck.equals(errorList)) {
                return false;
            }
        }

        if (lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.BLOCK_END.name())) {
            lexicalAnalyzer.nextToken();
            return true;
        }

        while (statement()) {
            lexicalAnalyzer.nextToken();
        }

        errorList += stackHandler.getErrorList();

        if (!errorCheck.equals(errorList)) {
            return false;
        }

        lexicalAnalyzer.nextToken();
        return true;
    }

    public boolean constructor_declaration() {
        if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
            return false;
        }

        // check format: class_name "(" [parameter_list] ")" method_body
        if (lexicalAnalyzer.getToken() instanceof NullTuple || !lexicalAnalyzer.getToken().getType().equals(KeyConst.CLASS_NAME.getKey())) {
            errorList += "Invalid class name in constructor declaration." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
            return false;
        }

        String constructorName = lexicalAnalyzer.getToken().getName();

        lexicalAnalyzer.nextToken();
        if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
            return false;
        }

        if (lexicalAnalyzer.getToken() instanceof NullTuple || !lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name())) {
            errorList += MISSING_OPENING_PARENTHESIS + " for constructor declaration. 'class " + constructorName + "'" + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
            return false;
        }

        lexicalAnalyzer.nextToken();
        if (lexicalAnalyzer.getToken() instanceof NullTuple) {
            errorList += MISSING_CLOSING_PARENTHESIS + " for constructor declaration. 'class " + constructorName + "'\n";
            return false;
        }

        if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
            return false;
        }

        if (lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
            lexicalAnalyzer.nextToken();
            symbolTable.put("M" + variableId, new Symbol(scope, "M" + variableId++, constructorName, "method", new MethodData("public", null, constructorName), ELEM_SIZE));
            incrementScope(constructorName, false);

            if (!method_body()) {
                return false;
            }

            decrementScope();
            return true;
        }

        String key = "M" + variableId;
        symbolTable.put(key, new Symbol(scope, "M" + variableId++, constructorName, "method", new MethodData("public", null, constructorName), ELEM_SIZE));
        incrementScope(constructorName, false);
        List<String> parameterNames = new ArrayList<String>();

        if (!parameter_list(parameterNames)) {
            errorList += "Invalid parameter list for constructor declaration. 'class " + constructorName + "'" + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
            return false;
        }

        ((MethodData)symbolTable.get(key).getData()).setParameters(parameterNames);

        if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
            return false;
        }

        if (lexicalAnalyzer.getToken() instanceof NullTuple || !lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
            errorList += MISSING_CLOSING_PARENTHESIS + " for constructor declaration. 'class " + constructorName + "'" + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
            return false;
        }

        lexicalAnalyzer.nextToken();
        if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
            return false;
        }

        if (!method_body()) {
            return false;
        }

        decrementScope();
        return true;
    }

    public boolean field_declaration(String accessMod, String type, String value) {
        if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
            return false;
        }

        if (lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name())) {
            // check format: "(" [parameter_list] ")" method_body

            lexicalAnalyzer.nextToken();
            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                errorList += "Invalid field declaration. NullTuple exception. " + MISSING_CLOSING_PARENTHESIS + "\n";
                return false;
            }

            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                symbolTable.put("M" + variableId, new Symbol(scope, "M" + variableId++, value, "method", new MethodData(accessMod, null, type), ELEM_SIZE));
                incrementScope(value, false);
                lexicalAnalyzer.nextToken();
                if (!method_body()) {
                    return false;
                }

                decrementScope();
                return true;
            }

            String methodKey = "M" + variableId;
            symbolTable.put(methodKey, new Symbol(scope, "M" + variableId++, value, "method", new MethodData(accessMod, null, type), ELEM_SIZE));
            incrementScope(value, false);
            List<String> parameters = new ArrayList<String>();

            if (!parameter_list(parameters)) {
                errorList += "Invalid parameter list in field declaration." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            ((MethodData)symbolTable.get(methodKey).getData()).setParameters(parameters);

            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple || !lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                errorList += "Invalid field declaration." + MISSING_CLOSING_PARENTHESIS + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            lexicalAnalyzer.nextToken();

            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (!method_body()) {
                return false;
            }

            decrementScope();
            return true;

        } else {
            boolean symbolAdded = false;

            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            // check format: ["[" "]"] ["=" assignment_expression ] ";"
            if (lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.EOT.name())) {
                symbolTable.put("V" + variableId, new Symbol(scope, "V" + variableId++, value, "ivar", new VariableData(type, accessMod), ELEM_SIZE));
                lexicalAnalyzer.nextToken();
                return true;
            }

            if (lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.ARRAY_BEGIN.name())) {

                lexicalAnalyzer.nextToken();
                if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                    return false;
                }

                if (lexicalAnalyzer.getToken() instanceof NullTuple || !lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.ARRAY_END.name())) {
                    errorList += "Invalid field declaration. Missing closing array bracket." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                    return false;
                }

                lexicalAnalyzer.nextToken();
                if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                    errorList += "Invalid field declaration. Missing semi-colon at end." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                    return false;
                }

                if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                    return false;
                }

                symbolTable.put("@" + variableId, new Symbol(scope, "@" + variableId++, value, "ivar", new VariableData(type, accessMod), ELEM_SIZE));
                symbolAdded = true;
                if (lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.EOT.name())) {
                    lexicalAnalyzer.nextToken();
                    return true;
                }
            }

            if (!symbolAdded) {
                symbolTable.put("V" + variableId, new Symbol(scope, "V" + variableId++, value, "ivar", new VariableData(type, accessMod), ELEM_SIZE));
            }

            if (lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.ASSIGNMENT_OPR.name())) {

                lexicalAnalyzer.nextToken();
                if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                    return false;
                }

                if (lexicalAnalyzer.getToken() instanceof NullTuple || !assignment_expression()) {
                    errorList += "Invalid field declaration. Invalid assignment expression." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                    return false;
                }

                if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                    errorList += "Invalid field declaration. Missing semi-colon at end." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                    return false;
                }

                if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                    return false;
                }
            }

            if (!lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.EOT.name())) {
                errorList += "Invalid field declaration. Missing semi-colon at end." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            lexicalAnalyzer.nextToken();
            return true;
        }
    }

    public boolean class_member_declaration() {
        if (lexicalAnalyzer.getToken().getType().equals(KeyConst.MODIFIER.getKey())) {
            // check format: modifier type identifier field_declaration

            String modifier = lexicalAnalyzer.getToken().getName();

            lexicalAnalyzer.nextToken();
            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple || !type(lexicalAnalyzer.getToken().getType())) {
                errorList += "Invalid class member declaration. Missing a valid type." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }
            String type = lexicalAnalyzer.getToken().getName();

            lexicalAnalyzer.nextToken();
            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple || !lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name())) {
                errorList += "Invalid class member declaration. Missing a valid identifier." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }
            String name = lexicalAnalyzer.getToken().getName();

            lexicalAnalyzer.nextToken();
            if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple || !field_declaration(modifier, type, name)) {
                errorList += "Invalid class member declaration. Invalid field declaration." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            return true;

        } else if (lexicalAnalyzer.getToken().getType().equals(KeyConst.CLASS_NAME.getKey())) {
            // check format: constructor_declaration
            if (!constructor_declaration()) {
                errorList += "Invalid class member declaration. Invalid constructor declaration." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            return true;
        }

        return false;
    }

    public boolean class_declaration() {
        // check format: "class" class_name "{" {class_member_declaration} "}"
        // class declaration should be in the symbol table at this point

        // look for "class"
        lexicalAnalyzer.nextToken();
        // validate class name

        incrementScope(lexicalAnalyzer.getToken().getName(), true);

        lexicalAnalyzer.nextToken();
        // check for block begin

        lexicalAnalyzer.nextToken();

        while (class_member_declaration()) {
            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                errorList += "Method body must end with a closing block '}'." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }
        }

        if (lexicalAnalyzer.getToken() instanceof NullTuple || !lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.BLOCK_END.name())) {
            errorList += "Invalid class declaration. Missing closing block." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
            return false;
        }

        decrementScope();

        if (isUnknownSymbol(lexicalAnalyzer.getToken().getType())) {
            return false;
        }

        lexicalAnalyzer.nextToken();
        return true;
    }

    public boolean compilation_unit() {
        // check format: {class_declaration} "void" "main" "(" ")" method_body
        if (!lexicalAnalyzer.getToken().getName().equals(KeyConst.VOID.getKey())) {
            if (!class_declaration()) {
                errorList += "Invalid compilation unit. Invalid class declaration." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            while (lexicalAnalyzer.getToken().getName().equals(KeyConst.CLASS.getKey())) {
                if (!class_declaration()) {
                    errorList += "Invalid compilation unit. Invalid class declaration." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                    return false;
                }
            }

            if (!lexicalAnalyzer.getToken().getName().equals(KeyConst.VOID.getKey())) {
                errorList += "Invalid compilation unit. Missing 'main' method.\n";
                return false;
            }
        }

        lexicalAnalyzer.nextToken();
        // look for main method

        lexicalAnalyzer.nextToken();
        // look for open parenthesis

        lexicalAnalyzer.nextToken();
        // look for closing parenthesis

        lexicalAnalyzer.nextToken();
        incrementScope("main", true);

        // at this point we have declared classes and "void main()"
        if (!method_body()) {
            return false;
        }

        decrementScope();
        return true;
    }

    private boolean type(String itemType) {
        return (itemType.equals(KeyConst.INT.getKey()) || itemType.equals(KeyConst.CHAR.getKey()) || itemType.equals(KeyConst.BOOL.getKey()) || itemType.equals(KeyConst.VOID.getKey()) || itemType.equals(KeyConst.CLASS_NAME.getKey()));
    }

    private boolean isLogicalConnectiveExpression(String itemType) {
        return itemType.equals(LexicalAnalyzer.tokenTypesEnum.LOGICAL_OPR.name());
    }

    private boolean isBooleanExpression(String itemType) {
        return itemType.equals(LexicalAnalyzer.tokenTypesEnum.BOOLEAN_OPR.name());
    }

    private boolean isMathematicalExpression(String itemType) {
        return itemType.equals(LexicalAnalyzer.tokenTypesEnum.MATH_OPR.name());
    }

    private boolean isUnknownSymbol(String type) {
        if (type.equals(LexicalAnalyzer.tokenTypesEnum.UNKNOWN.name())) {
            errorList += "Unknown Symbol on" + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
            return true;
        }

        return false;
    }

    private void decrementScope() {
        int scopeDepth = 0;
        for (char c : scope.toCharArray()) {
            if (c == '.') scopeDepth++;
        }

        if (scopeDepth > 1) {
            scope = scope.substring(0, scope.lastIndexOf("."));
        } else {
            scope = scope.substring(0, scope.lastIndexOf(".") + 1);
        }
    }

    private void incrementScope(String name, boolean isClass) {
        if (isClass) {
            scope += name;
        } else {
            scope += "." + name;
        }
    }
}
