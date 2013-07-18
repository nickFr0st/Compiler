package project;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 4/27/13
 * Time: 8:40 AM
 */
public class Compiler {
    private static final String ILLEGAL_EXPRESSION = "Illegal expression.";
    private static final String ILLEGAL_NEW_DECLARATION = "Illegal new_declaration";
    private static final String ILLEGAL_NEW_OPERATION = "Illegal new operation.";
    private static final String ILLEGAL_ATOI_OPERATION = "Illegal atoi operation.";
    private static final String ILLEGAL_ITOA_OPERATION = "Illegal itoa operation";

    private static final String INVALID_ARGUMENT_LIST = "Invalid argument list.";
    private static final String INVALID_FUNCTION = "Invalid function.";
    private static final String MISSING_ARRAY_CLOSE = "Array element missing array close.";

    private static final String MISSING_CLOSING_PARENTHESIS = "Missing closing parenthesis.";
    private static final String MISSING_OPENING_PARENTHESIS = "Missing opening parenthesis.";
    private static final String LINE = " Line ";

    private static final String OPERATION = " operation.";
    private static final String ARGUMENT_LIST = " argument_list.";
    private static final String EXPRESSION = " expression.";

    private LexicalAnalyzer lexicalAnalyzer;
    private String errorList = "";

    public Compiler(LexicalAnalyzer lexicalAnalyzer) {
        this.lexicalAnalyzer = lexicalAnalyzer;
    }

    public void evaluate() {
        // pass one
        while (lexicalAnalyzer.hasNext()) {

            if (expression()) {
                if (lexicalAnalyzer.getToken() instanceof NullTuple)
                    break;

                continue;
            }
            lexicalAnalyzer.nextToken();
        }

        System.out.print(errorList);
    }

    private boolean new_declaration() {
        if (lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name())) {

            // check format: "(" [argument_list] ")"
            lexicalAnalyzer.nextToken();
            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                errorList += ILLEGAL_NEW_DECLARATION + OPERATION + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
                return false;
            }

            if (lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                lexicalAnalyzer.nextToken();
                return true;
            }

            if (!argument_list()) {
                errorList += ILLEGAL_NEW_DECLARATION + ARGUMENT_LIST + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
                return false;
            }

            lexicalAnalyzer.nextToken();
            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                errorList += ILLEGAL_NEW_DECLARATION + OPERATION + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
                return false;
            }

            if (!lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                errorList += ILLEGAL_NEW_DECLARATION + OPERATION + " " + MISSING_CLOSING_PARENTHESIS + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
                return false;
            }
            return true;
        }
        if (lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.ARRAY_BEGIN.name())) {

            // check format: "[" expression "]"
            lexicalAnalyzer.nextToken();
            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                errorList += ILLEGAL_NEW_DECLARATION + OPERATION + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
                return false;
            }

            if (!expression()) {
                errorList += ILLEGAL_NEW_DECLARATION + EXPRESSION + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
                return false;
            }

            lexicalAnalyzer.nextToken();
            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                errorList += ILLEGAL_NEW_DECLARATION + OPERATION + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
                return false;
            }

            if (!lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.ARRAY_END.name())) {
                errorList += ILLEGAL_NEW_DECLARATION + OPERATION + ", " + MISSING_CLOSING_PARENTHESIS + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
                return false;
            }
            return true;
        }

        return false;
    }

    private boolean assignment_expression() {
        if (lexicalAnalyzer.getToken().getType().equals(KeyConst.NEW.getKey())) {

            // check format: "new" type new_declaration
            lexicalAnalyzer.nextToken();
            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                errorList += ILLEGAL_NEW_OPERATION + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
                return false;
            }

            if (!type(lexicalAnalyzer.getToken().getType())) {
                errorList += ILLEGAL_NEW_OPERATION + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
                return false;
            }

            lexicalAnalyzer.nextToken();
            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                errorList += ILLEGAL_NEW_OPERATION + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
                return false;
            }

            if (!new_declaration()) {
                errorList += ILLEGAL_NEW_OPERATION + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
                return false;
            }

            return true;
        }

        if (lexicalAnalyzer.getToken().getType().equals(KeyConst.ATOI.getKey())) {

            // check format: "atoi" "(" expression ")"
            lexicalAnalyzer.nextToken();
            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                errorList += ILLEGAL_ATOI_OPERATION + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
                return false;
            }

            if (!lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name())) {
                errorList += ILLEGAL_ATOI_OPERATION + MISSING_OPENING_PARENTHESIS + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
                return false;
            }

            lexicalAnalyzer.nextToken();
            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                errorList += ILLEGAL_ATOI_OPERATION + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
                return false;
            }

            if (!expression()) {
                errorList += ILLEGAL_ATOI_OPERATION + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
                return false;
            }

            lexicalAnalyzer.nextToken();
            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                errorList += ILLEGAL_ATOI_OPERATION + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
                return false;
            }

            if (!lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                errorList += ILLEGAL_ATOI_OPERATION + MISSING_CLOSING_PARENTHESIS + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
                return false;
            }

            return true;
        }

        if (lexicalAnalyzer.getToken().getType().equals(KeyConst.ITOA.getKey())) {

            // check format: "itoa" "(" expression ")"
            lexicalAnalyzer.nextToken();
            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                errorList += ILLEGAL_ITOA_OPERATION + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
                return false;
            }

            if (!lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name())) {
                errorList += ILLEGAL_ITOA_OPERATION + MISSING_OPENING_PARENTHESIS + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
                return false;
            }

            lexicalAnalyzer.nextToken();
            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                errorList += ILLEGAL_ITOA_OPERATION + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
                return false;
            }

            if (!expression()) {
                errorList += ILLEGAL_ITOA_OPERATION + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
                return false;
            }

            lexicalAnalyzer.nextToken();
            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                errorList += ILLEGAL_ITOA_OPERATION + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
                return false;
            }

            if (!lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                errorList += ILLEGAL_ITOA_OPERATION + MISSING_CLOSING_PARENTHESIS + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
                return false;
            }

            return true;
        }

        return expression() || lexicalAnalyzer.getToken().getType().equals(KeyConst.THIS.getKey());
    }

    public boolean expressionz() {
        Tuple token = lexicalAnalyzer.getToken();
        if (token.getType().equals(LexicalAnalyzer.tokenTypesEnum.ASSIGNMENT_OPR.name()) || token.getType().equals(LexicalAnalyzer.tokenTypesEnum.LOGICAL_OPR.name()) || token.getType().equals(LexicalAnalyzer.tokenTypesEnum.BOOLEAN_OPR.name()) || token.getType().equals(LexicalAnalyzer.tokenTypesEnum.MATH_OPR.name())) {

            lexicalAnalyzer.nextToken();
            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                errorList += "expressionz missing right hand expression." + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
                return false;
            }

            if (token.getType().equals(LexicalAnalyzer.tokenTypesEnum.ASSIGNMENT_OPR.name())) {
                if (!assignment_expression()) {
                    errorList += "illegal assignment operation." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                    return false;
                }
            } else {
                if (!expression()) {
                    errorList += "illegal operation." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    public boolean expression() {
        if (lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name())) {

            // check format: "(" expression ")" [expressionz]
            lexicalAnalyzer.nextToken();
            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                errorList += ILLEGAL_EXPRESSION + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
                return false;
            }

            if (!expression()) {
                errorList += ILLEGAL_EXPRESSION + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                errorList += ILLEGAL_EXPRESSION + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
                return false;
            }

            if (!lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                errorList += ILLEGAL_EXPRESSION + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }

            lexicalAnalyzer.nextToken();
            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                return true;
            }

            expressionz();
            return true;

        } else if (lexicalAnalyzer.getToken().getType().equals(KeyConst.TRUE.getKey()) || lexicalAnalyzer.getToken().getType().equals(KeyConst.FALSE.getKey()) || lexicalAnalyzer.getToken().getType().equals(KeyConst.NULL.getKey()) || lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.NUMBER.name()) || lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.CHARACTER.name())) {

            // check format: "value" [expressionz]
            lexicalAnalyzer.nextToken();
            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                return true;
            }

            expressionz();
            return true;

        } else if (lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name())) {

            // check format: identifier [ fn_arr_member ] [ member_refz ] [ expressionz ]
            lexicalAnalyzer.nextToken();
            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                return true;
            }
            String errCheck = errorList;

            if (fn_arr_member()) {
                return true;
            }

            if (!errCheck.equals(errorList)) {
                return false;
            }

            if (member_refz()) {
                return true;
            }

            if (!errCheck.equals(errorList)) {
                return false;
            }

            if (expressionz()) {
                return true;
            }

            if (!errCheck.equals(errorList)) {
                return false;
            }

            return true;
        }
        return false;
    }

    private boolean argument_list() {

        // check format: expression { "," expression}
        if (!expression()) {
            errorList += INVALID_ARGUMENT_LIST + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
            return false;
        }

        if (lexicalAnalyzer.getToken() instanceof NullTuple) {
            return true;
        }

        while (lexicalAnalyzer.getToken().getLexi().equals(",")) {
            if (!expression()) {
                errorList += INVALID_ARGUMENT_LIST + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
                return false;
            }

            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                break;
            }
        }
        return true;
    }

    private boolean fn_arr_member() {

        // check format: "(" [ argument_list ] ")" | "[" expression "]"
        if (lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name())) {

            //check format: "(" [ argument_list ] ")"
            lexicalAnalyzer.nextToken();
            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                errorList += INVALID_FUNCTION + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
                return false;
            }

            if (lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name())) {
                lexicalAnalyzer.nextToken();
                return true;
            }

            if (!argument_list()) {
                errorList +=  INVALID_ARGUMENT_LIST + " in function." + LINE + lexicalAnalyzer.getToken().getLineNum() + "\n";
                return false;
            }


            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                errorList += INVALID_FUNCTION + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
                return false;
            }

            if (!lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                errorList += INVALID_FUNCTION + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
                return false;
            }

            lexicalAnalyzer.nextToken();
            return true;

        } else if (lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.ARRAY_BEGIN.name())) {

            //check format: "[" expression "]"
            lexicalAnalyzer.nextToken();
            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                errorList += "array element missing expression." + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
                return false;
            }

            if (!expression()) {
                errorList += "invalid array expression." + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
                return false;
            }

            lexicalAnalyzer.nextToken();
            if (lexicalAnalyzer.getToken() instanceof NullTuple) {
                errorList += MISSING_ARRAY_CLOSE + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
                return false;
            }

            if (!lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.ARRAY_END.name())) {
                errorList += MISSING_ARRAY_CLOSE + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
                return false;
            }
            return true;
        }
        return false;
    }

    private boolean member_refz() {

        // check format: "." identifier [ fn_arr_member ] [ member_refz ]
        if (!lexicalAnalyzer.getToken().getLexi().equals(".")) {
            return false;
        }

        lexicalAnalyzer.nextToken();
        if (lexicalAnalyzer.getToken() instanceof NullTuple || !lexicalAnalyzer.getToken().getType().equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name())) {
            errorList += "invalid member ref." + LINE + lexicalAnalyzer.previousToken().getLineNum() + "\n";
            return false;
        }

        lexicalAnalyzer.nextToken();
        if (lexicalAnalyzer.getToken() instanceof NullTuple) {
            return true;
        }

        String errCheck = errorList;

        if (fn_arr_member()) {
            return true;
        }

        if (!errCheck.equals(errorList)) {
            return false;
        }

        if (member_refz()) {
            return true;
        }

        if (!errCheck.equals(errorList)) {
            return false;
        }

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
}