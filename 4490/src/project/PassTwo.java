package project;

import java.util.Map;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 8/31/13
 * Time: 10:30 AM
 */
public class PassTwo {
    private static final String LINE = " Line: ";

    private StackHandler stackHandler;
    private Map<String, Symbol> symbolTable;
    private LexicalAnalyzer lexicalAnalyzer;
    private String errorList;

    public PassTwo(StackHandler stackHandler, Map<String, Symbol> symbolTable, LexicalAnalyzer lexicalAnalyzer) {
        this.stackHandler = stackHandler;
        this.symbolTable = symbolTable;
        this.lexicalAnalyzer = lexicalAnalyzer;
    }

    public void evaluate() {
//        if (!compiliation_unit()) {
//            System.out.print(errorList);
//            System.exit(0);
//        }
        System.out.println("Semantic Analysis Successful!");
        lexicalAnalyzer.resetList();

    }

    public boolean expressionz() {
        Tuple token = lexicalAnalyzer.getToken();
        if (!(token.getType().equals(LexicalAnalyzer.tokenTypesEnum.ASSIGNMENT_OPR.name()) || isLogicalConnectiveExpression(token.getType()) || isBooleanExpression(token.getType()) || isMathematicalExpression(token.getType()))) {
            return false;
        }

        stackHandler.operatorPush(new Opr_SAR(lexicalAnalyzer.getToken()));

        lexicalAnalyzer.nextToken();
        if (token.getType().equals(LexicalAnalyzer.tokenTypesEnum.ASSIGNMENT_OPR.name())) {
            if (!assignment_expression()) {
                errorList += "Invalid assignment expression." + LINE + lexicalAnalyzer.peekPreviousToken().getLineNum() + "\n";
                return false;
            }
        } else {
//            if (!expression()) {
//                errorList += "Invalid expressionz expression." + LINE + lexicalAnalyzer.peekPreviousToken().getLineNum() + "\n";
//                return false;
//            }
        }

        return true;
    }

    public boolean assignment_expression() {
        if (lexicalAnalyzer.getToken().getType().equals(KeyConst.NEW.getKey())) {

            // check format: "new" type new_declaration
            lexicalAnalyzer.nextToken();

            if (!type(lexicalAnalyzer.getToken().getType())) {
                return false;
            }

            lexicalAnalyzer.nextToken();

//            if (!new_declaration()) {
//                return false;
//            }

            return true;
        } else if (lexicalAnalyzer.getToken().getType().equals(KeyConst.ATOI.getKey())) {

            // check format: "atoi" "(" expression ")"
            lexicalAnalyzer.nextToken();
            stackHandler.operatorPush(new Opr_SAR(lexicalAnalyzer.getToken()));

            lexicalAnalyzer.nextToken();
//            if (!expression()) {
//                return false;
//            }

            stackHandler.operatorPush(new Opr_SAR(lexicalAnalyzer.getToken()));

            // todo: #atoi

            lexicalAnalyzer.nextToken();
            return true;
        } else if (lexicalAnalyzer.getToken().getType().equals(KeyConst.ITOA.getKey())) {

            // check format: "itoa" "(" expression ")"
            lexicalAnalyzer.nextToken();
            stackHandler.operatorPush(new Opr_SAR(lexicalAnalyzer.getToken()));

            lexicalAnalyzer.nextToken();
//            if (!expression()) {
//                return false;
//            }

            stackHandler.operatorPush(new Opr_SAR(lexicalAnalyzer.getToken()));

            // todo: #itoa

            lexicalAnalyzer.nextToken();
            return true;
        } else if (lexicalAnalyzer.getToken().getType().equals(KeyConst.THIS.getKey())) {
            lexicalAnalyzer.nextToken();
            return true;
        } else {
//            if (!expression()) {
//                return false;
//            }
            return true;
        }
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
