package project;

import java.util.HashMap;
import java.util.Stack;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 8/31/13
 * Time: 9:21 AM
 */
public class StackHandler {
    private HashMap<String, Symbol> symbolTable = new HashMap<String, Symbol>();
    private Stack<SAR> SAS = new Stack<SAR>();
    private Stack<SAR> OS = new Stack<SAR>();

    public StackHandler(HashMap<String, Symbol> symbolTable) {
        this.symbolTable = symbolTable;
    }

    public void identifierPush(Identifier_SAR identifier) {
        SAS.push(identifier);
    }

    public void literalPush(Literal_SAR identifier) {
        SAS.push(identifier);
    }

    public void typePush(Type_SAR identifier) {
        SAS.push(identifier);
    }

    public void variablePush(Variable_SAR identifier) {
        SAS.push(identifier);
    }

    public void BALPush(BAL_SAR identifier) {
        SAS.push(identifier);
    }

    public void operatorPush(SAR opr) {
        int precedence = getOperatorPrecedence(opr.getLexi().getName());

        if (OS.isEmpty()) {
            OS.push(opr);
            return;
        }

        int lastOprPrecedence = getOperatorPrecedence(OS.peek().getLexi().getName());

        if (lastOprPrecedence <= precedence) {
            OS.push(opr);
        } else {

            // todo: this needs to handle a number of more items


            SAS.push(OS.pop());
            OS.push(opr);
        }

    }

    private int getOperatorPrecedence(String oprSymbol) {
        if (oprSymbol.equals(".") || oprSymbol.equals("(") || oprSymbol.equals("[")) {
            return 15;
        } else if (oprSymbol.equals(")") || oprSymbol.equals("]")) {
            return 0;
        } else if (oprSymbol.equals("*") || oprSymbol.equals("/") || oprSymbol.equals("%")) {
            return 13;
        } else if (oprSymbol.equals("+") || oprSymbol.equals("-")) {
            return 11;
        } else if (oprSymbol.equals("<") || oprSymbol.equals(">") || oprSymbol.equals("<=") || oprSymbol.equals(">=")) {
            return 9;
        } else if (oprSymbol.equals("==") || oprSymbol.equals("!=")) {
            return 7;
        } else if (oprSymbol.equals("&&")) {
            return 5;
        } else if (oprSymbol.equals("||")) {
            return 3;
        } else if (oprSymbol.equals("=")) {
            return 1;
        }
        return 0;
    }
}
