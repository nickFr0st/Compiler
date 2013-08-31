package project;

import java.util.HashMap;
import java.util.Map;
import java.util.Stack;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 8/31/13
 * Time: 9:21 AM
 */
public class StackHandler {
    private Map<String, Symbol> symbolTable = new HashMap<String, Symbol>();
    private Stack<SAR> SAS = new Stack<SAR>();
    private Stack<Opr_SAR> OS = new Stack<Opr_SAR>();

    public StackHandler(Map<String, Symbol> symbolTable) {
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

    public void operatorPush(Opr_SAR opr) {
        opr.setPrecedence(getOperatorPrecedence(opr.getLexi().getName()));

        if (OS.isEmpty()) {
            OS.push(opr);
            return;
        }

        int lastOprPrecedence = OS.peek().getPrecedence();

        if (lastOprPrecedence <= opr.getPrecedence()) {
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
