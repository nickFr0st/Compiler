package project;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Stack;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 8/31/13
 * Time: 9:21 AM
 */
public class StackHandler {
    private LinkedHashMap<String, Symbol> symbolTable = new LinkedHashMap<String, Symbol>();
    private Stack<SAR> SAS = new Stack<SAR>();
    private Stack<Opr_SAR> OS = new Stack<Opr_SAR>();
    private String errorList;
    private int variableId;

    public StackHandler(LinkedHashMap<String, Symbol> symbolTable, String errorList, int variableId) {
        this.symbolTable = symbolTable;
        this.errorList = errorList;
        this.variableId = variableId;
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

    public boolean operatorPush(Opr_SAR opr) {
        opr.setPrecedence(getOperatorPrecedence(opr.getLexi().getName()));

        if (OS.isEmpty()) {
            OS.push(opr);
            return true;
        }

        int lastOprPrecedence = OS.peek().getPrecedence();

        if (lastOprPrecedence <= opr.getPrecedence()) {
            OS.push(opr);
        } else {
            if (!handleOperation()) {
                return false;
            }
            OS.push(opr);
        }
        return true;
    }

    private boolean handleOperation() {
        Opr_SAR topOpr = OS.pop();

        if (topOpr.getLexi().getName().equals("+")) {
            return add();
        } else if(topOpr.getLexi().getName().equals("-")) {
            return subtract();
        } else if (topOpr.getLexi().getName().equals("/")) {
            return divide();
        } else if (topOpr.getLexi().getName().equals("*")) {
            return multiply();
        }

        return false;
    }

    private boolean add() {
        SAR lhs = SAS.pop();
        SAR rhs = SAS.pop();

        if (!lhs.getType().equals(rhs.getType())) {
            errorList += "left and right hand sides of addition operation are incompatible types. Line: " + lhs.getLexi().getLineNum() + "\n";
            return false;
        }

        if (!lhs.getType().equals("int")) {
            errorList += "left hand side of addition operator must an int. Line: " + lhs.getLexi().getLineNum() + "\n";
            return false;
        }

        if (!rhs.getType().equals("int")) {
            errorList += "right hand side of addition operator must an int. Line: " + rhs.getLexi().getLineNum() + "\n";
            return false;
        }

        Tuple tempTuple = new Tuple("T" + variableId, lhs.getLexi().getType(), lhs.getLexi().getLineNum());
        SAR temp = new Variable_SAR(tempTuple, lhs.getScope(), "T" + variableId++, lhs.getType());
        SAS.push(temp);

        return true;
    }

    private boolean subtract() {
        SAR lhs = SAS.pop();
        SAR rhs = SAS.pop();

        if (!lhs.getType().equals(rhs.getType())) {
            errorList += "left and right hand sides of subtraction operation are incompatible types. Line: " + lhs.getLexi().getLineNum() + "\n";
            return false;
        }

        if (!lhs.getType().equals("int")) {
            errorList += "left hand side of subtraction operator must an int. Line: " + lhs.getLexi().getLineNum() + "\n";
            return false;
        }

        if (!rhs.getType().equals("int")) {
            errorList += "right hand side of subtraction operator must an int. Line: " + rhs.getLexi().getLineNum() + "\n";
            return false;
        }

        Tuple tempTuple = new Tuple("T" + variableId, lhs.getLexi().getType(), lhs.getLexi().getLineNum());
        SAR temp = new Variable_SAR(tempTuple, lhs.getScope(), "T" + variableId++, lhs.getType());
        SAS.push(temp);

        return true;
    }

    private boolean multiply() {
        SAR lhs = SAS.pop();
        SAR rhs = SAS.pop();

        if (!lhs.getType().equals(rhs.getType())) {
            errorList += "left and right hand sides of multiplication operation are incompatible types. Line: " + lhs.getLexi().getLineNum() + "\n";
            return false;
        }

        if (!lhs.getType().equals("int")) {
            errorList += "left hand side of multiplication operator must an int. Line: " + lhs.getLexi().getLineNum() + "\n";
            return false;
        }

        if (!rhs.getType().equals("int")) {
            errorList += "right hand side of multiplication operator must an int. Line: " + rhs.getLexi().getLineNum() + "\n";
            return false;
        }

        Tuple tempTuple = new Tuple("T" + variableId, lhs.getLexi().getType(), lhs.getLexi().getLineNum());
        SAR temp = new Variable_SAR(tempTuple, lhs.getScope(), "T" + variableId++, lhs.getType());
        SAS.push(temp);

        return true;
    }

    private boolean divide() {
        SAR lhs = SAS.pop();
        SAR rhs = SAS.pop();

        if (!lhs.getType().equals(rhs.getType())) {
            errorList += "left and right hand sides of division operation are incompatible types. Line: " + lhs.getLexi().getLineNum() + "\n";
            return false;
        }

        if (!lhs.getType().equals("int")) {
            errorList += "left hand side of division operator must an int. Line: " + lhs.getLexi().getLineNum() + "\n";
            return false;
        }

        if (!rhs.getType().equals("int")) {
            errorList += "right hand side of division operator must an int. Line: " + rhs.getLexi().getLineNum() + "\n";
            return false;
        }

        Tuple tempTuple = new Tuple("T" + variableId, lhs.getLexi().getType(), lhs.getLexi().getLineNum());
        SAR temp = new Variable_SAR(tempTuple, lhs.getScope(), "T" + variableId++, lhs.getType());
        SAS.push(temp);

        return true;
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
