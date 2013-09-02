package project;

import java.util.LinkedHashMap;
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

    public String getErrorList() {
        return errorList;
    }

    public boolean EOE() {
        while(!OS.isEmpty()) {
            if (!handleOperation()) {
                return false;
            }
        }

        return true;
    }

    public void identifierPush(Identifier_SAR identifier) {
        SAS.push(identifier);
    }

    public boolean identifierExist() {
        return isInSymbolTable((Identifier_SAR)SAS.pop());
    }

    public void literalPush(Literal_SAR literal) {
        SAS.push(literal);
    }

    public void typePush(Type_SAR type) {
        SAS.push(type);
    }

    public boolean typeExists() {
        Type_SAR itemType = (Type_SAR)SAS.pop();

        if (type(itemType.getLexi().getName())) {
            return true;
        } else if (itemType.getLexi().getType().equals(KeyConst.CLASS_NAME.getKey())) {
            return isClassInSymbolTable(itemType);
        }

        return false;
    }

    private boolean isClassInSymbolTable(Type_SAR itemType) {
        for (String key : symbolTable.keySet()) {
            Symbol temp = symbolTable.get(key);

            if (!temp.getScope().equals("g.")) {
                continue;
            }

            if (temp.getValue().equals(itemType.getName())) {
                return true;
            }
        }

        errorList += "type: '" + itemType.getName() + "' does not exists. Line: " + itemType.getLexi().getLineNum() + "\n";
        return false;
    }

    private boolean isInSymbolTable(SAR sar) {
        String searchScope = sar.getScope();
        while (!searchScope.equals("g.")) {
            for (String key : symbolTable.keySet()) {
                Symbol temp = symbolTable.get(key);

                if (sar.getScope().contains(temp.getScope()) && temp.getValue().equals(sar.getLexi().getName())) {
                    return true;
                }
            }
            searchScope = decrementScope(searchScope);
        }

        errorList += "symbol: '" + sar.getLexi().getName() + "' does not exists. Line: " + sar.getLexi().getLineNum() + "\n";
        return false;
    }

    public void variablePush(Variable_SAR variable) {
        SAS.push(variable);
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
        } else if (topOpr.getLexi().getName().equals("=")) {
            return assignment();
        }

        return false;
    }

    private boolean assignment() {
        SAR rhs = SAS.pop();
        SAR lhs = SAS.pop();

        if (lhs instanceof Literal_SAR) {
            errorList += "left hand side is not a valid assignable type. Line. " + lhs.getLexi().getLineNum() + "\n";
            return false;
        }

        if (!lhs.getType().toUpperCase().equals(rhs.getType().toUpperCase())) {
            errorList += "left and right hand sides of assignment operation are incompatible types. Line: " + lhs.getLexi().getLineNum() + "\n";
            return false;
        }

        return true;
    }

    private boolean add() {
        SAR rhs = SAS.pop();
        SAR lhs = SAS.pop();

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
        SAR rhs = SAS.pop();
        SAR lhs = SAS.pop();

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
        SAR rhs = SAS.pop();
        SAR lhs = SAS.pop();

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
        SAR rhs = SAS.pop();
        SAR lhs = SAS.pop();

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

    private boolean type(String itemType) {
        return (itemType.equals(KeyConst.INT.getKey()) || itemType.equals(KeyConst.CHAR.getKey()) || itemType.equals(KeyConst.BOOL.getKey()) || itemType.equals(KeyConst.VOID.getKey()));
    }

    private String decrementScope(String scope) {
        int scopeDepth = 0;
        for (char c : scope.toCharArray()) {
            if (c == '.') scopeDepth++;
        }

        if (scopeDepth > 1) {
            return scope.substring(0, scope.lastIndexOf("."));
        } else {
            return scope.substring(0, scope.lastIndexOf(".") + 1);
        }
    }
}
