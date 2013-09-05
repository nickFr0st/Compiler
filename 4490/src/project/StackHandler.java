package project;

import com.sun.org.apache.xerces.internal.parsers.AbstractSAXParser;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
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

    public SAR popSAS() {
        if (!SAS.isEmpty()) {
            return SAS.pop();
        }
        return null;
    }

    public String getErrorList() {
        return errorList;
    }

    public boolean EOE() {
        while (!OS.isEmpty()) {
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
        Identifier_SAR id_sar = (Identifier_SAR) SAS.pop();

        String type = isInSymbolTable(id_sar);
        if (type != null) {
            id_sar.setType(type);
            SAS.push(id_sar);
            return true;
        }

        return false;
    }

    public boolean memberRefExists() {
        SAR rhs = SAS.pop();
        SAR lhs = SAS.pop();

        if (lhs instanceof Literal_SAR) {
            errorList += "left hand side of a call operation cannot be a literal value. Line: " + lhs.getLexi().getLineNum() + "\n";
            return false;
        }

        if (rhs instanceof Literal_SAR) {
            errorList += "right hand side of a call operation cannot be a literal value. Line: " + lhs.getLexi().getLineNum() + "\n";
            return false;
        }

        if (type(lhs.getType())) {
            errorList += "variable: '" + lhs.getLexi().getName() + "' is of type '" + lhs.getType() + "' which is un-assignable. Line: " + lhs.getLexi().getLineNum() + "\n";
            return false;
        }

        String lhsItemScope = "g." + lhs.getType();

        for (String key : symbolTable.keySet()) {
            Symbol temp = symbolTable.get(key);

            if (!temp.getScope().equals(lhsItemScope)) {
                continue;
            }

            if (temp.getValue().equals(rhs.getLexi().getName())) {
                if (temp.getData().getAccessMod().toUpperCase().equals(KeyConst.PRIVATE.name())) {
                    errorList += "'" + rhs.getLexi().getName() + "' must be a public variable in order to be accessed outside of its class. Line: " + rhs.getLexi().getLineNum() + "\n";
                    return false;
                }

                if (temp.getData() instanceof MethodData) {
                    ((Function_SAR)rhs).getFunction().setType(temp.getData().getType());

                    if (((Function_SAR)rhs).getArguments().getArguments().size() > ((MethodData) temp.getData()).getParameters().size()) {
                        errorList += "there are too many parameters for called method. Line: " + rhs.getLexi().getLineNum() + "\n";
                        return false;
                    } else if (((Function_SAR)rhs).getArguments().getArguments().size() < ((MethodData) temp.getData()).getParameters().size()) {
                        errorList += "there are too few parameters for called method. Line: " + rhs.getLexi().getLineNum() + "\n";
                        return false;
                    }

                    List<SAR> args = ((Function_SAR)rhs).getArguments().getArguments();
                    int index = ((Function_SAR)rhs).getArguments().getArguments().size() - 1;

                    for (String type : ((MethodData) temp.getData()).getParameters()) {
                        if (!args.get(index).getType().equals(type)) {
                            errorList += "invalid argument type. expected: " + type + " but was: " + args.get(index).getType() + " Line: " + rhs.getLexi().getLineNum() + "\n";
                            return false;
                        }
                        index--;
                    }
                }

                SAS.push(new Ref_SAR(lhs.getScope(), new Tuple("T" + variableId++, temp.getData().getType(), rhs.getLexi().getLineNum()), temp.getData().getType()));
                return true;
            }
        }

        errorList += "'" + rhs.getLexi().getName() + "' does not exists in '" + lhs.getLexi().getName() + "'. Line: " + rhs.getLexi().getLineNum() + "\n";
        return false;
    }

    public void literalPush(Literal_SAR literal) {
        SAS.push(literal);
    }

    public void typePush(Type_SAR type) {
        SAS.push(type);
    }

    public boolean typeExists() {
        Type_SAR itemType = (Type_SAR) SAS.peek();

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

    private String isInSymbolTable(SAR sar) {
        String searchScope = sar.getScope();
        while (!searchScope.equals("g.")) {
            for (String key : symbolTable.keySet()) {
                Symbol temp = symbolTable.get(key);

                if (sar.getScope().contains(temp.getScope()) && temp.getValue().equals(sar.getLexi().getName())) {
                    return temp.getData().getType();
                }
            }
            searchScope = decrementScope(searchScope);
        }

        errorList += "symbol: '" + sar.getLexi().getName() + "' does not exists. Line: " + sar.getLexi().getLineNum() + "\n";
        return null;
    }

    public void variablePush(Variable_SAR variable) {
        SAS.push(variable);
    }

    public void BALPush(BAL_SAR identifier) {
        SAS.push(identifier);
    }

    public void EALPush() {
        List<SAR> arguments = new ArrayList<SAR>();
        while(!(SAS.peek() instanceof BAL_SAR)) {
            arguments.add(SAS.pop());
        }

        // remove BAL_SAR
        SAS.pop();

        SAS.push(new EAL_SAR(arguments));
    }

    public void functionPush() {
        EAL_SAR arguments = (EAL_SAR)SAS.pop();
        Identifier_SAR function = (Identifier_SAR)SAS.pop();
        SAS.push(new Function_SAR(function.getScope(), function.getLexi(), function.getType(), function, arguments));
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

    public boolean newObjPush() {
        EAL_SAR parameters = (EAL_SAR)SAS.pop();
        Type_SAR type = (Type_SAR)SAS.pop();

        String constructorScope = "g." + type.getName();

        for (String key : symbolTable.keySet()) {
            Symbol temp = symbolTable.get(key);

            if (!temp.getScope().equals(constructorScope)) {
                continue;
            }

            if (temp.getValue().equals(type.getName())) {
                if (temp.getData() instanceof MethodData) {
                    type.setType(temp.getData().getType());

                    if (parameters.getArguments().size() > ((MethodData) temp.getData()).getParameters().size()) {
                        errorList += "there are too many parameters for called constructor. Line: " + type.getLexi().getLineNum() + "\n";
                        return false;
                    } else if (parameters.getArguments().size() < ((MethodData) temp.getData()).getParameters().size()) {
                        errorList += "there are too few parameters for called constructor. Line: " + type.getLexi().getLineNum() + "\n";
                        return false;
                    }

                    List<SAR> args = parameters.getArguments();
                    int index = parameters.getArguments().size() - 1;

                    for (String params : ((MethodData) temp.getData()).getParameters()) {
                        if (!args.get(index).getType().equals(params)) {
                            errorList += "invalid argument type. expected: " + params + " but was: " + args.get(index).getType() + " Line: " + type.getLexi().getLineNum() + "\n";
                            return false;
                        }
                        index--;
                    }
                }
                SAS.push(new New_SAR(type.getScope(), type.getLexi(), type.getType(), type, parameters));
                return true;
            }
        }

        errorList += "Invalid Constructor. Line: " + type.getLexi().getLineNum() + "\n";
        return false;
    }

    private boolean handleOperation() {
        Opr_SAR topOpr = OS.pop();

        if (topOpr.getLexi().getName().equals("+")) {
            return add();
        } else if (topOpr.getLexi().getName().equals("-")) {
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
