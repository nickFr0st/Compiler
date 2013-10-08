package project;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Stack;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 4/7/13
 * Time: 5:30 PM
 */
public class TCode {
    private final static String SL = "R97";
    private final static String SB = "R98";
    private final static String FP = "R99";
    private final static String SP = "R100";

    private LinkedHashMap<String, Symbol> symbolTable = new LinkedHashMap<String, Symbol>();
    private List<ICode> iCodeList = new ArrayList<ICode>();
    private List<String> tCode = new ArrayList<String>();
    private LinkedHashMap<String, String> reg = new LinkedHashMap<String, String>();
    private String startLabel;
    private Stack<Integer> retAddressStack = new Stack<Integer>();
    private int address = 0;

    private void initReg() {
        for (int i = 0; i < 101; i++) {
            if (i > 96) {
                reg.put("R" + i, "0");
            } else {
                reg.put("R" + i, "");
            }
        }
    }

    public TCode(LinkedHashMap<String, Symbol> symbolTable, List<ICode> iCodeList, String startLabel) {
        this.symbolTable = symbolTable;
        this.iCodeList = iCodeList;
        this.startLabel = startLabel;
        initReg();
    }

    private String getRegister(String id) {
        for (String s : reg.keySet()) {
            if (reg.get(s).equals("")) {
                reg.put(s, id);
                return s;
            }
        }
        return null;
    }

    private void freeResource(String r) {
        reg.put(r, "");
        address++;
        tCode.add("LDR " + r + " CLR");
    }

    public void buildCode() {
        tCode.add("CLR .INT 0");

        for (String key : symbolTable.keySet()) {
            Symbol s = symbolTable.get(key);

            if (s.getSymId().startsWith("L") && Character.isDigit(s.getSymId().toCharArray()[1])) {
                if (s.getData() instanceof VariableData) {
                    if (s.getData().getType().equalsIgnoreCase("int") || s.getData().getType().equalsIgnoreCase(LexicalAnalyzer.tokenTypesEnum.NUMBER.name())) {
                        tCode.add(s.getSymId() + " .INT " + s.getValue());
                    } else {
                        if (s.getValue().equals("\'\\n\'"))
                            tCode.add(s.getSymId() + " .BYT " + "\'13\'");
                        else
                            tCode.add(s.getSymId() + " .BYT " + s.getValue());
                    }
                }
            }
        }

        for (ICode iCode : iCodeList) {
            if (iCode.getOperation().equals(ICodeOprConst.CREATE_OPR.getKey()) && !iCode.getLabel().startsWith("L")) {
                if (symbolTable.get(iCode.getLabel()).getData() instanceof VariableData) {
                    if ((symbolTable.get(iCode.getLabel()).getData()).getType().equalsIgnoreCase("int")) {
                        tCode.add(iCode.getLabel() + " " + iCode.getArg1() + " " + "0" + " " + iCode.getComment());
                    } else {
                        tCode.add(iCode.getLabel() + " " + iCode.getArg1() + " " + "\'0\'" + " " + iCode.getComment());
                    }
                } else if (symbolTable.get(iCode.getLabel()).getData() instanceof MethodData) {
                    tCode.add(iCode.getLabel() + " " + iCode.getArg1() + " " + "\'0\'" + iCode.getComment());
                }
            }
        }

        String alphabet = "abcdefghijklmnopqrstuvwxyz";
        loadAlphabet(alphabet);
        loadAlphabet(alphabet.toUpperCase());
        tCode.add("CR .BYT '13'");
        tCode.add("SP .BYT '32'");

        tCode.add("");
        tCode.add("LDR R0 CLR");
        address++;
        tCode.add("LDR R1 CLR");
        address++;
        retAddressStack.push(address);
        address++;

        tCode.add(ICodeOprConst.JMP_OPR.getKey() + " " + startLabel + " ; program start");
        address++;
        tCode.add("TRP 0 ; program end");
        address++;
        handleUnderFlow();

        for (ICode iCode : iCodeList) {
            if (iCode.getOperation().equals(ICodeOprConst.FUNC_OPR.getKey())) {

                String reg1 = getRegister(SP);
                if (iCode.getLabel().isEmpty()) {
                    tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg1 + " CLR");
                } else {
                    tCode.add(iCode.getLabel() + " " + TCodeOprConst.LDR_OPR.getKey() + " " + reg1 + " " + " CLR");
                }
                address++;
                freeResource(reg1);
            } else if (iCode.getOperation().equals(ICodeOprConst.ADD_OPR.getKey()) || iCode.getOperation().equals(ICodeOprConst.ADI_OPR.getKey())) {
                mathOpr(iCode, ICodeOprConst.ADD_OPR.getKey());
            } else if (iCode.getOperation().equals(ICodeOprConst.SUB_OPR.getKey())) {
                mathOpr(iCode, ICodeOprConst.SUB_OPR.getKey());
            } else if (iCode.getOperation().equals(ICodeOprConst.MUL_OPR.getKey())) {
                mathOpr(iCode, ICodeOprConst.MUL_OPR.getKey());
            } else if (iCode.getOperation().equals(ICodeOprConst.DIV_OPR.getKey())) {
                mathOpr(iCode, ICodeOprConst.DIV_OPR.getKey());
            } else if (iCode.getOperation().equals(ICodeOprConst.MOD_OPR.getKey())) {

                String argReg1 = getRegister(iCode.getArg1());
                String argReg2 = getRegister(iCode.getArg2());
                String argReg3 = getRegister(iCode.getResult());

                if (iCode.getLabel().equals("")) {
                    tCode.add("LDR " + argReg1 + " " + iCode.getArg1());
                } else {
                    tCode.add(iCode.getLabel() + " LDR " + argReg1 + " " + iCode.getArg1());
                }
                address++;
                tCode.add("LDR " + argReg2 + " " + iCode.getArg2());
                address++;
                tCode.add("LDR " + argReg3 + " " + iCode.getArg1());
                address++;

                tCode.add("DIV " + argReg3 + " " + argReg2);
                address++;
                tCode.add("MUL " + argReg3 + " " + argReg2);
                address++;
                tCode.add("SUB " + argReg1 + " " + argReg3);
                address++;
                tCode.add("STR " + argReg1 + " " + iCode.getResult() + " " + iCode.getComment());

                freeResource(argReg1);
                freeResource(argReg2);
                freeResource(argReg3);
            } else if (iCode.getOperation().equals(ICodeOprConst.MOV_OPR.getKey())) {

                String argReg1 = getRegister(iCode.getArg1());
                String argReg2 = getRegister(iCode.getArg2());

                if (iCode.getLabel().equals("")) {
                    tCode.add("LDR " + argReg1 + " " + iCode.getArg1());
                } else {
                    tCode.add(iCode.getLabel() + " LDR " + argReg1 + " " + iCode.getArg1());
                }
                address++;
                tCode.add("LDR " + argReg2 + " " + iCode.getArg2());
                address++;

                tCode.add("MOV " + argReg1 + " " + argReg2 + " " + iCode.getComment());
                address++;
                tCode.add("STR " + argReg1 + " " + iCode.getArg1());
                address++;
                freeResource(argReg1);
                freeResource(argReg2);
            } else if (iCode.getOperation().equals(ICodeOprConst.WRTI_OPR.getKey())) {

                String argReg1 = getRegister(iCode.getArg1());
                if (iCode.getLabel().equals("")) {
                    tCode.add("LDR " + argReg1 + " " + iCode.getArg1());
                } else {
                    tCode.add(iCode.getLabel() + " LDR " + argReg1 + " " + iCode.getArg1());
                }
                address++;
                tCode.add("TRP 1 " + iCode.getComment());
                address++;
                freeResource(argReg1);
            } else if (iCode.getOperation().equals(ICodeOprConst.WRTC_OPR.getKey())) {

                String argReg1 = getRegister(iCode.getArg1());
                if (iCode.getLabel().equals("")) {
                    tCode.add("LDR " + argReg1 + " " + iCode.getArg1());
                } else {
                    tCode.add(iCode.getLabel() + " LDR " + argReg1 + " " + iCode.getArg1());
                }
                address++;

                tCode.add("TRP 3 " + iCode.getComment());
                address++;
                freeResource(argReg1);
            } else if (iCode.getOperation().equals(ICodeOprConst.RTN_OPR.getKey())) {

                String argReg1 = getRegister(FP);
                if (retAddressStack.size() == 0) {
                    tCode.add("JMP " + TCodeOprConst.UNDERFLOW_LBL.getKey());
                    address++;
                } else {
                    Integer addr = retAddressStack.pop();
                    tCode.add("LDR " + argReg1 + " CLR");
                    address++;
                    tCode.add("ADI " + argReg1 + " " + addr.toString() + " ; rtn addr");
                    address++;
                    tCode.add("JMR " + argReg1 + " ; GOTO rtn addr");
                    address++;
                }

                freeResource(argReg1);
            } else if (iCode.getOperation().equals(TCodeOprConst.JMP_OPR.getKey())) {

                if (iCode.getLabel().equals("")) {
                    tCode.add(iCode.getOperation() + " " + iCode.getArg1() + " " + iCode.getComment());
                } else {
                    tCode.add(iCode.getLabel() + " " + iCode.getOperation() + " " + iCode.getArg1() + " " + iCode.getComment());
                }
                address++;
            } else if (iCode.getOperation().equals("RDI")) {

                if (iCode.getLabel().equals("")) {
                    tCode.add("TRP 2");
                } else {
                    tCode.add(iCode.getLabel() + " TRP 2");
                }
                address++;
                String argReg1 = getRegister(iCode.getArg1());

                tCode.add("LDR " + argReg1 + " INII");
                address++;
                tCode.add("STR " + argReg1 + " " + iCode.getArg1());
                address++;
                freeResource(argReg1);
            } else if (iCode.getOperation().equals("RDC")) {

                if (iCode.getLabel().equals("")) {
                    tCode.add("TRP 4");
                } else {
                    tCode.add(iCode.getLabel() + "TRP 4");
                }
                address++;
                String argReg1 = getRegister(iCode.getArg1());

                tCode.add("LDR " + argReg1 + " INPT");
                tCode.add("STR " + argReg1 + " " + iCode.getArg1());
                address++;
                freeResource(argReg1);
            }
//            else if (iCode.getOperation().equals("EQ")) {
//
//                String argReg1 = getRegister(iCode.getArg1());
//                String argReg2 = getRegister(iCode.getArg2());
//                String argReg3 = getRegister(iCode.getResult());
//
//                if (iCode.getLabel().equals("")) {
//                    if (useCondLabel) {
//                        tCode.set(tCode.size() - 1, "L" + condIncr++ + " LDR " + argReg1 + " " + iCode.getArg1());
//                    } else {
//                        tCode.add("LDR " + argReg1 + " " + iCode.getArg1());
//                    }
//                } else {
//                    tCode.add(iCode.getLabel() + " LDR " + argReg1 + " " + iCode.getArg1());
//                }
//
//                String L3 = "L" + condIncr++;
//                String L4 = "L" + condIncr;
//
//                tCode.add("LDR " + argReg2 + " " + iCode.getArg2());
//                tCode.add("LDR " + argReg3 + " " + iCode.getResult());
//
//                tCode.add("MOV " + argReg3 + " " + argReg1);
//                tCode.add("CMP " + argReg3 + " " + argReg2);
//                tCode.add("BRZ " + argReg3 + " " + L3 + " ; " + iCode.getResult() + " == " + iCode.getArg2() + " GOTO " + L3);
//                tCode.add("STR R0 " + iCode.getResult() + " ; Set FALSE");
//                tCode.add("JMP " + L4);
//                tCode.add(L3 + " STR R1 " + iCode.getResult() + " ; Set TRUE");
//
//                freeResource(argReg1);
//                freeResource(argReg2);
//                freeResource(argReg3);
//
//                useCondLabel = true;
//                tCode.add(L4);
//            }

//

//            if (iCode.getOperation().equals("MOVI")) {
//                String argReg1 = getRegister(iCode.getArg2());
//
//                String value = symbolTable.get(iCode.getArg2()).getValue();
//
//                if (iCode.getLabel().equals("")) {
//                    tCode.add("ADI " + argReg1 + " " + value.substring(1, value.length()));
//                } else {
//                    tCode.add(iCode.getLabel() + " ADI " + argReg1 + " " + value.substring(1, value.length()));
//                }
//
//                tCode.add("STR " + argReg1 + " " + iCode.getArg1() + " " + iCode.getComment());
//                freeResource(argReg1);
//                continue;
//            }

//

//

//            if (iCode.getOperation().equals("LT")) {
//                String argReg1 = getRegister(iCode.getArg1());
//                String argReg2 = getRegister(iCode.getArg2());
//                String argReg3 = getRegister(iCode.getResult());
//
//                if (iCode.getLabel().equals("")) {
//                    if (useCondLabel) {
//                        tCode.set(tCode.size() - 1, "L" + condIncr++ + " LDR " + argReg1 + " " + iCode.getArg1());
//                    } else {
//                        tCode.add("LDR " + argReg1 + " " + iCode.getArg1());
//                    }
//                } else {
//                    tCode.add(iCode.getLabel() + " LDR " + argReg1 + " " + iCode.getArg1());
//                }
//
//                String L3 = "L" + condIncr++;
//                String L4 = "L" + condIncr;
//
//                tCode.add("LDR " + argReg2 + " " + iCode.getArg2());
//                tCode.add("LDR " + argReg3 + " " + iCode.getResult());
//                tCode.add("MOV " + argReg3 + " " + argReg1);
//                tCode.add("CMP " + argReg3 + " " + argReg2);
//                tCode.add("BLT " + argReg3 + " " + L3 + " ; " + iCode.getResult() + " < " + iCode.getArg2() + " GOTO " + L3);
//                tCode.add("STR R0 " + iCode.getResult() + " ; Set FALSE");
//                tCode.add("JMP " + L4);
//                tCode.add(L3 + " STR R1 " + iCode.getResult() + " ; Set TRUE");
//
//                freeResource(argReg1);
//                freeResource(argReg2);
//                freeResource(argReg3);
//
//                useCondLabel = true;
//                tCode.add(L4);
//                continue;
//            }
//
//            if (iCode.getOperation().equals("GT")) {
//                String argReg1 = getRegister(iCode.getArg1());
//                String argReg2 = getRegister(iCode.getArg2());
//                String argReg3 = getRegister(iCode.getResult());
//
//
//                if (iCode.getLabel().equals("")) {
//                    if (useCondLabel) {
//                        tCode.set(tCode.size() - 1, "L" + condIncr++ + " LDR " + argReg1 + " " + iCode.getArg1());
//                    } else {
//                        tCode.add("LDR " + argReg1 + " " + iCode.getArg1());
//                    }
//                } else {
//                    tCode.add(iCode.getLabel() + " LDR " + argReg1 + " " + iCode.getArg1());
//                }
//
//                String L3 = "L" + condIncr++;
//                String L4 = "L" + condIncr;
//
//                tCode.add("LDR " + argReg2 + " " + iCode.getArg2());
//                tCode.add("LDR " + argReg3 + " " + iCode.getResult());
//
//                tCode.add("MOV " + argReg3 + " " + argReg1);
//                tCode.add("CMP " + argReg3 + " " + argReg2);
//                tCode.add("BGT " + argReg3 + " " + L3 + " ; " + iCode.getResult() + " > " + iCode.getArg2() + " GOTO " + L3);
//                tCode.add("STR R0 " + iCode.getResult() + " ; Set FALSE");
//                tCode.add("JMP " + L4);
//                tCode.add(L3 + " STR R1 " + iCode.getResult() + " ; Set TRUE");
//
//                freeResource(argReg1);
//                freeResource(argReg2);
//                freeResource(argReg3);
//
//                useCondLabel = true;
//                tCode.add(L4);
//                continue;
//            }
//
//            if (iCode.getOperation().equals("NE")) {
//                String argReg1 = getRegister(iCode.getArg1());
//                String argReg2 = getRegister(iCode.getArg2());
//                String argReg3 = getRegister(iCode.getResult());
//
//                if (iCode.getLabel().equals("")) {
//                    if (useCondLabel) {
//                        tCode.set(tCode.size() - 1, "L" + condIncr++ + " LDR " + argReg1 + " " + iCode.getArg1());
//                    } else {
//                        tCode.add("LDR " + argReg1 + " " + iCode.getArg1());
//                    }
//                } else {
//                    tCode.add(iCode.getLabel() + " LDR " + argReg1 + " " + iCode.getArg1());
//                }
//
//                String L3 = "L" + condIncr++;
//                String L4 = "L" + condIncr;
//
//                tCode.add("LDR " + argReg2 + " " + iCode.getArg2());
//                tCode.add("LDR " + argReg3 + " " + iCode.getResult());
//
//                tCode.add("MOV " + argReg3 + " " + argReg1);
//                tCode.add("CMP " + argReg3 + " " + argReg2);
//                tCode.add("BNZ " + argReg3 + " " + L3 + " ; " + iCode.getResult() + " != " + iCode.getArg2() + " GOTO " + L3);
//                tCode.add("STR R0 " + iCode.getResult() + " ; Set FALSE");
//                tCode.add("JMP " + L4);
//                tCode.add(L3 + " STR R1 " + iCode.getResult() + " ; Set TRUE");
//
//                freeResource(argReg1);
//                freeResource(argReg2);
//                freeResource(argReg3);
//
//                useCondLabel = true;
//                tCode.add(L4);
//                continue;
//            }
//
//            if (iCode.getOperation().equals("LE")) {
//                String argReg1 = getRegister(iCode.getArg1());
//                String argReg2 = getRegister(iCode.getArg2());
//                String argReg3 = getRegister(iCode.getResult());
//
//                if (iCode.getLabel().equals("")) {
//                    if (useCondLabel) {
//                        tCode.set(tCode.size() - 1, "L" + condIncr++ + " LDR " + argReg1 + " " + iCode.getArg1());
//                    } else {
//                        tCode.add("LDR " + argReg1 + " " + iCode.getArg1());
//                    }
//                } else {
//                    tCode.add(iCode.getLabel() + " LDR " + argReg1 + " " + iCode.getArg1());
//                }
//
//                String L3 = "L" + condIncr++;
//                String L4 = "L" + condIncr;
//
//                tCode.add("LDR " + argReg2 + " " + iCode.getArg2());
//                tCode.add("LDR " + argReg3 + " " + iCode.getResult());
//
//                tCode.add("MOV " + argReg3 + " " + argReg1 + " ; Test " + iCode.getArg1() + " < " + iCode.getArg2());
//                tCode.add("CMP " + argReg3 + " " + argReg2);
//                tCode.add("BLT " + argReg3 + " " + L3 + " ; " + iCode.getResult() + " < " + iCode.getArg2() + " GOTO " + L3);
//                tCode.add("MOV " + argReg3 + " " + argReg1 + " ; Test " + iCode.getArg1() + " == " + iCode.getArg2());
//                tCode.add("CMP " + argReg3 + " " + argReg2);
//                tCode.add("BRZ " + argReg3 + " " + L3 + " ; " + iCode.getResult() + " == " + iCode.getArg2() + " GOTO " + L3);
//                tCode.add("STR R0 " + iCode.getResult() + " ; Set FALSE");
//                tCode.add("JMP " + L4);
//                tCode.add(L3 + " STR R1 " + iCode.getResult() + " ; Set TRUE");
//
//                freeResource(argReg1);
//                freeResource(argReg2);
//                freeResource(argReg3);
//
//                useCondLabel = true;
//                tCode.add(L4);
//                continue;
//            }
//
//            if (iCode.getOperation().equals("GE")) {
//                String argReg1 = getRegister(iCode.getArg1());
//                String argReg2 = getRegister(iCode.getArg2());
//                String argReg3 = getRegister(iCode.getResult());
//
//                if (iCode.getLabel().equals("")) {
//                    if (useCondLabel) {
//                        tCode.set(tCode.size() - 1, "L" + condIncr++ + " LDR " + argReg1 + " " + iCode.getArg1());
//                    } else {
//                        tCode.add("LDR " + argReg1 + " " + iCode.getArg1());
//                    }
//                } else {
//                    tCode.add(iCode.getLabel() + " LDR " + argReg1 + " " + iCode.getArg1());
//                }
//
//                String L3 = "L" + condIncr++;
//                String L4 = "L" + condIncr;
//
//                tCode.add("LDR " + argReg2 + " " + iCode.getArg2());
//                tCode.add("LDR " + argReg3 + " " + iCode.getResult());
//
//                tCode.add("MOV " + argReg3 + " " + argReg1 + " ; Test " + iCode.getArg1() + " > " + iCode.getArg2());
//                tCode.add("CMP " + argReg3 + " " + argReg2);
//                tCode.add("BGT " + argReg3 + " " + L3 + " ; " + iCode.getResult() + " > " + iCode.getArg2() + " GOTO " + L3);
//                tCode.add("MOV " + argReg3 + " " + argReg1 + " ; Test " + iCode.getArg1() + " == " + iCode.getArg2());
//                tCode.add("CMP " + argReg3 + " " + argReg2);
//                tCode.add("BRZ " + argReg3 + " " + L3 + " ; " + iCode.getResult() + " == " + iCode.getArg2() + " GOTO " + L3);
//                tCode.add("STR R0 " + iCode.getResult() + " ; Set FALSE");
//                tCode.add("JMP " + L4);
//                tCode.add(L3 + " STR R1 " + iCode.getResult() + " ; Set TRUE");
//
//                freeResource(argReg1);
//                freeResource(argReg2);
//                freeResource(argReg3);
//
//                useCondLabel = true;
//                tCode.add(L4);
//                continue;
//            }
//
//            if (iCode.getOperation().equals("AND")) {
//                String argReg1 = getRegister(iCode.getArg1());
//                String argReg2 = getRegister(iCode.getArg2());
//                String argReg3 = getRegister(iCode.getResult());
//
//                if (useCondLabel) {
//                    tCode.set(tCode.size() - 1, "L" + condIncr++ + " LDR " + argReg1 + " " + iCode.getArg1());
//                    useCondLabel = false;
//                } else {
//                    tCode.add("LDR " + argReg1 + " " + iCode.getArg1());
//                }
//
//                String L3 = "L" + condIncr++;
//                String L4 = "L" + condIncr;
//
//                tCode.add("CMP " + argReg1 + " R1 ; Check " + iCode.getArg1() +" for True");
//                tCode.add("BNZ  " + argReg1 + " " + L3 + " ; if FALSE GOTO " + L3);
//                tCode.add("LDR " + argReg2 + " " + iCode.getArg2());
//                tCode.add("CMP " + argReg2 + " R1 ; Check " + iCode.getArg2() +" for True");
//                tCode.add("BNZ  " + argReg2 + " " + L3 + " ; if FALSE GOTO " + L3);
//                tCode.add("STR R1 " + iCode.getResult());
//                tCode.add("JMP " + L4);
//                tCode.add(L3 + " STR R0 " + iCode.getResult());
//
//                freeResource(argReg1);
//                freeResource(argReg2);
//                freeResource(argReg3);
//
//                useCondLabel = true;
//                tCode.add(L4);
//                continue;
//            }
//
//            if (iCode.getOperation().equals("OR")) {
//                String argReg1 = getRegister(iCode.getArg1());
//                String argReg2 = getRegister(iCode.getArg2());
//                String argReg3 = getRegister(iCode.getResult());
//
//                if (useCondLabel) {
//                    tCode.set(tCode.size() - 1, "L" + condIncr++ + " LDR " + argReg1 + " " + iCode.getArg1());
//                    useCondLabel = false;
//                } else {
//                    tCode.add("LDR " + argReg1 + " " + iCode.getArg1());
//                }
//
//                String L3 = "L" + condIncr++;
//                String L4 = "L" + condIncr;
//
//                tCode.add("CMP " + argReg1 + " R1 ; Check " + iCode.getArg1() +" for True");
//                tCode.add("BRZ  " + argReg1 + " " + L3 + " ; if TRUE GOTO " + L3);
//                tCode.add("LDR " + argReg2 + " " + iCode.getArg2());
//                tCode.add("CMP " + argReg2 + " R1 ; Check " + iCode.getArg2() +" for True");
//                tCode.add("BRZ  " + argReg2 + " " + L3 + " ; if TRUE GOTO " + L3);
//                tCode.add("STR R0 " + iCode.getResult());
//                tCode.add("JMP " + L4);
//                tCode.add(L3 + " STR R1 " + iCode.getResult());
//
//                freeResource(argReg1);
//                freeResource(argReg2);
//                freeResource(argReg3);
//
//                useCondLabel = true;
//                tCode.add(L4);
//                continue;
//            }
//
//            if (iCode.getOperation().equals("BF")) {
//                String argReg1 = getRegister(iCode.getArg1());
//                if (useCondLabel) {
//                    tCode.set(tCode.size() - 1, "L" + condIncr++ + " LDR " + argReg1 + " " + iCode.getArg1());
//                    useCondLabel = false;
//                } else {
//                    tCode.add("LDR " + argReg1 + " " + iCode.getArg1());
//                }
//                tCode.add("BRZ " + argReg1 + " " + iCode.getArg2() + " " + iCode.getComment());
//                freeResource(argReg1);
//                continue;
//            }
//
//            if (iCode.getOperation().equals("BT")) {
//                String argReg1 = getRegister(iCode.getArg1());
//                if (useCondLabel) {
//                    tCode.set(tCode.size() - 1, "L" + condIncr++ + " LDR " + argReg1 + " " + iCode.getArg1());
//                    useCondLabel = false;
//                } else {
//                    tCode.add("LDR " + argReg1 + " " + iCode.getArg1());
//                }
//                tCode.add("BNZ " + argReg1 + " " + iCode.getArg2() + " " + iCode.getComment());
//                freeResource(argReg1);
//            }
        }

        // this is for debugging purposes and should be removed when done
        for (String j : tCode) {
            System.out.println(j);
        }

        try {
            FileWriter fWriter = new FileWriter("NNM-program.asm");
            BufferedWriter writer = new BufferedWriter(fWriter);

            for (String s : tCode) {
                writer.write(s);
                writer.newLine();
            }
            writer.close();
        } catch (Exception e) {
            System.out.println("error creating file");
        }

        Assembler assembler = new Assembler();
        assembler.action("NNM-program.asm");
    }

    private void mathOpr(ICode iCode, String opr) {
        String argReg1 = getRegister(iCode.getArg1());
        String argReg2 = getRegister(iCode.getArg2());

        if (iCode.getLabel().equals("")) {
            tCode.add("LDR " + argReg1 + " " + iCode.getArg1());
        } else {
            tCode.add(iCode.getLabel() + " LDR " + argReg1 + " " + iCode.getArg1());
        }
        address++;
        tCode.add("LDR " + argReg2 + " " + iCode.getArg2());
        address++;

        tCode.add(opr + " " + argReg1 + " " + argReg2);
        address++;
        tCode.add("STR " + argReg1 + " " + iCode.getResult() + " " + iCode.getComment());
        address++;

        freeResource(argReg1);
        freeResource(argReg2);
    }

    private void handleUnderFlow() {
        printOut("Error", TCodeOprConst.UNDERFLOW_LBL.getKey());
        String argReg1 = getRegister("SP");
        tCode.add("LDR " + argReg1 + " SP");
        address++;
        tCode.add("TRP 3");
        address++;
        printOut("Underflow", "");
        tCode.add("LDR " + argReg1 + " CR");
        address++;
        tCode.add("TRP 3");
        address++;
        freeResource(argReg1);
        tCode.add("TRP 0 ; exit program with underflow error");
        address++;
    }

    private void printOut(String error, String label) {
        for (char c : error.toCharArray()) {
            String argReg1 = getRegister(error);


            if (label.isEmpty()) {
                tCode.add("LDR " + argReg1 + " " + c);
            } else {
                tCode.add(label + " LDR " + argReg1 + " " + c);
                label = "";
            }
            address++;
            tCode.add("TRP 3");
            address++;
            freeResource(argReg1);
        }
    }

    private void loadAlphabet(String alphabet) {
        for (char c : alphabet.toCharArray()) {
            tCode.add(c + " .BYT '" + c + "'");
        }
    }
}
