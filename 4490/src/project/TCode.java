package project;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.util.*;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 4/7/13
 * Time: 5:30 PM
 */
public class TCode {
    private final String SP = "R100";
    private final String FP = "R99";
    private final String SL = "R98";
    private final String SB = "R97";

    private final String REG_FALSE = "R0";
    private final String REG_TRUE = "R1";

    private final String ACTIVATION_RECORD = "A0";
    private final int STACK_SIZE = 999;
    private final String END_PROGRAM = "ENDPROGRAM";
    private final int START_SIZE = 6000;

    private LinkedHashMap<String, Symbol> symbolTable = new LinkedHashMap<String, Symbol>();
    private List<ICode> iCodeList = new ArrayList<ICode>();
    private List<String> tCode = new ArrayList<String>();
    private LinkedHashMap<String, String> reg = new LinkedHashMap<String, String>();
    private String startLabel;
    private int address = 0;
    private int condIncr = START_SIZE;
    private Stack<String> L4 = new Stack<String>();

    private void initReg() {
        for (int i = 0; i < 101; i++) {
            if (i > 97) {
                reg.put("R" + i, "0");
            } else {
                reg.put("R" + i, "");
            }
        }

        // R0 is boolean false
        reg.put("R0", "0");

        // R1 is boolean true
        reg.put("R1", "1");

        // R7 used for something
        reg.put("R7", "0");

        // R100 is the stack pointer
        reg.put("R100", "0");

        // R99 is the frame pointer
        reg.put("R99", "0");

        // R98 is the stack limit
        reg.put("R98", "0");

        // R97 is the stack base
        reg.put("R97", "0");
    }

    public TCode(LinkedHashMap<String, Symbol> symbolTable, List<ICode> iCodeList, String startLabel) {
        this.symbolTable = symbolTable;
        this.iCodeList = iCodeList;
        this.startLabel = startLabel;
        initReg();
    }

    private String getNewRegister(String id) {
        for (String s : reg.keySet()) {
            if (reg.get(s).equals("")) {
                reg.put(s, id);
                return s;
            }
        }
        return null;
    }

    private String getRegister(String id) {
        for (String s : reg.keySet()) {
            if (s.equals("R" + id)) {
                reg.put(s, "0");
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
        addVariables();

        tCode.add("");
        address++;
        tCode.add("LDR R0 CLR ; Setup false reg");
        address++;
        tCode.add("LDR R1 CLR");
        address++;
        tCode.add("ADI R1 1 ; Setup true reg");
        address++;
        tCode.add("LDR R7 CLR");
        address++;
        tCode.add("LDA " + SB + " " + ACTIVATION_RECORD + " ; setup stack base: R97");
        address++;
        tCode.add("LDA " + SP + " " + ACTIVATION_RECORD + " ; setup stack pointer: R100");
        address++;
        tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + SP + " 1");
        address++;
        tCode.add("LDA " + FP + " " + ACTIVATION_RECORD + " ; setup frame pointer: R99");
        address++;

        // setup Stack Limit
        tCode.add("LDA " + SL + " " + ACTIVATION_RECORD + " ; setup stack limit: R98");
        address++;
        tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + SL + " " + STACK_SIZE);
        address++;

        String r = getRegister("5");
        tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + r + " " + (address + 2));
        address++;
        tCode.add(TCodeOprConst.STRI_OPR.getKey() + " " + FP + " " + r);
        address++;

        tCode.add(ICodeOprConst.JMP_OPR.getKey() + " " + startLabel + " ; program start");
        address++;

        // end program
        tCode.add(END_PROGRAM + " TRP 0 ; program end");
        address++;

        int listCount = -1;

        for (ICode iCode : iCodeList) {
            listCount++;
            if (iCode.getOperation().equals(ICodeOprConst.FRAME_OPR.getKey())) {

                Symbol method = symbolTable.get(iCode.getArg1());

                String reg5 = getRegister("5");
                String reg3 = getRegister("3");
                String reg6 = getRegister("6");
                String reg7 = getRegister("7");

                // check for overflow
                if (iCode.getLabel().isEmpty()) {
                    if (!L4.isEmpty()) {
                        tCode.add(L4.pop() + " " + TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + SP);
                    } else {
                        tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + SP);
                    }
                } else {
                    tCode.add(setLabel(iCode.getLabel()) + " " + TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + SP);
                }
                address++;
                tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg5 + " " + method.getObjectSize());
                address++;
                tCode.add(TCodeOprConst.CMP_OPR.getKey() + " " + reg5 + " " + SL);
                address++;
                tCode.add(TCodeOprConst.BGT_OPR.getKey() + " " + reg5 + " " + TCodeOprConst.OVERFLOW_LBL.getKey());
                address++;

                // setup frame pointers
                tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg3 + " " + FP + " ; Old Frame");
                address++;
                tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + FP + " " + SP + " ; New Frame");
                address++;
                tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + SP + " 1" + " ; PFP");
                address++;
                tCode.add(TCodeOprConst.STRI_OPR.getKey() + " " + SP + " " + reg3 + " ; Set PFP");
                address++;
                tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + SP + " 1" + " ; PFP");
                address++;

                tCode.add(TCodeOprConst.STRI_OPR.getKey() + " " + SP + " " + reg7 + " ; Set this on stack");
                address++;
                tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + SP + " 1");
                address++;


                int tempListCount = listCount + 1;
                if (iCodeList.get(tempListCount).getOperation().equals(ICodeOprConst.PUSH_OPR.getKey())) {

                    List<Integer> paramList = new ArrayList<Integer>();
                    while (iCodeList.get(tempListCount).getOperation().equals(ICodeOprConst.PUSH_OPR.getKey())) {
                        paramList.add(tempListCount++);
                    }
                    Collections.reverse(paramList);

                    for(Integer p : paramList) {
                        Symbol parameter = symbolTable.get(iCodeList.get(p).getArg1());

                        if (iCodeList.get(p).getArg1().startsWith("L")) {
                            tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg6 + " " + iCodeList.get(p).getArg1() + " ; load '" + parameter.getValue() + "' into R6");
                            address++;
                            tCode.add(TCodeOprConst.STRI_OPR.getKey() + " " + SP + " " + reg6 + " ; store ' " + parameter.getValue() + "' on the stack");
                            address++;
                            tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + SP + " 1");
                            address++;
                        } else {
                            tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + reg3);
                            address++;
                            tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg5 + " " + parameter.getSize() + " ; Address of " + parameter.getValue());
                            address++;
                            tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg6 + " " + reg5 + " ; load value of " + parameter.getValue() + " into R6");
                            address++;
                            tCode.add(TCodeOprConst.STRI_OPR.getKey() + " " + SP + " " + reg6 + " ; store " + parameter.getValue() + " on stack");
                            address++;
                            tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + SP + " 1");
                            address++;
                        }
                    }
                }

                if (iCodeList.get(tempListCount).getOperation().equals(ICodeOprConst.CALL_OPR.getKey())) {
                    ICode iCode1 = iCodeList.get(tempListCount);

                    tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg6 + " CLR");
                    address++;
                    tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg6 + " " + (address + 1) + " ; compute rtn addr");
                    address++;
                    tCode.add(TCodeOprConst.STRI_OPR.getKey() + " " + FP + " " + reg6 + " ; set rtn addr");
                    address++;
                    tCode.add(TCodeOprConst.JMP_OPR.getKey() + " " + iCode1.getArg1());
                    address++;
                }

            } else if (iCode.getOperation().equals(ICodeOprConst.FUNC_OPR.getKey())) {

                String reg5 = getRegister("5");

                Symbol method = symbolTable.get(iCode.getArg1());

                if (iCode.getLabel().isEmpty()) {
                    if (!L4.isEmpty()) {
                        tCode.add(L4.pop() + " " + TCodeOprConst.ADI_OPR.getKey() + " " + SP + " " + method.getSize());
                    } else {
                        tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + SP + " " + method.getSize());
                    }
                } else {
                    tCode.add(setLabel(iCode.getLabel()) + " " + TCodeOprConst.ADI_OPR.getKey() + " " + SP + " " + method.getSize());
                }
                address++;

                tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + SP + " ; Test Overflow");
                address++;
                tCode.add(TCodeOprConst.CMP_OPR.getKey() + " " + reg5 + " " + SL);
                address++;
                tCode.add(TCodeOprConst.BGT_OPR.getKey() + " " + reg5 + " " + TCodeOprConst.OVERFLOW_LBL.getKey());
                address++;

            } else if (iCode.getOperation().equals(ICodeOprConst.PEEK_OPR.getKey())) {

                String reg6 = getRegister("6");
                String reg3 = getRegister("3");

                if (iCode.getLabel().isEmpty()) {
                    if (!L4.isEmpty()) {
                        tCode.add(L4.pop() + " " + TCodeOprConst.LDR_OPR.getKey() + " " + reg6 + " " + SP);
                    } else {
                        tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg6 + " " + SP);
                    }
                } else {
                    tCode.add(setLabel(iCode.getLabel()) + " " + TCodeOprConst.LDR_OPR.getKey() + " " + reg6 + " " + SP);
                }
                address++;

                Symbol arg1 = symbolTable.get(iCode.getArg1());
                tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg3 + " " + FP);
                address++;
                tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg3 + " " + arg1.getSize());
                address++;
                tCode.add(TCodeOprConst.STRI_OPR.getKey() + " " + reg3 + " " + reg6);
                address++;

            } else if (isMathOperation(iCode.getOperation())) {

                mathOpr(iCode, iCode.getOperation());

            } else if (iCode.getOperation().equals(ICodeOprConst.MOD_OPR.getKey())) {

                mathModOpr(iCode);

            } else if (iCode.getOperation().equals(ICodeOprConst.MOV_OPR.getKey())) {

                String reg5 = getRegister("5");
                String reg6 = getRegister("6");

                Symbol arg1 = symbolTable.get(iCode.getArg1());

                if (iCode.getLabel().equals("")) {
                    if (!L4.isEmpty()) {
                        tCode.add(L4.pop() + " " + TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
                    } else {
                        tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
                    }
                } else {
                    tCode.add(setLabel(iCode.getLabel()) + " " + TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
                }
                address++;

                tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg5 + " " + arg1.getSize().toString() + " ; get address of " + arg1.getValue());
                address++;

                Symbol arg2 = symbolTable.get(iCode.getArg2());

                if (iCode.getArg2().startsWith("L")) {
                    tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg6 + " " + iCode.getArg2() + " ; load '" + arg2.getValue() + "' into R6");
                    address++;
                    tCode.add(TCodeOprConst.STRI_OPR.getKey() + " " + reg5 + " " + reg6 + " ; store value into address pointed to by R5");
                    address++;
                } else {
                    String reg7 = getRegister("7");

                    tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg7 + " " + FP);
                    address++;
                    tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg7 + " " + arg2.getSize().toString() + " ; get address of " + arg2.getValue());
                    address++;
                    tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg6 + " " + reg7 + " ; load value of " + arg2.getValue() + " into R6");
                    address++;
                    tCode.add(TCodeOprConst.STRI_OPR.getKey() + " " + reg5 + " " + reg6 + " ; store value into address pointed to by R5");
                    address++;
                }

            } else if (iCode.getOperation().equals(ICodeOprConst.MOVI_OPR.getKey())) {

                // currently not in use

            } else if (iCode.getOperation().equals(ICodeOprConst.WRTI_OPR.getKey()) || iCode.getOperation().equals(ICodeOprConst.WRTC_OPR.getKey())) {

                addWriteInstruction(iCode, iCode.getOperation());

            } else if (iCode.getOperation().equals(ICodeOprConst.RTN_OPR.getKey())) {

                String method = iCode.getComment().substring(iCode.getComment().indexOf(":") + 1, iCode.getComment().length()).trim();
                if (method.equals("g.main")) {
                    if (iCode.getLabel().equals("")) {
                        if (!L4.isEmpty()) {
                            tCode.add(L4.pop() + " " + TCodeOprConst.JMP_OPR.getKey() + " " + END_PROGRAM);
                        } else {
                            tCode.add(TCodeOprConst.JMP_OPR.getKey() + " " + END_PROGRAM);
                        }
                    } else {
                        tCode.add(setLabel(iCode.getLabel()) + " " + TCodeOprConst.JMP_OPR.getKey() + " " + END_PROGRAM);
                    }
                    address++;
                    continue;
                }

                String reg5 = getRegister("5");
                String reg6 = getRegister("6");

                if (iCode.getLabel().equals("")) {
                    if (!L4.isEmpty()) {
                        tCode.add(L4.pop() + " " + TCodeOprConst.MOV_OPR.getKey() + " " + SP + " " + FP);
                    } else {
                        tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + SP + " " + FP);
                    }
                } else {
                    tCode.add(setLabel(iCode.getLabel()) + " " + TCodeOprConst.MOV_OPR.getKey() + " " + SP + " " + FP);
                }
                address++;

                tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg6 + " " + FP);
                address++;
                tCode.add(TCodeOprConst.CMP_OPR.getKey() + " " + reg6 + " " + SB);
                address++;
                tCode.add(TCodeOprConst.BLT_OPR.getKey() + " " + reg6 + " " + TCodeOprConst.UNDERFLOW_LBL.getKey());
                address++;

                tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg6 + " " + FP + " ; rtn address");
                address++;
                tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
                address++;
                tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg5 + " 1");
                address++;
                tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + FP + " " + reg5 + " ; PFP into FP");
                address++;
                tCode.add(TCodeOprConst.JMR_OPR.getKey() + " " + reg6 + " " + iCode.getComment());
                address++;

            } else if (iCode.getOperation().equals(ICodeOprConst.RETURN_OPR.getKey())) {

                String reg4 = getRegister("4");
                String reg5 = getRegister("5");
                String reg6 = getRegister("6");

                Symbol arg1 = symbolTable.get(iCode.getArg1());

                if (iCode.getArg1().startsWith("L")) {
                    if (iCode.getLabel().equals("")) {
                        if (!L4.isEmpty()) {
                            tCode.add(L4.pop() + " " + TCodeOprConst.LDR_OPR.getKey() + " " + reg4 + " " + iCode.getArg1() + " ; load '" + arg1.getValue() + "' into R4");
                        } else {
                            tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg4 + " " + iCode.getArg1() + " ; load '" + arg1.getValue() + "' into R4");
                        }
                    } else {
                        tCode.add(setLabel(iCode.getLabel()) + " " + TCodeOprConst.LDR_OPR.getKey() + " " + reg4 + " " + iCode.getArg1() + " ; load '" + arg1.getValue() + "' into R4");
                    }
                    address++;
                } else {
                    if (iCode.getLabel().equals("")) {
                        if (!L4.isEmpty()) {
                            tCode.add(L4.pop() + " " + TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
                        } else {
                            tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
                        }
                    } else {
                        tCode.add(setLabel(iCode.getLabel()) + " " + TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
                    }
                    address++;
                    tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg5 + " " + arg1.getSize().toString() + " ; get address of " + arg1.getValue());
                    address++;
                    tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg4 + " " + reg5 + " ; load value of " + arg1.getValue() + " into R4");
                    address++;
                }


                // check for underflow
                tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + SP + " " + FP);
                address++;
                tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg6 + " " + FP);
                address++;
                tCode.add(TCodeOprConst.CMP_OPR.getKey() + " " + reg6 + " " + SB);
                address++;
                tCode.add(TCodeOprConst.BLT_OPR.getKey() + " " + reg6 + " " + TCodeOprConst.UNDERFLOW_LBL.getKey());
                address++;

                tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg6 + " " + FP + " ; ret address");
                address++;
                tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
                address++;
                tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg5 + " 1");
                address++;
                tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + FP + " " + reg5 + " ; PFP into FP");
                address++;
                tCode.add(TCodeOprConst.STRI_OPR.getKey() + " " + SP + " " + reg4 + " ; return " + arg1.getValue());
                address++;
                tCode.add(TCodeOprConst.JMR_OPR.getKey() + " " + reg6 + " " + iCode.getComment());
                address++;

            } else if (iCode.getOperation().equals(TCodeOprConst.JMP_OPR.getKey())) {

                if (iCode.getLabel().equals("")) {
                    tCode.add(iCode.getOperation() + " " + iCode.getArg1() + " " + iCode.getComment());
                } else {
                    tCode.add(iCode.getLabel() + " " + iCode.getOperation() + " " + iCode.getArg1() + " " + iCode.getComment());
                }
                address++;

            } else if (iCode.getOperation().equals(ICodeOprConst.RDI_OPR.getKey())) {

                if (iCode.getLabel().equals("")) {
                    if (!L4.isEmpty()) {
                        tCode.add(L4.pop() + " " + TCodeOprConst.TRP_2.getKey());
                    } else {
                        tCode.add(TCodeOprConst.TRP_2.getKey());
                    }
                } else {
                    tCode.add(iCode.getLabel() + " " + TCodeOprConst.TRP_2.getKey());
                }
                address++;

                String reg5 = getRegister("5");
                String reg6 = getRegister("6");
                Symbol arg1 = symbolTable.get(iCode.getArg1());

                tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg6 + " INII");
                address++;
                tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
                address++;
                tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg5 + " " + arg1.getSize().toString() + " ; get address of " + arg1.getValue());
                address++;
                tCode.add(TCodeOprConst.STRI_OPR.getKey() + " " + reg5 + " " + reg6 + " ; store user input into the address pointed to by R5");
                address++;

            } else if (iCode.getOperation().equals(ICodeOprConst.RDC_OPR.getKey())) {

                if (iCode.getLabel().equals("")) {
                    if (!L4.isEmpty()) {
                        tCode.add(L4.pop() + " " + TCodeOprConst.TRP_4.getKey());
                    } else {
                        tCode.add(TCodeOprConst.TRP_4.getKey());
                    }
                } else {
                    tCode.add(iCode.getLabel() + " " + TCodeOprConst.TRP_4.getKey());
                }
                address++;

                String reg5 = getRegister("5");
                String reg6 = getRegister("6");
                Symbol arg1 = symbolTable.get(iCode.getArg1());

                tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg6 + " INII");
                address++;
                tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
                address++;
                tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg5 + " " + arg1.getSize().toString() + " ; get address of " + arg1.getValue());
                address++;
                tCode.add(TCodeOprConst.STRI_OPR.getKey() + " " + reg5 + " " + reg6 + " ; store user input into the address pointed to by R5");
                address++;

            } else if (isBooleanOperation(iCode.getOperation())) {

                addBooleanOperation(iCode, iCode.getOperation());

            } else if (iCode.getOperation().equals(ICodeOprConst.LE_OPR.getKey()) || iCode.getOperation().equals(ICodeOprConst.GE_OPR.getKey())) {

                addGEorLEOperation(iCode, iCode.getOperation());

            } else if (iCode.getOperation().equals(ICodeOprConst.BF_OPR.getKey()) || iCode.getOperation().equals(ICodeOprConst.BT_OPR.getKey())) {

                addBreakTrueFalse(iCode);

            } else if (iCode.getOperation().equals(ICodeOprConst.OR_OPR.getKey())) {

                String reg5 = getRegister("5");
                String reg6 = getRegister("6");
                String reg7 = getRegister("7");

                Symbol arg1 = symbolTable.get(iCode.getArg1());
                if (iCode.getArg1().startsWith("L")) {
                    String value;
                    if (arg1.getValue().equalsIgnoreCase("true")) {
                        value = "ONE";
                    } else if (arg1.getValue().equalsIgnoreCase("false")) {
                        value = "CLR";
                    } else {
                        value = iCode.getArg1();
                    }

                    if (iCode.getLabel().isEmpty()) {
                        if (!L4.isEmpty()) {
                            tCode.add(L4.pop() + " " + TCodeOprConst.LDR_OPR.getKey() + " " + reg7 + " " + value + " ; load '" + arg1.getValue() + "' into R7");
                        } else {
                            tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg7 + " " + value + " ; load '" + arg1.getValue() + "' into R7");
                        }
                    } else {
                        tCode.add(setLabel(iCode.getLabel()) + " " + TCodeOprConst.LDR_OPR.getKey() + " " + reg7 + " " + value + " ; load '" + arg1.getValue() + "' into R7");
                    }
                    address++;
                } else {
                    if (iCode.getLabel().isEmpty()) {
                        if (!L4.isEmpty()) {
                            tCode.add(L4.pop() + " " + TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
                        } else {
                            tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
                        }
                    } else {
                        tCode.add(setLabel(iCode.getLabel()) + " " + TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
                    }
                    address++;
                    tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg5 + " " + arg1.getSize().toString() + " ; get address of " + arg1.getValue());
                    address++;
                    tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg7 + " " + reg5 + " ; load value of " + arg1.getValue() + " into R7");
                    address++;
                }

                String L3 = setupL3();
                L4.push("L" + condIncr);

                // check boolean value of arg1
                tCode.add(TCodeOprConst.CMP_OPR.getKey() + " " + reg7 + " " + REG_TRUE + " ; see if '" + arg1.getValue() + "' is true");
                address++;
                tCode.add(TCodeOprConst.BRZ_OPR.getKey() + " " + reg7 + " " + L3 + " ; if '" + arg1.getValue() + "' is true then GOTO " + L3);
                address++;

                Symbol arg2 = symbolTable.get(iCode.getArg2());
                if (iCode.getArg2().startsWith("L")) {
                    String value;
                    if (arg2.getValue().equalsIgnoreCase("true")) {
                        value = "ONE";
                    } else if (arg2.getValue().equalsIgnoreCase("false")) {
                        value = "CLR";
                    } else {
                        value = iCode.getArg2();
                    }

                    tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg6 + " " + value + " ; load '" + arg2.getValue() + "' into R6");
                    address++;
                } else {
                    tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
                    address++;
                    tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg5 + " " + arg2.getSize().toString() + " ; get address of " + arg2.getValue());
                    address++;
                    tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg6 + " " + reg5 + " ; load value of " + arg2.getValue() + " into R6");
                    address++;
                }

                // check boolean value of arg2
                tCode.add(TCodeOprConst.CMP_OPR.getKey() + " " + reg6 + " " + REG_TRUE + " ; see if '" + arg2.getValue() + "' is true");
                address++;
                tCode.add(TCodeOprConst.BRZ_OPR.getKey() + " " + reg6 + " " + L3 + " ; if '" + arg2.getValue() + "' is true then GOTO " + L3);
                address++;

                // store result
                Symbol result = symbolTable.get(iCode.getResult());
                tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
                address++;
                tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg5 + " " + result.getSize().toString() + " ; get address of " + result.getValue());
                address++;
                tCode.add(TCodeOprConst.STRI_OPR.getKey() + " " + reg5 + " " + REG_FALSE + " ; set result to false");
                address++;
                tCode.add(TCodeOprConst.JMP_OPR.getKey() + " " + L4.peek());
                address++;

                tCode.add(L3 + " " + TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
                address++;
                tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg5 + " " + result.getSize().toString() + " ; get address of " + result.getValue());
                address++;
                tCode.add(TCodeOprConst.STRI_OPR.getKey() + " " + reg5 + " " + REG_TRUE + " ; set result to true");
                address++;

            } else if (iCode.getOperation().equals(ICodeOprConst.AND_OPR.getKey())) {

                String reg5 = getRegister("5");
                String reg6 = getRegister("6");
                String reg7 = getRegister("7");

                Symbol arg1 = symbolTable.get(iCode.getArg1());
                if (iCode.getArg1().startsWith("L")) {
                    String value;
                    if (arg1.getValue().equalsIgnoreCase("true")) {
                        value = "ONE";
                    } else if (arg1.getValue().equalsIgnoreCase("false")) {
                        value = "CLR";
                    } else {
                        value = iCode.getArg1();
                    }

                    if (iCode.getLabel().isEmpty()) {
                        if (!L4.isEmpty()) {
                            tCode.add(L4.pop() + " " + TCodeOprConst.LDR_OPR.getKey() + " " + reg7 + " " + value + " ; load '" + arg1.getValue() + "' into R7");
                        } else {
                            tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg7 + " " + value + " ; load '" + arg1.getValue() + "' into R7");
                        }
                    } else {
                        tCode.add(setLabel(iCode.getLabel()) + " " + TCodeOprConst.LDR_OPR.getKey() + " " + reg7 + " " + value + " ; load '" + arg1.getValue() + "' into R7");
                    }
                    address++;
                } else {
                    if (iCode.getLabel().isEmpty()) {
                        if (!L4.isEmpty()) {
                            tCode.add(L4.pop() + " " + TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
                        } else {
                            tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
                        }
                    } else {
                        tCode.add(setLabel(iCode.getLabel()) + " " + TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
                    }
                    address++;
                    tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg5 + " " + arg1.getSize().toString() + " ; get address of " + arg1.getValue());
                    address++;
                    tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg7 + " " + reg5 + " ; load value of " + arg1.getValue() + " into R7");
                    address++;
                }

                Symbol arg2 = symbolTable.get(iCode.getArg2());
                if (iCode.getArg2().startsWith("L")) {
                    String value;
                    if (arg2.getValue().equalsIgnoreCase("true")) {
                        value = "ONE";
                    } else if (arg2.getValue().equalsIgnoreCase("false")) {
                        value = "CLR";
                    } else {
                        value = iCode.getArg2();
                    }

                    tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg6 + " " + value + " ; load '" + arg2.getValue() + "' into R6");
                    address++;
                } else {
                    tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
                    address++;
                    tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg5 + " " + arg2.getSize().toString() + " ; get address of " + arg2.getValue());
                    address++;
                    tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg6 + " " + reg5 + " ; load value of " + arg2.getValue() + " into R6");
                    address++;
                }

                String L3 = setupL3();
                L4.push("L" + condIncr);

                // check boolean value of arg1
                tCode.add(TCodeOprConst.CMP_OPR.getKey() + " " + reg7 + " " + REG_TRUE + " ; see if '" + arg1.getValue() + "' is true");
                address++;
                tCode.add(TCodeOprConst.BNZ_OPR.getKey() + "  " + reg7 + " " + L3 + " ; if '" + arg1.getValue() + "' is false then GOTO " + L3);
                address++;

                // check boolean value of arg2
                tCode.add(TCodeOprConst.CMP_OPR.getKey() + " " + reg6 + " " + REG_TRUE + " ; see if '" + arg2.getValue() + "' is true");
                address++;
                tCode.add(TCodeOprConst.BNZ_OPR.getKey() + "  " + reg6 + " " + L3 + " ; if '" + arg2.getValue() + "' is false then GOTO " + L3);
                address++;

                // store result
                Symbol result = symbolTable.get(iCode.getResult());
                tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
                address++;
                tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg5 + " " + result.getSize().toString() + " ; get address of " + result.getValue());
                address++;
                tCode.add(TCodeOprConst.STRI_OPR.getKey() + " " + reg5 + " " + REG_TRUE + " ; set result to true");
                address++;
                tCode.add(TCodeOprConst.JMP_OPR.getKey() + " " + L4.peek());
                address++;
                tCode.add(L3 + " " + TCodeOprConst.STRI_OPR.getKey() + " " + reg5 + " " + REG_FALSE + " ; set result to false");
                address++;

            }
        }

        addOverflow();
        addUnderflow();

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

    private void addUnderflow() {
        String reg = getNewRegister("0");
        tCode.add(TCodeOprConst.UNDERFLOW_LBL.getKey() + " LDR " + reg + " RC");
        address++;
        tCode.add("TRP 3");
        address++;
        tCode.add("LDR " + reg + " U");
        address++;
        tCode.add("TRP 3");
        address++;
        tCode.add("LDR " + reg + " N");
        address++;
        tCode.add("TRP 3");
        address++;
        tCode.add("LDR " + reg + " D");
        address++;
        tCode.add("TRP 3");
        address++;
        tCode.add("LDR " + reg + " E");
        address++;
        tCode.add("TRP 3");
        address++;
        tCode.add("LDR " + reg + " R");
        address++;
        tCode.add("TRP 3");
        address++;
        tCode.add("LDR " + reg + " F");
        address++;
        tCode.add("TRP 3");
        address++;
        tCode.add("LDR " + reg + " L");
        address++;
        tCode.add("TRP 3");
        address++;
        tCode.add("LDR " + reg + " O");
        address++;
        tCode.add("TRP 3");
        address++;
        tCode.add("LDR " + reg + " W");
        address++;
        tCode.add("TRP 3");
        address++;
        tCode.add("LDR " + reg + " SP");
        address++;
        tCode.add("TRP 3");
        address++;
        tCode.add("LDR " + reg + " E");
        address++;
        tCode.add("TRP 3");
        address++;
        tCode.add("LDR " + reg + " R");
        address++;
        tCode.add("TRP 3");
        address++;
        tCode.add("LDR " + reg + " R");
        address++;
        tCode.add("TRP 3");
        address++;
        tCode.add("LDR " + reg + " O");
        address++;
        tCode.add("TRP 3");
        address++;
        tCode.add("LDR " + reg + " R");
        address++;
        tCode.add("TRP 3");
        address++;
        tCode.add("LDR " + reg + " RC");
        address++;
        tCode.add("TRP 3");
        address++;
        freeResource(reg);
        tCode.add(TCodeOprConst.JMP_OPR.getKey() + " " + END_PROGRAM);
        address++;
    }

    private void addOverflow() {
        String reg = getNewRegister("0");
        tCode.add(TCodeOprConst.OVERFLOW_LBL.getKey() + " LDR " + reg + " RC");
        address++;
        tCode.add("TRP 3");
        address++;
        tCode.add("LDR " + reg + " O");
        address++;
        tCode.add("TRP 3");
        address++;
        tCode.add("LDR " + reg + " V");
        address++;
        tCode.add("TRP 3");
        address++;
        tCode.add("LDR " + reg + " E");
        address++;
        tCode.add("TRP 3");
        address++;
        tCode.add("LDR " + reg + " R");
        address++;
        tCode.add("TRP 3");
        address++;
        tCode.add("LDR " + reg + " F");
        address++;
        tCode.add("TRP 3");
        address++;
        tCode.add("LDR " + reg + " L");
        address++;
        tCode.add("TRP 3");
        address++;
        tCode.add("LDR " + reg + " O");
        address++;
        tCode.add("TRP 3");
        address++;
        tCode.add("LDR " + reg + " W");
        address++;
        tCode.add("TRP 3");
        address++;
        tCode.add("LDR " + reg + " SP");
        address++;
        tCode.add("TRP 3");
        address++;
        tCode.add("LDR " + reg + " E");
        address++;
        tCode.add("TRP 3");
        address++;
        tCode.add("LDR " + reg + " R");
        address++;
        tCode.add("TRP 3");
        address++;
        tCode.add("LDR " + reg + " R");
        address++;
        tCode.add("TRP 3");
        address++;
        tCode.add("LDR " + reg + " O");
        address++;
        tCode.add("TRP 3");
        address++;
        tCode.add("LDR " + reg + " R");
        address++;
        tCode.add("TRP 3");
        address++;
        tCode.add("LDR " + reg + " RC");
        address++;
        tCode.add("TRP 3");
        address++;
        freeResource(reg);
        tCode.add(TCodeOprConst.JMP_OPR.getKey() + " " + END_PROGRAM);
        address++;
    }

    private void addBreakTrueFalse(ICode iCode) {
        String branchType;
        if (iCode.getOperation().equals(ICodeOprConst.BF_OPR.getKey())) {
            branchType = TCodeOprConst.BRZ_OPR.getKey();
        } else {
            branchType = TCodeOprConst.BNZ_OPR.getKey();
        }

        String reg5 = getRegister("5");
        String reg6 = getRegister("6");

        Symbol arg1 = symbolTable.get(iCode.getArg1());
        if (iCode.getArg1().startsWith("L")) {
            String value;
            if (arg1.getValue().equalsIgnoreCase("true")) {
                value = "ONE";
            } else if (arg1.getValue().equalsIgnoreCase("false")) {
                value = "CLR";
            } else {
                value = iCode.getArg1();
            }

            if (iCode.getLabel().isEmpty()) {
                if (!L4.isEmpty()) {
                    tCode.add(L4.pop() + " " + TCodeOprConst.LDR_OPR.getKey() + " " + reg6 + " " + value + " ; load '" + arg1.getValue() + "' into R6");
                } else {
                    tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg6 + " " + value + " ; load '" + arg1.getValue() + "' into R6");
                }
            } else {
                tCode.add(setLabel(iCode.getLabel()) + " " + TCodeOprConst.LDR_OPR.getKey() + " " + reg6 + " " + value + " ; load '" + arg1.getValue() + "' into R6");
            }
            address++;
        } else {
            if (iCode.getLabel().isEmpty()) {
                if (!L4.isEmpty()) {
                    tCode.add(L4.pop() + " " + TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
                } else {
                    tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
                }
            } else {
                tCode.add(setLabel(iCode.getLabel()) + " " + TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
            }
            address++;
            tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg5 + " " + arg1.getSize().toString() + " ; get address of " + arg1.getValue());
            address++;
            tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg6 + " " + reg5 + " ; load value of " + arg1.getValue() + " into R6");
            address++;
        }
        tCode.add(branchType + " " + reg6 + " " + updateLabel(iCode.getArg2()) + " " + iCode.getComment());
        address++;
    }

    private void addGEorLEOperation(ICode iCode, String operation) {
        String reg4 = getRegister("4");
        String reg5 = getRegister("5");
        String reg6 = getRegister("6");
        String reg7 = getRegister("7");

        Symbol lhs = symbolTable.get(iCode.getArg1());
        if (iCode.getArg1().startsWith("L")) {
            if (iCode.getLabel().equals("")) {
                if (!L4.isEmpty()) {
                    tCode.add(L4.pop() + " " + TCodeOprConst.LDR_OPR.getKey() + " " + reg7 + " " + iCode.getArg1() + " ; load '" + lhs.getValue() + "; into R7");
                } else {
                    tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg7 + " " + iCode.getArg1() + " ; load '" + lhs.getValue() + "; into R7");
                }
            } else {
                tCode.add(setLabel(iCode.getLabel()) + " " + TCodeOprConst.LDR_OPR.getKey() + " " + reg7 + " " + iCode.getArg1() + " ; load '" + lhs.getValue() + "; into R7");
            }
            address++;
        } else {
            if (iCode.getLabel().equals("")) {
                if (!L4.isEmpty()) {
                    tCode.add(L4.pop() + " " + TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
                } else {
                    tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
                }
            } else {
                tCode.add(setLabel(iCode.getLabel()) + " " + TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
            }
            address++;

            tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg5 + " " + lhs.getSize() + " ; get address of " + lhs.getValue());
            address++;
            tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg7 + " " + reg5 + " ; put value of " + lhs.getValue() + " into R7");
            address++;
        }


        Symbol rhs = symbolTable.get(iCode.getArg2());
        if (iCode.getArg2().startsWith("L")) {
            tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg6 + " " + iCode.getArg2());
            address++;
        } else {
            tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
            address++;
            tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg5 + " " + rhs.getSize() + " ; get address of " + rhs.getValue());
            address++;
            tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg6 + " " + reg5 + " ; put value of " + rhs.getValue() + " into R6");
            address++;
        }

        String L3 = setupL3();
        L4.push("L" + condIncr);

        tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg4 + " " + reg7 + " ; Test " + lhs.getValue() + " > " + rhs.getValue());
        address++;
        tCode.add(TCodeOprConst.CMP_OPR.getKey() + " " + reg4 + " " + reg6);
        address++;
        addBranchInstruction(lhs.getValue(), rhs.getValue(), reg4, L3, operation);
        address++;

        tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg4 + " " + reg7 + " ; Test " + lhs.getValue() + " == " + rhs.getValue());
        address++;
        tCode.add(TCodeOprConst.CMP_OPR.getKey() + " " + reg4 + " " + reg6);
        address++;
        addBranchInstruction(lhs.getValue(), rhs.getValue(), reg4, L3, ICodeOprConst.EQ_OPR.getKey());
        address++;

        Symbol result = symbolTable.get(iCode.getResult());
        tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg6 + " " + REG_FALSE + " ; set False");
        address++;
        tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
        address++;
        tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg5 + " " + result.getSize().toString() + " ; get address of " + result.getValue());
        address++;
        tCode.add(TCodeOprConst.STRI_OPR.getKey() + " " + reg5 + " " + reg6 + " ; set " + result.getValue() + " to false");
        address++;

        tCode.add("JMP " + L4.peek());
        address++;

        tCode.add(L3 + " " + TCodeOprConst.MOV_OPR.getKey() + " " + reg6 + " " + REG_TRUE + " ; set True");
        address++;
        tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
        address++;
        tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg5 + " " + result.getSize().toString() + " ; get address of " + result.getValue());
        address++;
        tCode.add(TCodeOprConst.STRI_OPR.getKey() + " " + reg5 + " " + reg6 + " ; set " + result.getValue() + " to true");
        address++;
    }

    private String setupL3() {
        if (condIncr != START_SIZE) {
            condIncr++;
        }
        return "L" + condIncr++;
    }

    private void addBooleanOperation(ICode iCode, String operation) {
        String reg5 = getRegister("5");
        String reg6 = getRegister("6");
        String reg7 = getRegister("7");

        Symbol arg1 = symbolTable.get(iCode.getArg1());
        if (iCode.getArg1().startsWith("L")) {
            if (iCode.getLabel().equals("")) {
                if (!L4.isEmpty()) {
                    tCode.add(L4.pop() + " " + TCodeOprConst.LDR_OPR.getKey() + " " + reg7 + " " + iCode.getArg1() + " ; load '" + arg1.getValue() + "' into R7");
                } else {
                    tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg7 + " " + iCode.getArg1() + " ; load '" + arg1.getValue() + "' into R7");
                }
            } else {
                tCode.add(setLabel(iCode.getLabel()) + " " + TCodeOprConst.LDR_OPR.getKey() + " " + reg7 + " " + iCode.getArg1() + " ; load '" + arg1.getValue() + "' into R7");
            }
            address++;
        } else {
            if (iCode.getLabel().equals("")) {
                if (!L4.isEmpty()) {
                    tCode.add(L4.pop() + " " + TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
                } else {
                    tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
                }
            } else {
                tCode.add(setLabel(iCode.getLabel()) + " " + TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
            }
            address++;
            tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg5 + " " + arg1.getSize().toString() + " ; get address of " + arg1.getValue());
            address++;
            tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg7 + " " + reg5 + " ; load value of " + arg1.getValue() + " into R7");
            address++;
        }

        Symbol arg2 = symbolTable.get(iCode.getArg2());
        if (iCode.getArg2().startsWith("L")) {
            tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg6 + " " + iCode.getArg2() + " ; load '" + arg2.getValue() + "' into R6");
            address++;
        } else {
            tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
            address++;
            tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg5 + " " + arg2.getSize().toString() + " ; get address of " + arg2.getValue());
            address++;
            tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg6 + " " + reg5 + " ; load value of " + arg2.getValue() + " into R6");
            address++;
        }

        String L3 = setupL3();
        L4.push("L" + condIncr);

        tCode.add(TCodeOprConst.CMP_OPR.getKey() + " " + reg7 + " " + reg6);
        address++;
        addBranchInstruction(arg1.getValue(), arg2.getValue(), reg7, L3, operation);
        address++;

        Symbol result = symbolTable.get(iCode.getResult());
        tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg6 + " " + REG_FALSE + " ; set False");
        address++;
        tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
        address++;
        tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg5 + " " + result.getSize().toString() + " ; get address of " + result.getValue());
        address++;
        tCode.add(TCodeOprConst.STRI_OPR.getKey() + " " + reg5 + " " + reg6 + " ; set " + result.getValue() + " to false");
        address++;

        tCode.add("JMP " + L4.peek());
        address++;

        tCode.add(L3 + " " + TCodeOprConst.MOV_OPR.getKey() + " " + reg6 + " " + REG_TRUE + " ; set True");
        address++;
        tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
        address++;
        tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg5 + " " + result.getSize().toString() + " ; get address of " + result.getValue());
        address++;
        tCode.add(TCodeOprConst.STRI_OPR.getKey() + " " + reg5 + " " + reg6 + " ; set " + result.getValue() + " to true");
        address++;

    }

    private void addBranchInstruction(String arg1, String arg2, String result, String jmpLabel, String operation) {
        if (operation.equals(ICodeOprConst.EQ_OPR.getKey())) {
            tCode.add("BRZ " + result + " " + jmpLabel + " ; " + arg1 + " == " + arg2 + " GOTO " + jmpLabel);
        } else if (operation.equals(ICodeOprConst.GT_OPR.getKey()) || operation.equals(ICodeOprConst.GE_OPR.getKey())) {
            tCode.add("BGT " + result + " " + jmpLabel + " ; " + arg1 + " > " + arg2 + " GOTO " + jmpLabel);
        } else if (operation.equals(ICodeOprConst.LT_OPR.getKey()) || operation.equals(ICodeOprConst.LE_OPR.getKey())) {
            tCode.add("BLT " + result + " " + jmpLabel + " ; " + arg1 + " < " + arg2 + " GOTO " + jmpLabel);
        } else if (operation.equals(ICodeOprConst.NE_OPR.getKey())) {
            tCode.add("BNZ " + result + " " + jmpLabel + " ; " + arg1 + " != " + arg2 + " GOTO " + jmpLabel);
        }
    }

    private void mathModOpr(ICode iCode) {
        String reg5 = getRegister("5");
        String reg6 = getRegister("6");
        String reg7 = getRegister("7");

        Symbol lhs = symbolTable.get(iCode.getArg1());
        if (iCode.getArg1().startsWith("L")) {
            if (iCode.getLabel().equals("")) {
                if (!L4.isEmpty()) {
                    tCode.add(L4.pop() + " " + TCodeOprConst.LDR_OPR.getKey() + " " + reg7 + " " + iCode.getArg1() + " ; load '" + lhs.getValue() + "; into R7");
                } else {
                    tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg7 + " " + iCode.getArg1() + " ; load '" + lhs.getValue() + "; into R7");
                }
            } else {
                tCode.add(setLabel(iCode.getLabel()) + " " + TCodeOprConst.LDR_OPR.getKey() + " " + reg7 + " " + iCode.getArg1() + " ; load '" + lhs.getValue() + "; into R7");
            }
            address++;
        } else {
            if (iCode.getLabel().equals("")) {
                if (!L4.isEmpty()) {
                    tCode.add(L4.pop() + " " + TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
                } else {
                    tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
                }
            } else {
                tCode.add(setLabel(iCode.getLabel()) + " " + TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
            }
            address++;

            tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg5 + " " + lhs.getSize() + " ; get address of " + lhs.getValue());
            address++;
            tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg7 + " " + reg5 + " ; put value of " + lhs.getValue() + " into R7");
            address++;
        }


        Symbol rhs = symbolTable.get(iCode.getArg2());
        if (iCode.getArg2().startsWith("L")) {
            tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg6 + " " + iCode.getArg2());
            address++;
        } else {
            tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
            address++;
            tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg5 + " " + rhs.getSize() + " ; get address of " + rhs.getValue());
            address++;
            tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg6 + " " + reg5 + " ; put value of " + rhs.getValue() + " into R6");
            address++;
        }

        tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + reg7);
        address++;
        tCode.add(TCodeOprConst.DIV_OPR.getKey() + " " + reg5 + " " + reg6);
        address++;
        tCode.add(TCodeOprConst.MUL_OPR.getKey() + " " + reg5 + " " + reg6);
        address++;
        tCode.add(TCodeOprConst.SUB_OPR.getKey() + " " + reg7 + " " + reg5);
        address++;

        Symbol result = symbolTable.get(iCode.getResult());
        tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
        address++;
        tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg5 + " " + result.getSize() + " ; get address of " + result.getValue());
        address++;
        tCode.add(TCodeOprConst.STRI_OPR.getKey() + " " + reg5 + " " + reg7 + " " + iCode.getComment());
        address++;
    }

    private void addWriteInstruction(ICode iCode, String operation) {
        String reg5 = getRegister("5");
        String reg6 = getRegister("6");
        Symbol arg1 = symbolTable.get(iCode.getArg1());

        if (iCode.getArg1().startsWith("L")) {
            if (iCode.getLabel().equals("")) {
                if (!L4.isEmpty()) {
                    tCode.add(L4.pop() + " " + TCodeOprConst.LDR_OPR.getKey() + " " + reg6 + " " + iCode.getArg1() + " ; load '" + arg1.getValue() + "' into R6");
                } else {
                    tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg6 + " " + iCode.getArg1() + " ; load '" + arg1.getValue() + "' into R6");
                }
            } else {
                tCode.add(setLabel(iCode.getLabel()) + " " + TCodeOprConst.LDR_OPR.getKey() + " " + reg6 + " " + iCode.getArg1() + " ; load '" + arg1.getValue() + "' into R6");
            }
            address++;
        } else {
            if (iCode.getLabel().equals("")) {
                if (!L4.isEmpty()) {
                    tCode.add(L4.pop() + " " + TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
                } else {
                    tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
                }
            } else {
                tCode.add(setLabel(iCode.getLabel()) + " " + TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
            }
            address++;
            tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg5 + " " + arg1.getSize().toString() + " ; get address of " + arg1.getValue());
            address++;
            tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg6 + " " + reg5 + " ; load value of " + arg1.getValue() + " into R6");
            address++;
        }

        if (operation.equals(ICodeOprConst.WRTC_OPR.getKey())) {
            tCode.add(TCodeOprConst.TRP_3.getKey() + " " + iCode.getComment());
        } else {
            tCode.add(TCodeOprConst.TRP_1.getKey() + " " + iCode.getComment());
        }
        address++;
    }

    private void addVariables() {
        tCode.add("CLR .INT 0");
        tCode.add("ONE .INT 1");
        tCode.add("RC .BYT '13'");
        tCode.add("SP .BYT '32'");

        addErrorLetters();

        for (String key : symbolTable.keySet()) {
            Symbol s = symbolTable.get(key);

            if (s.getSymId().startsWith("L") && Character.isDigit(s.getSymId().toCharArray()[1])) {
                if (s.getData() instanceof VariableData) {
                    if (s.getData().getType().equalsIgnoreCase("int") || s.getData().getType().equalsIgnoreCase(LexicalAnalyzer.tokenTypesEnum.NUMBER.name())) {
                        tCode.add(s.getSymId() + " .INT " + s.getValue());
                    } else {
                        if (s.getValue().equals("\'\\n\'")) {
                            tCode.add(s.getSymId() + " .BYT " + "\'13\'");
                        } else if ((int) s.getValue().charAt(1) == 32) {
                            tCode.add(s.getSymId() + " .BYT " + "\'32\'");
                        } else {
                            tCode.add(s.getSymId() + " .BYT " + s.getValue());
                        }
                    }
                }
            }
        }

        for (ICode iCode : iCodeList) {
            if (iCode.getOperation().equals(ICodeOprConst.CREATE_OPR.getKey()) && !iCode.getLabel().startsWith("L")) {
                if (symbolTable.get(iCode.getLabel()).getData() instanceof VariableData) {
                    if ((symbolTable.get(iCode.getLabel()).getData()).getType().equalsIgnoreCase(KeyConst.INT.getKey())) {
                        tCode.add(iCode.getLabel() + " " + iCode.getArg1() + " " + "0" + " " + iCode.getComment());
                    } else {
                        tCode.add(iCode.getLabel() + " " + iCode.getArg1() + " " + "\'0\'" + " " + iCode.getComment());
                    }
                } else if (symbolTable.get(iCode.getLabel()).getData() instanceof MethodData) {
                    tCode.add(iCode.getLabel() + " " + iCode.getArg1() + " " + "\'0\'" + iCode.getComment());
                }
            }
        }

        tCode.add(ACTIVATION_RECORD + " .INT 0");
        String tempRec = ACTIVATION_RECORD.substring(0, ACTIVATION_RECORD.length() - 1);
        for (int i = 1; i <= STACK_SIZE; i++) {
            tCode.add(tempRec + i + " .INT 0");
        }
    }

    private void addErrorLetters() {
        tCode.add("O .BYT 'O'");
        tCode.add("V .BYT 'V'");
        tCode.add("E .BYT 'E'");
        tCode.add("R .BYT 'R'");
        tCode.add("F .BYT 'F'");
        tCode.add("L .BYT 'L'");
        tCode.add("W .BYT 'W'");
        tCode.add("U .BYT 'U'");
        tCode.add("N .BYT 'N'");
        tCode.add("D .BYT 'D'");
    }

    private void mathOpr(ICode iCode, String opr) {
        String reg5 = getRegister("5");
        String reg6 = getRegister("6");
        String reg7 = getRegister("7");

        Symbol lhs = symbolTable.get(iCode.getArg1());
        if (iCode.getArg1().startsWith("L")) {
            if (iCode.getLabel().equals("")) {
                if (!L4.isEmpty()) {
                    tCode.add(L4.pop() + " " + TCodeOprConst.LDR_OPR.getKey() + " " + reg7 + " " + iCode.getArg1() + " ; load '" + lhs.getValue() + "; into R7");
                } else {
                    tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg7 + " " + iCode.getArg1() + " ; load '" + lhs.getValue() + "; into R7");
                }
            } else {
                tCode.add(setLabel(iCode.getLabel()) + " " + TCodeOprConst.LDR_OPR.getKey() + " " + reg7 + " " + iCode.getArg1() + " ; load '" + lhs.getValue() + "; into R7");
            }
            address++;
        } else {
            if (iCode.getLabel().equals("")) {
                if (!L4.isEmpty()) {
                    tCode.add(L4.pop() + " " + TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
                } else {
                    tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
                }
            } else {
                tCode.add(setLabel(iCode.getLabel()) + " " + TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
            }
            address++;

            tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg5 + " " + lhs.getSize() + " ; get address of " + lhs.getValue());
            address++;
            tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg7 + " " + reg5 + " ; put value of " + lhs.getValue() + " into R7");
            address++;
        }


        Symbol rhs = symbolTable.get(iCode.getArg2());
        if (iCode.getArg2().startsWith("L")) {
            tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg6 + " " + iCode.getArg2());
            address++;
        } else {
            tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
            address++;
            tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg5 + " " + rhs.getSize() + " ; get address of " + rhs.getValue());
            address++;
            tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg6 + " " + reg5 + " ; put value of " + rhs.getValue() + " into R6");
            address++;
        }

        tCode.add(opr + " " + reg7 + " " + reg6 + " ; " + opr + " " + lhs.getValue() + " and " + rhs.getValue());
        address++;

        Symbol result = symbolTable.get(iCode.getResult());
        tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg5 + " " + FP);
        address++;
        tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg5 + " " + result.getSize() + " ; get address of " + result.getValue());
        address++;
        tCode.add(TCodeOprConst.STRI_OPR.getKey() + " " + reg5 + " " + reg7 + " ; store result into address pointed to by R5");
        address++;
    }

    private String updateLabel(String label) {
        if (labelHelper(label)) return label;
        L4.push(label);
        return label;
    }

    private String setLabel(String label) {
        if (labelHelper(label)) return label;
        return label;
    }

    private boolean labelHelper(String label) {
        if (L4.isEmpty()) {
            return true;
        }

        if (L4.peek().equals(label)) {
            L4.pop();
            return true;
        }

        for (int i = 0; i < tCode.size(); i++) {
            tCode.set(i, tCode.get(i).replace(L4.peek(), label));
        }

        L4.pop();
        return false;
    }

    /**
     * returns true if operation is +, -, /, or *
     *
     * @param operation Icode operation
     * @return true if operation is +, -, /, or *
     */
    private boolean isMathOperation(String operation) {
        return (operation.equals(ICodeOprConst.ADD_OPR.getKey()) ||
                operation.equals(ICodeOprConst.ADI_OPR.getKey()) ||
                operation.equals(ICodeOprConst.SUB_OPR.getKey()) ||
                operation.equals(ICodeOprConst.MUL_OPR.getKey()) ||
                operation.equals(ICodeOprConst.DIV_OPR.getKey()));
    }

    /**
     * returns true if operation is ==, >, <, !=
     *
     * @param operation Icode operation
     * @return true if operation is ==, >, <, !=
     */
    private boolean isBooleanOperation(String operation) {
        return (operation.equals(ICodeOprConst.EQ_OPR.getKey()) ||
                operation.equals(ICodeOprConst.GT_OPR.getKey()) ||
                operation.equals(ICodeOprConst.LT_OPR.getKey()) ||
                operation.equals(ICodeOprConst.NE_OPR.getKey()));
    }
}
