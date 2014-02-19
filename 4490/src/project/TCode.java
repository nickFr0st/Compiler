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
    private final static int PARAM_REG_START = 96;
    private final static String RETURN_VALUE_REG = "R97";

    private final static int SB = 20;

    private final int START_SIZE = 6000;
    private final String ACTIVATION_RECORD = "AR";
    private final String RETURN_ADDRESS = "RA";

    private LinkedHashMap<String, Symbol> symbolTable = new LinkedHashMap<String, Symbol>();
    private List<ICode> iCodeList = new ArrayList<ICode>();
    private List<String> tCode = new ArrayList<String>();
    private LinkedHashMap<String, String> reg = new LinkedHashMap<String, String>();
    private String startLabel;
    private int address = 0;
    private int condIncr = START_SIZE;
    private Stack<String> L4 = new Stack<String>();
    private Stack<Integer> retAddressStack = new Stack<Integer>();

    private int SP = 0;
    private int RP = 0;
    private Stack<String> FP = new Stack<String>();


    private void initReg() {
        for (int i = 0; i < 101; i++) {
            if (i > PARAM_REG_START) {
                reg.put("R" + i, "0");
            } else {
                reg.put("R" + i, "");
            }
        }

        reg.put("R0", "0");
        reg.put("R1", "1");
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
        addVariables();

        tCode.add("");
        address++;
        tCode.add("LDR R0 CLR");
        address++;
        tCode.add("LDR R1 CLR");
        address++;
        tCode.add("ADI R1 1");
        address++;

        // setup mains' activation record
        String r = getRegister("0");

        tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + r + " CLR");
        address++;
        tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + r + " " + (address + 5));
        address++;
        tCode.add(TCodeOprConst.STR_OPR.getKey() + " " + r + " " + RETURN_ADDRESS + RP);
        address++;
        tCode.add(TCodeOprConst.LDA_OPR.getKey() + " " + r + " " + RETURN_ADDRESS + RP++);
        address++;
        tCode.add(TCodeOprConst.STR_OPR.getKey() + " " + r + " " + ACTIVATION_RECORD + SP);
        address++;
        tCode.add(TCodeOprConst.LDA_OPR.getKey() + " R" + PARAM_REG_START + " " + ACTIVATION_RECORD + SP++);
        address++;

        freeResource(r);

        tCode.add(ICodeOprConst.JMP_OPR.getKey() + " " + startLabel + " ; program start");
        address++;

        // end program
        tCode.add("TRP 0 ; program end");
        address++;

        int listCount = -1;

        for (ICode iCode : iCodeList) {
            listCount++;
            if (iCode.getOperation().equals(ICodeOprConst.FRAME_OPR.getKey())) {

                String reg1 = getRegister("0");

                if (iCode.getLabel().isEmpty()) {
                    if (!L4.isEmpty()) {
                        tCode.add(L4.pop() + " " + TCodeOprConst.LDR_OPR.getKey() + " " + reg1 + " CLR");
                    } else {
                        tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg1 + " CLR");
                    }
                } else {
                    tCode.add(setLabel(iCode.getLabel()) + " " + TCodeOprConst.LDR_OPR.getKey() + " " + reg1 + " CLR");
                }
                address++;

                int tempListCount = listCount + 1;
                if (iCodeList.get(listCount + 1).getOperation().equals(ICodeOprConst.PUSH_OPR.getKey())) {
                    while (iCodeList.get(tempListCount).getOperation().equals(ICodeOprConst.PUSH_OPR.getKey())) {
                        tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg1 + " " + iCodeList.get(tempListCount).getArg1());
                        address++;
                        tCode.add(TCodeOprConst.STR_OPR.getKey() + " " + reg1 + " " + ACTIVATION_RECORD + SP++);
                        address++;
                        tempListCount++;
                    }
                }

                if (iCodeList.get(tempListCount).getOperation().equals(ICodeOprConst.CALL_OPR.getKey())) {
                    ICode iCode1 = iCodeList.get(tempListCount);

                    tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg1 + " CLR");
                    address++;
                    tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg1 + " " + (address + 5));
                    address++;
                    tCode.add(TCodeOprConst.STR_OPR.getKey() + " " + reg1 + " " + RETURN_ADDRESS + RP);
                    address++;

                    tCode.add(TCodeOprConst.LDA_OPR.getKey() + " " + reg1 + " " + RETURN_ADDRESS + RP++);
                    address++;
                    tCode.add(TCodeOprConst.STR_OPR.getKey() + " " + reg1 + " " + ACTIVATION_RECORD + SP);
                    address++;

                    tCode.add(TCodeOprConst.LDA_OPR.getKey() + " R" + PARAM_REG_START + " " + ACTIVATION_RECORD + SP);
                    address++;
                    tCode.add(TCodeOprConst.JMP_OPR.getKey() + " " + iCode1.getArg1());
                    address++;
                    FP.push(ACTIVATION_RECORD + SP);
                    SP++;
                }

                freeResource(r);

            } else if (iCode.getOperation().equals(ICodeOprConst.FUNC_OPR.getKey())) {

                String reg1 = getRegister(iCode.getArg1());
                String reg2 = getRegister("2");

                if (iCode.getLabel().isEmpty()) {
                    if (!L4.isEmpty()) {
                        tCode.add(L4.pop() + " " + TCodeOprConst.LDR_OPR.getKey() + " " + reg1 + " CLR");
                    } else {
                        tCode.add(iCode.getArg1() + " " + TCodeOprConst.LDR_OPR.getKey() + " " + reg1 + " CLR");
                    }
                } else {
                    tCode.add(setLabel(iCode.getLabel()) + " " + TCodeOprConst.LDR_OPR.getKey() + " " + reg1 + " " + " CLR");
                }
                address++;

                tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg1 + " R" + PARAM_REG_START);

                if (iCodeList.get(listCount + 1).getOperation().equals(ICodeOprConst.CREATE_OPR.getKey()) && !iCodeList.get(listCount + 1).getLabel().isEmpty() && iCodeList.get(listCount + 1).getLabel().startsWith("P")) {
                    int tempListCount = listCount + 1;

                    while (iCodeList.get(tempListCount).getOperation().equals(ICodeOprConst.CREATE_OPR.getKey()) && !iCodeList.get(tempListCount).getLabel().isEmpty() && iCodeList.get(tempListCount).getLabel().startsWith("P")) {
                        tCode.add(TCodeOprConst.ADI_OPR.getKey() + " " + reg1 + " -1");
                        address++;
                        tCode.add(TCodeOprConst.ADDI_OPR.getKey() + " " + reg2 + " " + reg1);
                        address++;
                        tCode.add(TCodeOprConst.STR_OPR.getKey() + " " + reg2 + " " + iCodeList.get(tempListCount).getLabel());
                        address++;
                        tempListCount++;
                    }
                }

                freeResource(reg1);
                freeResource(reg2);

            } else if (iCode.getOperation().equals(ICodeOprConst.PEEK_OPR.getKey())) {

                if (iCode.getLabel().isEmpty()) {
                    if (!L4.isEmpty()) {
                        tCode.add(L4.pop() + " " + TCodeOprConst.STR_OPR.getKey() + " " + RETURN_VALUE_REG + " " + iCode.getArg1());
                    } else {
                        tCode.add(TCodeOprConst.STR_OPR.getKey() + " " + RETURN_VALUE_REG + " " + iCode.getArg1());
                    }
                } else {
                    tCode.add(setLabel(iCode.getLabel()) + " " + TCodeOprConst.STR_OPR.getKey() + " " + RETURN_VALUE_REG + " " + iCode.getArg1());
                }
                address++;

            } else if (isMathOperation(iCode.getOperation())) {

                mathOpr(iCode, iCode.getOperation());

            } else if (iCode.getOperation().equals(ICodeOprConst.MOD_OPR.getKey())) {

                mathModOpr(iCode);

            } else if (iCode.getOperation().equals(ICodeOprConst.MOV_OPR.getKey())) {

                String argReg1 = getRegister(iCode.getArg1());
                String argReg2 = getRegister(iCode.getArg2());

                if (iCode.getLabel().equals("")) {
                    if (!L4.isEmpty()) {
                        tCode.add(L4.pop() + " LDR " + argReg1 + " " + iCode.getArg1());
                    } else {
                        tCode.add("LDR " + argReg1 + " " + iCode.getArg1());
                    }
                } else {
                    tCode.add(setLabel(iCode.getLabel()) + " LDR " + argReg1 + " " + iCode.getArg1());
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

            } else if (iCode.getOperation().equals(ICodeOprConst.MOVI_OPR.getKey())) {

                String argReg1 = getRegister(iCode.getArg2());
                String value = symbolTable.get(iCode.getArg2()).getValue();

                if (iCode.getLabel().equals("")) {
                    if (!L4.isEmpty()) {
                        tCode.add(L4.pop() + " ADI " + argReg1 + " " + value.substring(1, value.length()));
                    } else {
                        tCode.add("ADI " + argReg1 + " " + value.substring(1, value.length()));
                    }
                } else {
                    tCode.add(setLabel(iCode.getLabel()) + " ADI " + argReg1 + " " + value.substring(1, value.length()));
                }
                address++;

                tCode.add("STR " + argReg1 + " " + iCode.getArg1() + " " + iCode.getComment());
                address++;
                freeResource(argReg1);

            } else if (iCode.getOperation().equals(ICodeOprConst.WRTI_OPR.getKey()) || iCode.getOperation().equals(ICodeOprConst.WRTC_OPR.getKey())) {

                addWriteInstruction(iCode, iCode.getOperation());

            } else if (iCode.getOperation().equals(ICodeOprConst.RTN_OPR.getKey())) {

                // todo: needs to be re-worked

                String methodScope = iCode.getComment().substring(iCode.getComment().indexOf(":") + 1, iCode.getComment().lastIndexOf(".")).trim();
                if (methodScope.equals("g")) {
                    methodScope += ".";
                }
                String methodName = iCode.getComment().substring(iCode.getComment().indexOf(".") + 1, iCode.getComment().length()).trim();

                Symbol symbol = getSymbol(methodName, methodScope);


                String reg1 = getRegister("0");
                String reg2 = getRegister("0");

                if (iCode.getLabel().equals("")) {
                    if (!L4.isEmpty()) {
                        tCode.add(L4.pop() + " " + TCodeOprConst.LDR_OPR.getKey() + " " + reg1 + " CLR");
                    } else {
                        tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg1 + " CLR");
                    }
                } else {
                    tCode.add(setLabel(iCode.getLabel()) + " " + TCodeOprConst.LDR_OPR.getKey() + " " + reg1 + " CLR");
                }
                address++;

                tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg2 + " CLR");
                address++;
                tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg1 + " R" + PARAM_REG_START);
                address++;
                tCode.add(TCodeOprConst.ADDI_OPR.getKey() + " " + reg2 + " " + reg1);
                address++;
                tCode.add(TCodeOprConst.ADDI_OPR.getKey() + " " + reg1 + " " + reg2);
                address++;

                tCode.add("JMR " + reg1 + " " + iCode.getComment());
                address++;

                freeResource(reg1);
                freeResource(reg2);

            } else if (iCode.getOperation().equals(ICodeOprConst.RETURN_OPR.getKey())) {

                // todo: remove parameter value by moving the param reg pointer up the number of params in method you are leaving

                String reg1 = getRegister("0");
                String reg2 = getRegister("0");

                if (iCode.getLabel().equals("")) {
                    if (!L4.isEmpty()) {
                        tCode.add(L4.pop() + " " + TCodeOprConst.LDR_OPR.getKey() + " " + reg1 + " CLR");
                    } else {
                        tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg1 + " CLR");
                    }
                } else {
                    tCode.add(setLabel(iCode.getLabel()) + " " + TCodeOprConst.LDR_OPR.getKey() + " " + reg1 + " CLR");
                }
                address++;

                tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + reg2 + " CLR");
                address++;
                tCode.add(TCodeOprConst.MOV_OPR.getKey() + " " + reg1 + " R" + PARAM_REG_START);
                address++;
                tCode.add(TCodeOprConst.ADDI_OPR.getKey() + " " + reg2 + " " + reg1);
                address++;
                tCode.add(TCodeOprConst.ADDI_OPR.getKey() + " " + reg1 + " " + reg2);
                address++;

                tCode.add(TCodeOprConst.LDR_OPR.getKey() + " " + RETURN_VALUE_REG + " " + iCode.getArg1());
                address++;

                tCode.add("JMR " + reg1 + " " + iCode.getComment());
                address++;

                freeResource(reg1);
                freeResource(reg2);

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
                        tCode.add(L4.pop() + " TRP 2");
                    } else {
                        tCode.add("TRP 2");
                    }
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

            } else if (iCode.getOperation().equals(ICodeOprConst.RDC_OPR.getKey())) {

                if (iCode.getLabel().equals("")) {
                    if (!L4.isEmpty()) {
                        tCode.add(L4.pop() + " TRP 4");
                    } else {
                        tCode.add("TRP 4");
                    }
                } else {
                    tCode.add(setLabel(iCode.getLabel()) + "TRP 4");
                }
                address++;
                String argReg1 = getRegister(iCode.getArg1());

                tCode.add("LDR " + argReg1 + " INPT");
                address++;
                tCode.add("STR " + argReg1 + " " + iCode.getArg1());
                address++;
                freeResource(argReg1);

            } else if (isBooleanOperation(iCode.getOperation())) {

                addBooleanOperation(iCode, iCode.getOperation());

            } else if (iCode.getOperation().equals(ICodeOprConst.LE_OPR.getKey()) || iCode.getOperation().equals(ICodeOprConst.GE_OPR.getKey())) {

                addGEorLEOperation(iCode, iCode.getOperation());

            } else if (iCode.getOperation().equals(ICodeOprConst.BF_OPR.getKey()) || iCode.getOperation().equals(ICodeOprConst.BT_OPR.getKey())) {

                addBreakTrueFalse(iCode);

            } else if (iCode.getOperation().equals(ICodeOprConst.OR_OPR.getKey())) {

                String argReg1 = getRegister(iCode.getArg1());
                String argReg2 = getRegister(iCode.getArg2());
                String argReg3 = getRegister(iCode.getResult());

                if (iCode.getLabel().isEmpty()) {
                    if (!L4.isEmpty()) {
                        tCode.add(L4.pop() + " LDR " + argReg1 + " " + iCode.getArg1());
                    } else {
                        tCode.add("LDR " + argReg1 + " " + iCode.getArg1());
                    }
                } else {
                    tCode.add(setLabel(iCode.getLabel()) + " LDR " + argReg1 + " " + iCode.getArg1());
                }
                address++;

                String L3 = setupL3();
                L4.push("L" + condIncr);

                tCode.add("CMP " + argReg1 + " R1 ; Check " + iCode.getArg1() + " for True");
                address++;
                tCode.add("BRZ  " + argReg1 + " " + L3 + " ; if TRUE GOTO " + L3);
                address++;
                tCode.add("LDR " + argReg2 + " " + iCode.getArg2());
                address++;
                tCode.add("CMP " + argReg2 + " R1 ; Check " + iCode.getArg2() + " for True");
                address++;
                tCode.add("BRZ  " + argReg2 + " " + L3 + " ; if TRUE GOTO " + L3);
                address++;
                tCode.add("STR R0 " + iCode.getResult());
                address++;
                tCode.add("JMP " + L4.peek());
                address++;
                tCode.add(L3 + " STR R1 " + iCode.getResult());
                address++;

                freeResource(argReg1);
                freeResource(argReg2);
                freeResource(argReg3);

            } else if (iCode.getOperation().equals(ICodeOprConst.AND_OPR.getKey())) {

                String argReg1 = getRegister(iCode.getArg1());
                String argReg2 = getRegister(iCode.getArg2());
                String argReg3 = getRegister(iCode.getResult());

                if (iCode.getLabel().isEmpty()) {
                    if (!L4.isEmpty()) {
                        tCode.add(L4.pop() + " LDR " + argReg1 + " " + iCode.getArg1());
                    } else {
                        tCode.add("LDR " + argReg1 + " " + iCode.getArg1());
                    }
                } else {
                    tCode.add(setLabel(iCode.getLabel()) + " LDR " + argReg1 + " " + iCode.getArg1());
                }
                address++;

                String L3 = setupL3();
                L4.push("L" + condIncr);

                tCode.add("CMP " + argReg1 + " R1 ; Check " + iCode.getArg1() + " for True");
                address++;
                tCode.add("BNZ  " + argReg1 + " " + L3 + " ; if FALSE GOTO " + L3);
                address++;
                tCode.add("LDR " + argReg2 + " " + iCode.getArg2());
                address++;
                tCode.add("CMP " + argReg2 + " R1 ; Check " + iCode.getArg2() + " for True");
                address++;
                tCode.add("BNZ  " + argReg2 + " " + L3 + " ; if FALSE GOTO " + L3);
                address++;
                tCode.add("STR R1 " + iCode.getResult());
                address++;
                tCode.add("JMP " + L4.peek());
                address++;
                tCode.add(L3 + " STR R0 " + iCode.getResult());
                address++;

                freeResource(argReg1);
                freeResource(argReg2);
                freeResource(argReg3);
            }
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

    private void addBreakTrueFalse(ICode iCode) {
        String branchType;
        if (iCode.getOperation().equals(ICodeOprConst.BF_OPR.getKey())) {
            branchType = "BRZ ";
        } else {
            branchType = "BNZ ";
        }

        String argReg1 = getRegister(iCode.getArg1());
        if (iCode.getLabel().isEmpty()) {
            if (!L4.isEmpty()) {
                tCode.add(L4.pop() + " LDR " + argReg1 + " " + iCode.getArg1());
            } else {
                tCode.add("LDR " + argReg1 + " " + iCode.getArg1());
            }
        } else {
            tCode.add(setLabel(iCode.getLabel()) + " LDR " + argReg1 + " " + iCode.getArg1());
        }
        address++;
        tCode.add(branchType + argReg1 + " " + updateLabel(iCode.getArg2()) + " " + iCode.getComment());
        address++;
        freeResource(argReg1);
    }

    private void addGEorLEOperation(ICode iCode, String operation) {
        String argReg1 = getRegister(iCode.getArg1());
        String argReg2 = getRegister(iCode.getArg2());
        String argReg3 = getRegister(iCode.getResult());

        if (iCode.getLabel().equals("")) {
            if (!L4.isEmpty()) {
                tCode.add(L4.pop() + " LDR " + argReg1 + " " + iCode.getArg1());
            } else {
                tCode.add("LDR " + argReg1 + " " + iCode.getArg1());
            }
        } else {
            tCode.add(setLabel(iCode.getLabel()) + " LDR " + argReg1 + " " + iCode.getArg1());
        }
        address++;

        String L3 = setupL3();
        L4.push("L" + condIncr);

        tCode.add("LDR " + argReg2 + " " + iCode.getArg2());
        address++;
        tCode.add("LDR " + argReg3 + " " + iCode.getResult());
        address++;

        tCode.add("MOV " + argReg3 + " " + argReg1 + " ; Test " + iCode.getArg1() + " > " + iCode.getArg2());
        address++;
        tCode.add("CMP " + argReg3 + " " + argReg2);
        address++;
        addBranchInstruction(argReg1, argReg2, argReg3, L3, operation);
        address++;
        tCode.add("MOV " + argReg3 + " " + argReg1 + " ; Test " + iCode.getArg1() + " == " + iCode.getArg2());
        address++;
        tCode.add("CMP " + argReg3 + " " + argReg2);
        address++;
        addBranchInstruction(argReg1, argReg2, argReg3, L3, ICodeOprConst.EQ_OPR.getKey());
        address++;
        tCode.add("STR R0 " + iCode.getResult() + " ; Set FALSE");
        address++;
        tCode.add("JMP " + L4.peek());
        address++;
        tCode.add(L3 + " STR R1 " + iCode.getResult() + " ; Set TRUE");
        address++;

        freeResource(argReg1);
        freeResource(argReg2);
        freeResource(argReg3);
    }

    private String setupL3() {
        if (condIncr != START_SIZE) {
            condIncr++;
        }
        return "L" + condIncr++;
    }

    private void addBooleanOperation(ICode iCode, String operation) {
        String argReg1 = getRegister(iCode.getArg1());
        String argReg2 = getRegister(iCode.getArg2());
        String argReg3 = getRegister(iCode.getResult());

        if (iCode.getLabel().equals("")) {
            if (!L4.isEmpty()) {
                tCode.add(L4.pop() + " LDR " + argReg1 + " " + iCode.getArg1());
            } else {
                tCode.add("LDR " + argReg1 + " " + iCode.getArg1());
            }
        } else {
            tCode.add(setLabel(iCode.getLabel()) + " LDR " + argReg1 + " " + iCode.getArg1());
        }
        address++;

        String L3 = setupL3();
        L4.push("L" + condIncr);

        tCode.add("LDR " + argReg2 + " " + iCode.getArg2());
        address++;
        tCode.add("LDR " + argReg3 + " " + iCode.getResult());
        address++;

        tCode.add("MOV " + argReg3 + " " + argReg1);
        address++;
        tCode.add("CMP " + argReg3 + " " + argReg2);
        address++;
        addBranchInstruction(argReg1, argReg2, argReg3, L3, operation);
        address++;
        tCode.add("STR R0 " + iCode.getResult() + " ; Set FALSE");
        address++;
        tCode.add("JMP " + L4.peek());
        address++;
        tCode.add(L3 + " STR R1 " + iCode.getResult() + " ; Set TRUE");
        address++;

        freeResource(argReg1);
        freeResource(argReg2);
        freeResource(argReg3);
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
        String argReg1 = getRegister(iCode.getArg1());
        String argReg2 = getRegister(iCode.getArg2());
        String argReg3 = getRegister(iCode.getResult());

        if (iCode.getLabel().equals("")) {
            if (!L4.isEmpty()) {
                tCode.add(L4.pop() + " LDR " + argReg1 + " " + iCode.getArg1());
            } else {
                tCode.add("LDR " + argReg1 + " " + iCode.getArg1());
            }
        } else {
            tCode.add(setLabel(iCode.getLabel()) + " LDR " + argReg1 + " " + iCode.getArg1());
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
        address++;

        freeResource(argReg1);
        freeResource(argReg2);
        freeResource(argReg3);
    }

    private void addWriteInstruction(ICode iCode, String operation) {
        String argReg1 = getRegister(iCode.getArg1());
        if (iCode.getLabel().equals("")) {
            if (!L4.isEmpty()) {
                tCode.add(L4.pop() + " LDR " + argReg1 + " " + iCode.getArg1());
            } else {
                tCode.add("LDR " + argReg1 + " " + iCode.getArg1());
            }
        } else {
            tCode.add(setLabel(iCode.getLabel()) + " LDR " + argReg1 + " " + iCode.getArg1());
        }
        address++;

        if (operation.equals(ICodeOprConst.WRTC_OPR.getKey())) {
            tCode.add("TRP 3 " + iCode.getComment());
        } else {
            tCode.add("TRP 1 " + iCode.getComment());
        }
        address++;

        freeResource(argReg1);
    }

    private void addVariables() {
        tCode.add("CLR .INT 0");

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

        for (int i = 0; i < SB; i++) {
            tCode.add(ACTIVATION_RECORD + i + " .INT 0");
        }

        for (int i = 0; i < SB; i++) {
            tCode.add(RETURN_ADDRESS + i + " .INT 0");
        }
    }

    private void mathOpr(ICode iCode, String opr) {
        String argReg1 = getRegister(iCode.getArg1());
        String argReg2 = getRegister(iCode.getArg2());

        if (iCode.getLabel().equals("")) {
            if (!L4.isEmpty()) {
                tCode.add(L4.pop() + " LDR " + argReg1 + " " + iCode.getArg1());
            } else {
                tCode.add("LDR " + argReg1 + " " + iCode.getArg1());
            }
        } else {
            tCode.add(setLabel(iCode.getLabel()) + " LDR " + argReg1 + " " + iCode.getArg1());
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

    private Symbol getSymbol(String name, String scope) {
        String searchScope = scope;
        while (!searchScope.equals("g.")) {
            for (Symbol temp : symbolTable.values()) {
                if (!temp.getScope().equals(searchScope)) {
                    continue;
                }

                if (scope.contains(temp.getScope()) && temp.getValue().equals(name)) {
                    return temp;
                }
            }
            searchScope = decrementScope(searchScope);
        }

        if (searchScope.equals("g.") && name.equals("main")) {
            for (Symbol temp : symbolTable.values()) {
                if (temp.getValue().equals(name)) {
                    return temp;
                }
            }
        }

        return null;
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
