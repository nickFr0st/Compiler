package project;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 10/13/12
 * Time: 3:29 PM
 */
public class Instruction {
    private int opCode;
    private String opd1;
    private String opd2;
    private String label;

    public Instruction() {
    }

    public Instruction(int opCode, String opd1, String opd2, String label) {
        this.opCode = opCode;
        this.opd1 = opd1;
        this.opd2 = opd2;
        this.label = label;
    }

    public Instruction(int opCode, String opd1, String opd2) {
        this.opCode = opCode;
        this.opd1 = opd1;
        this.opd2 = opd2;
    }

    public Instruction(int opCode, String opd1) {
        this.opCode = opCode;
        this.opd1 = opd1;
    }

    public String getOpd2() {
        return opd2;
    }

    public void setOpd2(String opd2) {
        this.opd2 = opd2;
    }

    public String getOpd1() {
        return opd1;
    }

    public void setOpd1(String opd1) {
        this.opd1 = opd1;
    }

    public int getOpCode() {
        return opCode;
    }

    public void setOpCode(int opCode) {
        this.opCode = opCode;
    }

    public String getLabel() {
        return label;
    }

    public void setLabel(String label) {
        this.label = label;
    }
}
