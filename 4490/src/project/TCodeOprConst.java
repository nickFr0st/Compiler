package project;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 10/1/13
 * Time: 10:37 PM
 */
public enum TCodeOprConst {
    MOV_OPR("MOV"),
    REF_OPR("REF"),
    ADD_OPR("ADD"),
    ADI_OPR("ADI"),
    SUB_OPR("SUB"),
    MUL_OPR("MUL"),
    DIV_OPR("DIV"),
    OVERFLOW_LBL("OVERFLOW"),
    UNDERFLOW_LBL("UNDERFLOW"),
    CMP_OPR("CMP"),
    LDR_OPR("LDR"),
    JMP_OPR("JMP"),
    BLT_OPR("BLT"),
    STR_OPR("STR"),
    ADDI_OPR("ADDI"),
    LDA_OPR("LDA");

    private String key;

    private TCodeOprConst(String key) {
        this.key = key;
    }

    public String getKey() {
        return key;
    }
}
