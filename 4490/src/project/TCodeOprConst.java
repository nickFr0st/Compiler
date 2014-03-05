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
    STRI_OPR("STRI"),
    ADDI_OPR("ADDI"),
    LDA_OPR("LDA"),
    BGT_OPR("BGT"),
    JMR_OPR("JMR"),
    TRP_2("TRP 2"),
    TRP_4("TRP 4"),
    BNZ_OPR("BNZ"),
    BRZ_OPR("BRZ"),
    TRP_3("TRP 3"),
    TRP_1("TRP 1");

    private String key;

    private TCodeOprConst(String key) {
        this.key = key;
    }

    public String getKey() {
        return key;
    }
}
