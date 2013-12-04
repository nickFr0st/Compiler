package project;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 10/1/13
 * Time: 10:37 PM
 */
public enum ICodeOprConst {
    MOV_OPR("MOV"),
    REF_OPR("REF"),
    ADD_OPR("ADD"),
    ADI_OPR("ADI"),
    SUB_OPR("SUB"),
    MUL_OPR("MUL"),
    DIV_OPR("DIV"),
    LT_OPR("LT"),
    GT_OPR("GT"),
    NE_OPR("NE"),
    EQ_OPR("EQ"),
    LE_OPR("LE"),
    GE_OPR("GE"),
    AND_OPR("AND"),
    OR_OPR("OR"),
    BF_OPR("BF"),
    BT_OPR("BT"),
    JMP_OPR("JMP"),
    WRTI_OPR("WRTI"),
    WRTC_OPR("WRTC"),
    RDI_OPR("RDI"),
    RDC_OPR("RDC"),
    RTN_OPR("RTN"),
    RETURN_OPR("RETURN"),
    FRAME_OPR("FRAME"),
    CALL_OPR("CALL"),
    PEEK_OPR("PEEK"),
    PUSH_OPR("PUSH"),
    SKIP_IF("SKIPIF"),
    SKIP_ELSE("SKIPELSE"),
    WHILE_BEGIN("BEGIN"),
    END_WHILE("ENDWHILE"),
    NEWI_OPR("NEWI"),
    NEW_OPR("NEW"),
    CREATE_OPR("CREATE"),
    FUNC_OPR("FUNC"),
    MOD_OPR("MOD"),
    MOVI_OPR("MOVI");

    private String key;

    private ICodeOprConst(String key) {
        this.key = key;
    }

    public String getKey() {
        return key;
    }
}
