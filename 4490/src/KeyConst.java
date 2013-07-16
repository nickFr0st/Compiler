/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 7/15/13
 * Time: 10:28 PM
 */
public enum KeyConst {
    ATOI("atoi"),
    BOOL("bool"),
    CLASS("class"),
    CHAR("char"),
    CIN("cin"),
    COUT("cout"),
    ELSE("else"),
    FALSE("false"),
    IF("if"),
    INT("int"),
    ITOA("itoa"),
    MAIN("main"),
    NEW("new"),
    NULL("null"),
    OBJECT("object"),
    PUBLIC("public"),
    PRIVATE("private"),
    RETURN("return"),
    STRING("string"),
    THIS("this"),
    TRUE("true"),
    VOID("void"),
    WHILE("while"),
    CLASS_NAME("class_name");

    private String key;

    private KeyConst(String key) {
        this.key = key;
    }

    public String getKey() {
        return key;
    }

    public void setKey(String key) {
        this.key = key;
    }
}
