package project;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 7/15/13
 * Time: 10:27 PM
 */
public enum ModifierTypeConst {
    PUBLIC("public"),
    PRIVATE("private");

    private String key;

    private ModifierTypeConst(String key) {
        this.key = key;
    }

    public String getKey() {
        return key;
    }
}
