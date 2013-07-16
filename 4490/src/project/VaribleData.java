package project;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 2/9/13
 * Time: 3:33 PM
 */
public class VaribleData implements IData {
    private String type;
    private String accessMod;

    public VaribleData(String type, String accessMod) {
        this.type = type;
        this.accessMod = accessMod;
    }

    public String getAccessMod() {
        return accessMod;
    }

    public String getType() {
        return type;
    }
}
