package project;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 2/9/13
 * Time: 3:33 PM
 */
public class VariableData implements IData {
    private String type;
    private String accessMod;

    public VariableData(String type, String accessMod) {
        this.type = type;
        this.accessMod = accessMod;
    }

    /**
     * this returns the item type.
     * @return type
     */
    public String getType() {
        return type;
    }

    public String getAccessMod() {
        return accessMod;
    }
}
