/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 2/19/13
 * Time: 10:11 PM
 */
public class SAR {
    private Tuple lexi;
    private String scope;
    private String type;
    private String key;

    public SAR(Tuple lexi, String scope, String type, String key) {
        this.key = key;
        this.lexi = lexi;
        this.scope = scope;
        this.type = type;
    }

    public String getKey() {
        return key;
    }

    public void setKey(String key) {
        this.key = key;
    }

    public String getScope() {
        return scope;
    }

    public Tuple getLexi() {
        return lexi;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }
}
