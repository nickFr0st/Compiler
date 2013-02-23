/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 2/19/13
 * Time: 10:11 PM
 */
public class SAR {
    private Tuple<String, String, Integer> lexi;
    private String scope;
    private String type;

    public SAR(Tuple<String, String, Integer> lexi, String scope, String type) {
        this.lexi = lexi;
        this.scope = scope;
        this.type = type;
    }

    public String getScope() {
        return scope;
    }

    public Tuple<String, String, Integer> getLexi() {
        return lexi;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }
}
