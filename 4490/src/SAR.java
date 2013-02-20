/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 2/19/13
 * Time: 10:11 PM
 */
public class SAR {
    private Tuple<String, String, Integer> lexi;
    private String scope;

    public SAR(Tuple<String, String, Integer> lexi, String scope) {
        this.lexi = lexi;
        this.scope = scope;
    }

    public String getScope() {
        return scope;
    }

    public Tuple<String, String, Integer> getLexi() {
        return lexi;
    }
}
