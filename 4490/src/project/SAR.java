package project;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 2/19/13
 * Time: 10:11 PM
 */
public class SAR {
    private Tuple lexi;
    private String scope;
    private String sarId;

    public SAR() {
    }

    public SAR(Tuple lexi, String scope, String sarId) {
        this.sarId = sarId;
        this.lexi = lexi;
        this.scope = scope;
    }

    public String getSarId() {
        return sarId;
    }

    public void setSarId(String key) {
        this.sarId = key;
    }

    public String getScope() {
        return scope;
    }

    public Tuple getLexi() {
        return lexi;
    }
}
