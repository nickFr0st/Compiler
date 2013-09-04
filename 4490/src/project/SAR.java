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
    private String type;

    public SAR() {
    }

    /**
     * used for the Type_SAR
     * @param lexi
     * @param scope
     */
    public SAR(Tuple lexi, String scope) {
        this.lexi = lexi;
        this.scope = scope;
    }

    /**
     * this is used for the Opr_SAR
     * @param lexi
     */
    public SAR(Tuple lexi) {
        this.lexi = lexi;
    }

    public SAR(String type, Tuple lexi) {
        this.type = type;
        this.lexi = lexi;
    }

    public SAR(Tuple lexi, String scope, String sarId, String type) {
        this.sarId = sarId;
        this.lexi = lexi;
        this.scope = scope;
        this.type = type;
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

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }
}
