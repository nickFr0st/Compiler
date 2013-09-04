package project;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 8/31/13
 * Time: 10:08 AM
 */
public class Identifier_SAR extends SAR {
    public Identifier_SAR(Tuple lexi, String scope) {
        super(lexi, scope);
    }

    public Identifier_SAR(String scope, Tuple lexi, String type) {
        super(scope, lexi, type);
    }

    public void setType(String type) {
        super.setType(type);
    }
}
