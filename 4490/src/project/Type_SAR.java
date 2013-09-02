package project;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 8/31/13
 * Time: 10:11 AM
 */
public class Type_SAR extends SAR {
    String name;

    public Type_SAR(Tuple lexi, String scope) {
        super(lexi, scope);
        name = lexi.getName();
    }

    public String getName() {
        return name;
    }
}
