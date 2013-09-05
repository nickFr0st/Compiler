package project;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 9/3/13
 * Time: 10:58 PM
 */
public class New_SAR extends SAR {
    private Type_SAR classObj;
    private EAL_SAR arguments;

    public New_SAR(String scope, Tuple lexi, String type, Type_SAR classObj, EAL_SAR arguments) {
        super(scope, lexi, type);
        this.classObj = classObj;
        this.arguments = arguments;
    }

    public Type_SAR getClassObj() {
        return classObj;
    }

    public EAL_SAR getArguments() {
        return arguments;
    }
}
