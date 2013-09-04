package project;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 9/3/13
 * Time: 10:58 PM
 */
public class Function_SAR extends SAR {
    private Identifier_SAR function;
    private EAL_SAR arguments;

    public Function_SAR(String scope, Tuple lexi, String type, Identifier_SAR function, EAL_SAR arguments) {
        super(scope, lexi, type);
        this.function = function;
        this.arguments = arguments;
    }

    public Identifier_SAR getFunction() {
        return function;
    }

    public EAL_SAR getArguments() {
        return arguments;
    }
}
