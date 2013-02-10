import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 2/9/13
 * Time: 3:33 PM
 */
public class FunctionData implements IData {
    private String returnType;
    private List<String> parameters;
    private String accessMod;

    public FunctionData(String accessMod, List<String> parameters, String returnType) {
        this.accessMod = accessMod;
        this.parameters = parameters;
        this.returnType = returnType;
    }

    public String getReturnType() {
        return returnType;
    }

    public List<String> getParameters() {
        return parameters;
    }

    public String getAccessMod() {
        return accessMod;
    }
}
