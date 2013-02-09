import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 2/9/13
 * Time: 3:33 PM
 */
public class FunctionData implements IData {
    String returnType;
    List<String> parameters;
    String accessMod;

    public FunctionData(String accessMod, List<String> parameters, String returnType) {
        this.accessMod = accessMod;
        this.parameters = parameters;
        this.returnType = returnType;
    }
}
