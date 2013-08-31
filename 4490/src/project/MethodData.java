package project;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 2/9/13
 * Time: 3:33 PM
 */
public class MethodData implements IData {
    private String returnType;
    private List<String> parameters;
    private String accessMod;

    public MethodData(String accessMod, List<String> parameters, String returnType) {
        this.accessMod = accessMod;
        this.parameters = parameters;
        this.returnType = returnType;
    }

    /**
     * this returns the methods return type.
     * @return returnType
     */
    public String getType() {
        return returnType;
    }

    public String getAccessMod() {
        return accessMod;
    }

    public List<String> getParameters() {
        return parameters;
    }

    public void setParameters(List<String> parameters) {
        this.parameters = parameters;
    }
}
