/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 3/30/13
 * Time: 9:01 PM
 */
public class ICode {
    private String operation;
    private String arg1;
    private String arg2;
    private String result;

    public ICode(String operation, String arg1, String arg2, String result) {
        this.operation = operation;
        this.arg1 = arg1;
        this.arg2 = arg2;
        this.result = result;
    }

    public String getOperation() {
        return operation;
    }

    public void setOperation(String operation) {
        this.operation = operation;
    }

    public String getArg1() {
        return arg1;
    }

    public void setArg1(String arg1) {
        this.arg1 = arg1;
    }

    public String getArg2() {
        return arg2;
    }

    public void setArg2(String arg2) {
        this.arg2 = arg2;
    }

    public String getResult() {
        return result;
    }

    public void setResult(String result) {
        this.result = result;
    }
}
