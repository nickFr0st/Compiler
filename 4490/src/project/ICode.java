package project;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 3/30/13
 * Time: 9:01 PM
 */
public class ICode {
    private String label;
    private String operation;
    private String arg1;
    private String arg2;
    private String result;
    private String comment;

    public ICode(String label, String operation, String arg1, String arg2, String result, String comment) {
        this.label = label;
        this.operation = operation;
        this.arg1 = arg1;
        this.arg2 = arg2;
        this.result = result;
        this.comment = comment;
    }

    public String getLabel() {
        return label;
    }

    public void setLabel(String label) {
        this.label = label;
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

    public String getComment() {
        return comment;
    }

    public void setComment(String comment) {
        this.comment = comment;
    }

    @Override
    public String toString() {
        return "project.ICode{" +
                "label='" + label + '\'' +
                ", operation='" + operation + '\'' +
                ", arg1='" + arg1 + '\'' +
                ", arg2='" + arg2 + '\'' +
                ", result='" + result + '\'' +
                ", comment='" + comment + '\'' +
                '}';
    }
}
