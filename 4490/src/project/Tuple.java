package project;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 1/21/13
 * Time: 7:15 PM
 */
public class Tuple {
    private String lexi;
    private String type;
    private int lineNum;

    /**
     * used for nullTuple
     */
    public Tuple() {
    }

    public Tuple(String lexi, String type, int lineNum) {
        this.lexi = lexi;
        this.type = type;
        this.lineNum = lineNum;
    }

    public String getLexi() {
        return lexi;
    }

    public void setLexi(String lexi) {
        this.lexi = lexi;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public int getLineNum() {
        return lineNum;
    }

    public void setLineNum(int lineNum) {
        this.lineNum = lineNum;
    }
}
