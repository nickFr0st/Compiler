package project;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 7/15/13
 * Time: 10:25 PM
 */
public class NullTuple extends Tuple {
    private String lexi;
    private String type;
    private int lineNum;

    public NullTuple() {
        lexi = "";
        type = "";
        lineNum = 0;
    }

    public String getName() {
        return lexi;
    }

    public String getType() {
        return type;
    }

    public int getLineNum() {
        return lineNum;
    }
}
