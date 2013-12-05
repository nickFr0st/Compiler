package project;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 1/21/13
 * Time: 7:15 PM
 */
public class Tuple {
    private String name;
    private String type;
    private int lineNum;

    /**
     * used for nullTuple
     */
    public Tuple() {
    }

    public Tuple(String name, String type, int lineNum) {
        this.name = name;
        this.type = type;
        this.lineNum = lineNum;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
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

    public String[] toArray() {
        return new String[]{name, type, Integer.toString(lineNum), "\n"};
    }
}
