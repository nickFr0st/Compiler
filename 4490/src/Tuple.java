/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 1/21/13
 * Time: 7:15 PM
 */
public class Tuple<X, Y, Z> {
    public final X lexi;
    public final Y type;
    public final Z lineNum;

    public Tuple(X lexi, Y type, Z lineNum) {
        this.lexi = lexi;
        this.type = type;
        this.lineNum = lineNum;
    }
}
