package project;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 8/31/13
 * Time: 10:43 AM
 */
public class Opr_SAR extends SAR {
    private int precedence;

    public Opr_SAR(Tuple lexi) {
        super(lexi);
    }

    public int getPrecedence() {
        return precedence;
    }

    public void setPrecedence(int precedence) {
        this.precedence = precedence;
    }
}
