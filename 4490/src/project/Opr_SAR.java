package project;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 8/31/13
 * Time: 10:43 AM
 */
public class Opr_SAR extends SAR {
    private int precedence;
    private String type;

    public Opr_SAR(Tuple lexi) {
        super(lexi);
        type = lexi.getType();
    }

    public int getPrecedence() {
        return precedence;
    }

    public void setPrecedence(int precedence) {
        this.precedence = precedence;
    }

    @Override
    public String getType() {
        return type;
    }
}
