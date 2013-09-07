package project;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 9/3/13
 * Time: 10:58 PM
 */
public class Array_SAR extends SAR {
    private Identifier_SAR array;
    private Identifier_SAR value;

    public Array_SAR(String scope, Tuple lexi, String type, Identifier_SAR array, Identifier_SAR value) {
        super(scope, lexi, type);
        this.array = array;
        this.value = value;
    }

    public Identifier_SAR getArray() {
        return array;
    }

    public Identifier_SAR getValue() {
        return value;
    }
}
