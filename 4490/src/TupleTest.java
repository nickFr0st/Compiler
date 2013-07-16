import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 7/15/13
 * Time: 10:46 PM
 */

public class TupleTest {
    Tuple tuple;

    @Before
    public void setUp() throws Exception {
        tuple = new Tuple();
    }

    @Test
    public void testGetLexi() throws Exception {
        String lexi = "lexi";
        tuple.setLexi(lexi);
        assertEquals(lexi, tuple.getLexi());
    }

    @Test
    public void testGetType() throws Exception {
        int lineNum = 12;
        tuple.setLineNum(lineNum);
        assertEquals(lineNum, tuple.getLineNum());
    }

    @Test
    public void testGetLineNum() throws Exception {
        String type = "type";
        tuple.setType(type);
        assertEquals(type, tuple.getType());
    }
}
