package project.tests;

import org.junit.Before;
import org.junit.Test;
import project.Tuple;

import static org.junit.Assert.assertEquals;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 7/15/13
 * Time: 10:46 PM
 */

public class TupleTest {
    private Tuple tuple;

    @Before
    public void setUp() throws Exception {
        tuple = new Tuple("l", "t", 1);
    }

    @Test
    public void testGetLexi() throws Exception {
        String lexi = "lexi";
        tuple.setName(lexi);
        assertEquals(lexi, tuple.getName());
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
