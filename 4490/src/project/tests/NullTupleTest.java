package project.tests;

import org.junit.Before;
import org.junit.Test;
import project.NullTuple;
import project.Tuple;

import static org.junit.Assert.assertEquals;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 7/15/13
 * Time: 11:00 PM
 */
public class NullTupleTest {
    private Tuple tuple;

    @Before
    public void setUp() throws Exception {
        tuple = new NullTuple();
    }

    @Test
    public void testNullTuple() throws Exception {
        assertEquals("", tuple.getName());
        assertEquals("", tuple.getType());
        assertEquals(0, tuple.getLineNum());
    }
}
