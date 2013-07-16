package project.tests;

import org.junit.Before;
import org.junit.Test;
import project.Memory;

import static org.junit.Assert.assertEquals;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 7/15/13
 * Time: 11:08 PM
 */
public class MemoryTest {
    private Memory memory;
    private String label = "label";
    private String data = "data";

    @Before
    public void setUp() throws Exception {
        memory = new Memory(label, data);
    }

    @Test
    public void testGetLabel() throws Exception {
        assertEquals(label, memory.getLabel());

        String newLabel = "newLabel";
        memory.setLabel(newLabel);
        assertEquals(newLabel, memory.getLabel());
    }

    @Test
    public void testGetData() throws Exception {
        assertEquals(data, memory.getData());

        String newData = "newData";
        memory.setData(newData);
        assertEquals(newData, memory.getData());
    }
}
