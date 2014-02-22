package project.tests;

import org.junit.Before;
import org.junit.Test;
import project.Symbol;

import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertNull;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 8/29/13
 * Time: 11:31 PM
 */
public class SymbolTest {
    private Symbol symbol;
    private String scope = "g.Cat.run";
    private String symId = "L106";
    private String value = "speed";
    private String kind = "lvar";
    private Integer size = 6;

    @Before
    public void setUp() throws Exception {
        symbol = new Symbol(scope, symId, value, kind, null, size);
    }

    @Test
    public void testGetScope() throws Exception {
        assertEquals(scope, symbol.getScope());
    }

    @Test
    public void testGetSymId() throws Exception {
        assertEquals(symId, symbol.getSymId());
    }

    @Test
    public void testGetValue() throws Exception {
        assertEquals(value, symbol.getValue());
        String newValue = "newValue";
        symbol.setValue(newValue);
        assertEquals(newValue, symbol.getValue());
    }

    @Test
    public void testGetKind() throws Exception {
        assertEquals(kind, symbol.getKind());
    }

    @Test
    public void testGetData() throws Exception {
        assertNull(symbol.getData());
    }

    @Test
    public void testGetSize() throws Exception {
        assertEquals(size, symbol.getSize());
    }
}
