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
    Symbol symbol;
    String scope = "g.Cat.run";
    String symId = "L106";
    String value = "speed";
    String kind = "lvar";

    @Before
    public void setUp() throws Exception {
        symbol = new Symbol(scope, symId, value, kind, null);
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
}
