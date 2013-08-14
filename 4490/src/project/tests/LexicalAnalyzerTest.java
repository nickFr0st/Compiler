package project.tests;

import org.junit.Test;
import org.junit.Before;
import project.LexicalAnalyzer;
import project.Tuple;

import java.util.ArrayList;

import static org.junit.Assert.*;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 7/16/13
 * Time: 1:03 AM
 */
public class LexicalAnalyzerTest {
    LexicalAnalyzer lexicalAnalyzer;

    @Before
    public void setUp() throws Exception {
        lexicalAnalyzer = new LexicalAnalyzer();
    }

    @Test
    public void testLexicalList() {
        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        lexicalList.add(new Tuple("lexi", "type", 12));
        lexicalAnalyzer.setLexicalList(lexicalList);
        assertEquals("lexi", lexicalAnalyzer.getLexicalList().get(0).getLexi());
    }

    @Test
    public void testConstructor() throws Exception {
        lexicalAnalyzer = new LexicalAnalyzer();
        assertNotNull(lexicalAnalyzer.getLexicalList());
        assertEquals(0, lexicalAnalyzer.getLexicalList().size());
        assertEquals(0, lexicalAnalyzer.getLexPtr());
    }

    @Test
    public void testLexPtr() throws Exception {
        int lexPtr = 12;
        lexicalAnalyzer.setLexPtr(lexPtr);
        assertEquals(lexPtr, lexicalAnalyzer.getLexPtr());
    }

    @Test
    public void testSetKeyWords() throws Exception {
        assertNull(lexicalAnalyzer.getKeyWords());

        lexicalAnalyzer.setKeyWords();

        assertNotNull(lexicalAnalyzer.getKeyWords());
        assertTrue(lexicalAnalyzer.getKeyWords().contains("atoi"));
        assertTrue(lexicalAnalyzer.getKeyWords().contains("bool"));
        assertTrue(lexicalAnalyzer.getKeyWords().contains("class"));
        assertTrue(lexicalAnalyzer.getKeyWords().contains("char"));
        assertTrue(lexicalAnalyzer.getKeyWords().contains("cin"));
        assertTrue(lexicalAnalyzer.getKeyWords().contains("cout"));
        assertTrue(lexicalAnalyzer.getKeyWords().contains("else"));
        assertTrue(lexicalAnalyzer.getKeyWords().contains("false"));
        assertTrue(lexicalAnalyzer.getKeyWords().contains("if"));
        assertTrue(lexicalAnalyzer.getKeyWords().contains("itoa"));
        assertTrue(lexicalAnalyzer.getKeyWords().contains("main"));
        assertTrue(lexicalAnalyzer.getKeyWords().contains("new"));
        assertTrue(lexicalAnalyzer.getKeyWords().contains("null"));
        assertTrue(lexicalAnalyzer.getKeyWords().contains("object"));
        assertTrue(lexicalAnalyzer.getKeyWords().contains("public"));
        assertTrue(lexicalAnalyzer.getKeyWords().contains("private"));
        assertTrue(lexicalAnalyzer.getKeyWords().contains("return"));
        assertTrue(lexicalAnalyzer.getKeyWords().contains("string"));
        assertTrue(lexicalAnalyzer.getKeyWords().contains("this"));
        assertTrue(lexicalAnalyzer.getKeyWords().contains("true"));
        assertTrue(lexicalAnalyzer.getKeyWords().contains("void"));
        assertTrue(lexicalAnalyzer.getKeyWords().contains("while"));
    }

    @Test
    public void testSetSymbolCheck() throws Exception {
        assertNull(lexicalAnalyzer.getSymbolCheck());

        lexicalAnalyzer.setSymbolCheck();

        assertNotNull(lexicalAnalyzer.getSymbolCheck());
        assertTrue(lexicalAnalyzer.getSymbolCheck().contains("&&"));
        assertTrue(lexicalAnalyzer.getSymbolCheck().contains("||"));
        assertTrue(lexicalAnalyzer.getSymbolCheck().contains("<<"));
        assertTrue(lexicalAnalyzer.getSymbolCheck().contains(">>"));
        assertTrue(lexicalAnalyzer.getSymbolCheck().contains("\'"));
        assertTrue(lexicalAnalyzer.getSymbolCheck().contains("<="));
        assertTrue(lexicalAnalyzer.getSymbolCheck().contains(">="));
        assertTrue(lexicalAnalyzer.getSymbolCheck().contains("=="));
        assertTrue(lexicalAnalyzer.getSymbolCheck().contains("<"));
        assertTrue(lexicalAnalyzer.getSymbolCheck().contains(">"));
        assertTrue(lexicalAnalyzer.getSymbolCheck().contains("!="));
        assertTrue(lexicalAnalyzer.getSymbolCheck().contains("="));
        assertTrue(lexicalAnalyzer.getSymbolCheck().contains(")"));
        assertTrue(lexicalAnalyzer.getSymbolCheck().contains("("));
        assertTrue(lexicalAnalyzer.getSymbolCheck().contains("["));
        assertTrue(lexicalAnalyzer.getSymbolCheck().contains("]"));
        assertTrue(lexicalAnalyzer.getSymbolCheck().contains("{"));
        assertTrue(lexicalAnalyzer.getSymbolCheck().contains("}"));
        assertTrue(lexicalAnalyzer.getSymbolCheck().contains("\""));
        assertTrue(lexicalAnalyzer.getSymbolCheck().contains(","));
        assertTrue(lexicalAnalyzer.getSymbolCheck().contains("."));
        assertTrue(lexicalAnalyzer.getSymbolCheck().contains("+"));
        assertTrue(lexicalAnalyzer.getSymbolCheck().contains("-"));
        assertTrue(lexicalAnalyzer.getSymbolCheck().contains("/"));
        assertTrue(lexicalAnalyzer.getSymbolCheck().contains("*"));
        assertTrue(lexicalAnalyzer.getSymbolCheck().contains("%"));
        assertTrue(lexicalAnalyzer.getSymbolCheck().contains(";"));
    }
}
