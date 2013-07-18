package project;

import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;

import static org.junit.Assert.*;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 7/17/13
 * Time: 6:26 PM
 */
public class CompilerTest {
    Compiler compiler;
    LexicalAnalyzer lexicalAnalyzer;

    @Before
    public void setUp() throws Exception {
        lexicalAnalyzer = setupLexicalAnalyzer();
        compiler = new Compiler(lexicalAnalyzer);
    }

    @Test
    public void testExpressionz() {
        assertTrue(compiler.expression());
    }

    private LexicalAnalyzer setupLexicalAnalyzer() {
        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        Tuple t1 = new Tuple("h", "IDENTIFIER", 0);
        Tuple t2 = new Tuple("=", "ASSIGNMENT_OPR", 0);
        Tuple t3 = new Tuple("3", "NUMBER", 0);
        Tuple t4 = new Tuple("+", "MATH_OPR", 0);
        Tuple t5 = new Tuple("9", "NUMBER", 0);
        Tuple t6 = new Tuple(";", "EOT", 0);

        lexicalList.add(t1);
        lexicalList.add(t2);
        lexicalList.add(t3);
        lexicalList.add(t4);
        lexicalList.add(t5);
        lexicalList.add(t6);

        LexicalAnalyzer analyzer = new LexicalAnalyzer();
        analyzer.setLexicalList(lexicalList);

        return analyzer;
    }
}
