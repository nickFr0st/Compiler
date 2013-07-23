package project.tests;

import org.junit.Before;
import org.junit.Test;
import project.*;
import project.Compiler;

import java.util.ArrayList;

import static org.junit.Assert.*;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 7/17/13
 * Time: 6:26 PM
 */
public class CompilerTest {
    project.Compiler compiler;
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

    @Test
    public void testExpressionzNull() {
        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        Tuple t1 = new Tuple("h", "IDENTIFIER", 0);
        Tuple t2 = new Tuple("=", "ASSIGNMENT_OPR", 0);

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalList.add(t1);
        lexicalList.add(t2);

        compiler = new Compiler(lexicalAnalyzer1);
        assertFalse(compiler.expression());
    }

    @Test
    public void testExpressionzNoneAssignment() {
        Tuple t2 = new Tuple("+", "MATH_OPR", 0);
        Tuple t3 = new Tuple("3", "NUMBER", 0);

        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        lexicalList.add(t2);
        lexicalList.add(t3);

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertTrue(compiler.expressionz());
    }

    @Test
    public void testExpressionzNoneAssignmentFail() {
        Tuple t2 = new Tuple("!=", "LOGICAL_OPR", 0);

        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        lexicalList.add(t2);

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertFalse(compiler.expressionz());
    }

    @Test
    public void testExpressionzBadExpression() {
        Tuple t2 = new Tuple("&&", "BOOLEAN_OPR", 0);
        Tuple t3 = new Tuple("{0", "UNKNOWN", 0);

        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        lexicalList.add(t2);
        lexicalList.add(t3);

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertFalse(compiler.expressionz());
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
