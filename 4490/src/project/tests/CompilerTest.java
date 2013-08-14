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
    public void testExpressionzUnknown() {
        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        Tuple t1 = new Tuple(":[", "UNKNOWN", 1);
        Tuple t2 = new Tuple("=", "ASSIGNMENT_OPR", 1);

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalList.add(t1);
        lexicalList.add(t2);
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertFalse(compiler.expressionz());
        assertEquals(t1, compiler.getLexicalAnalyzer().getToken());
        assertEquals("Unknown Symbol on Line: 1\n", compiler.getErrorList());
    }

    @Test
    public void testExpressionzInvalidExpression() {
        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        Tuple t1 = new Tuple("a", "CHARACTER", 1);
        Tuple t2 = new Tuple("=", "ASSIGNMENT_OPR", 1);

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalList.add(t1);
        lexicalList.add(t2);
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertFalse(compiler.expressionz());
        assertEquals(t1, compiler.getLexicalAnalyzer().getToken());
        assertEquals("Invalid expressionz. Line: 1\n", compiler.getErrorList());
    }

    @Test
    public void testExpressionzUnknownTwo() {
        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        Tuple t1 = new Tuple("+", "MATH_OPR", 0);
        Tuple t2 = new Tuple("[Y", "UNKNOWN", 1);

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalList.add(t1);
        lexicalList.add(t2);
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertFalse(compiler.expressionz());
        assertEquals(t1, compiler.getLexicalAnalyzer().getToken());
        assertEquals("Unknown Symbol on Line: 1\n", compiler.getErrorList());
    }

    @Test
    public void testExpressionzNullTuple() {
        Tuple t1 = new Tuple("+", "MATH_OPR", 1);
        Tuple t2 = new NullTuple();

        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        lexicalList.add(t1);
        lexicalList.add(t2);

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertFalse(compiler.expressionz());
        assertEquals(t1, compiler.getLexicalAnalyzer().getToken());
        assertEquals("expressionz missing right hand expression. Line: 1\n", compiler.getErrorList());
    }

    @Test
    public void testExpressionzBadAssignment() {
        Tuple t1 = new Tuple("=", "ASSIGNMENT_OPR", 1);
        Tuple t2 = new Tuple("+", "MATH_OPR", 1);

        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        lexicalList.add(t1);
        lexicalList.add(t2);

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertFalse(compiler.expressionz());
        assertTrue(compiler.getErrorList().contains("Invalid assignment expression. Line: 1\n"));
    }

    @Test
    public void testExpressionzBadExpression() {
        Tuple t1 = new Tuple("+", "MATH_OPR", 1);
        Tuple t2 = new Tuple("=", "ASSIGNMENT_OPR", 1);

        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        lexicalList.add(t1);
        lexicalList.add(t2);

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertFalse(compiler.expressionz());
        assertTrue(compiler.getErrorList().contains("Invalid expressionz expression. Line: 1\n"));
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
    public void testExpression() {
        assertTrue(compiler.expression());
    }

    @Test
    public void testExpressionNull() {
        compiler = new Compiler(new LexicalAnalyzer());
        assertFalse(compiler.expression());
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
