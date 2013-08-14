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
        lexicalAnalyzer = new LexicalAnalyzer();
        lexicalAnalyzer.setLexicalList(lexicalList);

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

    @Test
    public void testAssignmentExpressionUnknown() {
        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        Tuple t1 = new Tuple(":[", "UNKNOWN", 1);

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalList.add(t1);
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertFalse(compiler.assignment_expression());
        assertEquals(t1, compiler.getLexicalAnalyzer().getToken());
        assertEquals("Unknown Symbol on Line: 1\n", compiler.getErrorList());
    }

    @Test
    public void testAssignmentExpressionNewUnknown() {
        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        Tuple t1 = new Tuple("new", KeyConst.NEW.getKey(), 1);
        Tuple t2 = new Tuple(":[", "UNKNOWN", 1);

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalList.add(t1);
        lexicalList.add(t2);
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertFalse(compiler.assignment_expression());
        assertEquals(t2, compiler.getLexicalAnalyzer().getToken());
        assertEquals("Unknown Symbol on Line: 1\n", compiler.getErrorList());
    }

    @Test
    public void testAssignmentExpressionNewNullTuple() {
        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        Tuple t1 = new Tuple("new", KeyConst.NEW.getKey(), 1);
        Tuple t2 = new NullTuple();

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalList.add(t1);
        lexicalList.add(t2);
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertFalse(compiler.assignment_expression());
        assertEquals(t2, compiler.getLexicalAnalyzer().getToken());
        assertEquals("Illegal new operation. Invalid type. Line: 1\n", compiler.getErrorList());
    }

    @Test
    public void testAssignmentExpressionNewBadType() {
        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        Tuple t1 = new Tuple("new", KeyConst.NEW.getKey(), 1);
        Tuple t2 = new Tuple("hey you", "MATH_OPR", 1);

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalList.add(t1);
        lexicalList.add(t2);
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertFalse(compiler.assignment_expression());
        assertEquals(t2, compiler.getLexicalAnalyzer().getToken());
        assertEquals("Illegal new operation. Invalid type. Line: 1\n", compiler.getErrorList());
    }

    @Test
    public void testAssignmentExpressionNewTypeUnknown() {
        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        Tuple t1 = new Tuple("new", KeyConst.NEW.getKey(), 1);
        Tuple t2 = new Tuple("Field", KeyConst.CLASS_NAME.getKey(), 1);
        Tuple t3 = new Tuple("{e", "UNKNOWN", 1);

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalList.add(t1);
        lexicalList.add(t2);
        lexicalList.add(t3);
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertFalse(compiler.assignment_expression());
        assertEquals(t3, compiler.getLexicalAnalyzer().getToken());
        assertEquals("Unknown Symbol on Line: 1\n", compiler.getErrorList());
    }

    @Test
    public void testAssignmentExpressionNewTypeNullTuple() {
        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        Tuple t1 = new Tuple("new", KeyConst.NEW.getKey(), 1);
        Tuple t2 = new Tuple("Field", KeyConst.CLASS_NAME.getKey(), 1);
        Tuple t3 = new NullTuple();

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalList.add(t1);
        lexicalList.add(t2);
        lexicalList.add(t3);
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertFalse(compiler.assignment_expression());
        assertEquals(t3, compiler.getLexicalAnalyzer().getToken());
        assertEquals("Illegal new operation. Line: 1\n", compiler.getErrorList());
    }

    @Test
    public void testAssignmentExpressionNewTypeBadNewDeclaration() {
        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        Tuple t1 = new Tuple("new", KeyConst.NEW.getKey(), 1);
        Tuple t2 = new Tuple("Field", KeyConst.CLASS_NAME.getKey(), 1);
        Tuple t3 = new Tuple("=", LexicalAnalyzer.tokenTypesEnum.ASSIGNMENT_OPR.name(), 1);

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalList.add(t1);
        lexicalList.add(t2);
        lexicalList.add(t3);
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertFalse(compiler.assignment_expression());
        assertEquals(t3, compiler.getLexicalAnalyzer().getToken());
        assertEquals("Illegal new operation. Line: 1\n", compiler.getErrorList());
    }

    @Test
    public void testAssignmentExpressionNewGood() {
        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        Tuple t1 = new Tuple("new", KeyConst.NEW.getKey(), 1);
        Tuple t2 = new Tuple("Field", KeyConst.CLASS_NAME.getKey(), 1);
        Tuple t3 = new Tuple("(", LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name(), 1);
        Tuple t4 = new Tuple(")", LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name(), 1);
        Tuple t5 = new Tuple(";", LexicalAnalyzer.tokenTypesEnum.EOT.name(), 1);

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalList.add(t1);
        lexicalList.add(t2);
        lexicalList.add(t3);
        lexicalList.add(t4);
        lexicalList.add(t5);
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertTrue(compiler.assignment_expression());
        assertEquals(t5, compiler.getLexicalAnalyzer().getToken());
        assertTrue(compiler.getErrorList().isEmpty());
    }

    @Test
    public void testAssignmentExpressionThis() {
        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        Tuple t1 = new Tuple("this", KeyConst.THIS.getKey(), 1);
        Tuple t2 = new Tuple(";", LexicalAnalyzer.tokenTypesEnum.EOT.name(), 1);

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalList.add(t1);
        lexicalList.add(t2);
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertTrue(compiler.assignment_expression());
        assertEquals(t2, compiler.getLexicalAnalyzer().getToken());
    }

    @Test
    public void testAssignmentExpressionAtoiUnknown() {
        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        Tuple t1 = new Tuple("atoi", KeyConst.ATOI.getKey(), 1);
        Tuple t2 = new Tuple(":[", "UNKNOWN", 1);

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalList.add(t1);
        lexicalList.add(t2);
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertFalse(compiler.assignment_expression());
        assertEquals(t2, compiler.getLexicalAnalyzer().getToken());
        assertEquals("Unknown Symbol on Line: 1\n", compiler.getErrorList());
    }

    @Test
    public void testAssignmentExpressionAtoiNullTuple() {
        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        Tuple t1 = new Tuple("atoi", KeyConst.ATOI.getKey(), 1);
        Tuple t2 = new NullTuple();

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalList.add(t1);
        lexicalList.add(t2);
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertFalse(compiler.assignment_expression());
        assertEquals(t2, compiler.getLexicalAnalyzer().getToken());
        assertEquals("Illegal atoi operation. Missing opening parenthesis. Line: 1\n", compiler.getErrorList());
    }

    @Test
    public void testAssignmentExpressionAtoiNoOpenParenthesis() {
        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        Tuple t1 = new Tuple("atoi", KeyConst.ATOI.getKey(), 1);
        Tuple t2 = new Tuple("hey you", "MATH_OPR", 1);

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalList.add(t1);
        lexicalList.add(t2);
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertFalse(compiler.assignment_expression());
        assertEquals(t2, compiler.getLexicalAnalyzer().getToken());
        assertEquals("Illegal atoi operation. Missing opening parenthesis. Line: 1\n", compiler.getErrorList());
    }

    @Test
    public void testAssignmentExpressionAtoiOpenParanUnknown() {
        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        Tuple t1 = new Tuple("atoi", KeyConst.ATOI.getKey(), 1);
        Tuple t2 = new Tuple("(", LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name(), 1);
        Tuple t3 = new Tuple("{e", "UNKNOWN", 1);

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalList.add(t1);
        lexicalList.add(t2);
        lexicalList.add(t3);
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertFalse(compiler.assignment_expression());
        assertEquals(t3, compiler.getLexicalAnalyzer().getToken());
        assertEquals("Unknown Symbol on Line: 1\n", compiler.getErrorList());
    }

    @Test
    public void testAssignmentExpressionAtoiOpenParanNullTuple() {
        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        Tuple t1 = new Tuple("atoi", KeyConst.ATOI.getKey(), 1);
        Tuple t2 = new Tuple("(", LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name(), 1);
        Tuple t3 = new NullTuple();

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalList.add(t1);
        lexicalList.add(t2);
        lexicalList.add(t3);
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertFalse(compiler.assignment_expression());
        assertEquals(t3, compiler.getLexicalAnalyzer().getToken());
        assertEquals("Illegal atoi operation. Missing closing parenthesis. Line: 1\n", compiler.getErrorList());
    }

    @Test
    public void testAssignmentExpressionAtoiOpenParanBadExpression() {
        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        Tuple t1 = new Tuple("atoi", KeyConst.ATOI.getKey(), 1);
        Tuple t2 = new Tuple("(", LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name(), 1);
        Tuple t3 = new Tuple(";", LexicalAnalyzer.tokenTypesEnum.EOT.name(), 1);

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalList.add(t1);
        lexicalList.add(t2);
        lexicalList.add(t3);
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertFalse(compiler.assignment_expression());
        assertEquals(t3, compiler.getLexicalAnalyzer().getToken());
        assertTrue(compiler.getErrorList().contains("Illegal atoi operation. Invalid Expression. Line: 1\n"));
    }

    @Test
    public void testAssignmentExpressionAtoiOpenParanExpressionNullTuple() {
        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        Tuple t1 = new Tuple("atoi", KeyConst.ATOI.getKey(), 1);
        Tuple t2 = new Tuple("(", LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name(), 1);
        Tuple t3 = new Tuple("true", KeyConst.TRUE.getKey(), 1);
        Tuple t4 = new NullTuple();

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalList.add(t1);
        lexicalList.add(t2);
        lexicalList.add(t3);
        lexicalList.add(t4);
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertFalse(compiler.assignment_expression());
        assertEquals(t4, compiler.getLexicalAnalyzer().getToken());
        assertEquals("Illegal atoi operation. atoi can only contain one parameter. Missing closing parenthesis. Line: 1\n", compiler.getErrorList());
    }

    @Test
    public void testAssignmentExpressionAtoiOpenParanExpressionNoClosingParan() {
        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        Tuple t1 = new Tuple("atoi", KeyConst.ATOI.getKey(), 1);
        Tuple t2 = new Tuple("(", LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name(), 1);
        Tuple t3 = new Tuple("true", KeyConst.TRUE.getKey(), 1);
        Tuple t4 = new Tuple("5", LexicalAnalyzer.tokenTypesEnum.NUMBER.name(), 1);

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalList.add(t1);
        lexicalList.add(t2);
        lexicalList.add(t3);
        lexicalList.add(t4);
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertFalse(compiler.assignment_expression());
        assertEquals(t4, compiler.getLexicalAnalyzer().getToken());
        assertEquals("Illegal atoi operation. atoi can only contain one parameter. Missing closing parenthesis. Line: 1\n", compiler.getErrorList());
    }

    @Test
    public void testAssignmentExpressionAtoiGood() {
        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        Tuple t1 = new Tuple("atoi", KeyConst.ATOI.getKey(), 1);
        Tuple t2 = new Tuple("(", LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name(), 1);
        Tuple t3 = new Tuple("true", KeyConst.TRUE.getKey(), 1);
        Tuple t4 = new Tuple(")", LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name(), 1);
        Tuple t5 = new Tuple(";", LexicalAnalyzer.tokenTypesEnum.EOT.name(), 1);

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalList.add(t1);
        lexicalList.add(t2);
        lexicalList.add(t3);
        lexicalList.add(t4);
        lexicalList.add(t5);
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertTrue(compiler.assignment_expression());
        assertTrue(compiler.getErrorList().isEmpty());
        assertEquals(t5, compiler.getLexicalAnalyzer().getToken());
    }

    @Test
    public void testAssignmentExpressionItoaUnknown() {
        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        Tuple t1 = new Tuple("itoa", KeyConst.ITOA.getKey(), 1);
        Tuple t2 = new Tuple(":[", "UNKNOWN", 1);

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalList.add(t1);
        lexicalList.add(t2);
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertFalse(compiler.assignment_expression());
        assertEquals(t2, compiler.getLexicalAnalyzer().getToken());
        assertEquals("Unknown Symbol on Line: 1\n", compiler.getErrorList());
    }

    @Test
    public void testAssignmentExpressionItoaNullTuple() {
        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        Tuple t1 = new Tuple("itoa", KeyConst.ITOA.getKey(), 1);
        Tuple t2 = new NullTuple();

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalList.add(t1);
        lexicalList.add(t2);
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertFalse(compiler.assignment_expression());
        assertEquals(t2, compiler.getLexicalAnalyzer().getToken());
        assertEquals("Illegal itoa operation. Missing opening parenthesis. Line: 1\n", compiler.getErrorList());
    }

    @Test
    public void testAssignmentExpressionItoaNoOpenParenthesis() {
        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        Tuple t1 = new Tuple("itoa", KeyConst.ITOA.getKey(), 1);
        Tuple t2 = new Tuple("hey you", "MATH_OPR", 1);

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalList.add(t1);
        lexicalList.add(t2);
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertFalse(compiler.assignment_expression());
        assertEquals(t2, compiler.getLexicalAnalyzer().getToken());
        assertEquals("Illegal itoa operation. Missing opening parenthesis. Line: 1\n", compiler.getErrorList());
    }

    @Test
    public void testAssignmentExpressionItoaOpenParanUnknown() {
        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        Tuple t1 = new Tuple("itoa", KeyConst.ITOA.getKey(), 1);
        Tuple t2 = new Tuple("(", LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name(), 1);
        Tuple t3 = new Tuple("{e", "UNKNOWN", 1);

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalList.add(t1);
        lexicalList.add(t2);
        lexicalList.add(t3);
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertFalse(compiler.assignment_expression());
        assertEquals(t3, compiler.getLexicalAnalyzer().getToken());
        assertEquals("Unknown Symbol on Line: 1\n", compiler.getErrorList());
    }

    @Test
    public void testAssignmentExpressionItoaOpenParanNullTuple() {
        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        Tuple t1 = new Tuple("itoa", KeyConst.ITOA.getKey(), 1);
        Tuple t2 = new Tuple("(", LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name(), 1);
        Tuple t3 = new NullTuple();

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalList.add(t1);
        lexicalList.add(t2);
        lexicalList.add(t3);
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertFalse(compiler.assignment_expression());
        assertEquals(t3, compiler.getLexicalAnalyzer().getToken());
        assertEquals("Illegal itoa operation. Missing closing parenthesis. Line: 1\n", compiler.getErrorList());
    }

    @Test
    public void testAssignmentExpressionItoaOpenParanBadExpression() {
        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        Tuple t1 = new Tuple("itoa", KeyConst.ITOA.getKey(), 1);
        Tuple t2 = new Tuple("(", LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name(), 1);
        Tuple t3 = new Tuple(";", LexicalAnalyzer.tokenTypesEnum.EOT.name(), 1);

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalList.add(t1);
        lexicalList.add(t2);
        lexicalList.add(t3);
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertFalse(compiler.assignment_expression());
        assertEquals(t3, compiler.getLexicalAnalyzer().getToken());
        assertTrue(compiler.getErrorList().contains("Illegal itoa operation. Invalid Expression. Line: 1\n"));
    }

    @Test
    public void testAssignmentExpressionItoaOpenParanExpressionNullTuple() {
        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        Tuple t1 = new Tuple("itoa", KeyConst.ITOA.getKey(), 1);
        Tuple t2 = new Tuple("(", LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name(), 1);
        Tuple t3 = new Tuple("true", KeyConst.TRUE.getKey(), 1);
        Tuple t4 = new NullTuple();

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalList.add(t1);
        lexicalList.add(t2);
        lexicalList.add(t3);
        lexicalList.add(t4);
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertFalse(compiler.assignment_expression());
        assertEquals(t4, compiler.getLexicalAnalyzer().getToken());
        assertEquals("Illegal itoa operation. itoa can only contain one parameter. Missing closing parenthesis. Line: 1\n", compiler.getErrorList());
    }

    @Test
    public void testAssignmentExpressionItoaOpenParanExpressionNoClosingParan() {
        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        Tuple t1 = new Tuple("itoa", KeyConst.ITOA.getKey(), 1);
        Tuple t2 = new Tuple("(", LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name(), 1);
        Tuple t3 = new Tuple("true", KeyConst.TRUE.getKey(), 1);
        Tuple t4 = new Tuple("5", LexicalAnalyzer.tokenTypesEnum.NUMBER.name(), 1);

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalList.add(t1);
        lexicalList.add(t2);
        lexicalList.add(t3);
        lexicalList.add(t4);
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertFalse(compiler.assignment_expression());
        assertEquals(t4, compiler.getLexicalAnalyzer().getToken());
        assertEquals("Illegal itoa operation. itoa can only contain one parameter. Missing closing parenthesis. Line: 1\n", compiler.getErrorList());
    }

    @Test
    public void testAssignmentExpressionItoaGood() {
        ArrayList<Tuple> lexicalList = new ArrayList<Tuple>();
        Tuple t1 = new Tuple("itoa", KeyConst.ITOA.getKey(), 1);
        Tuple t2 = new Tuple("(", LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name(), 1);
        Tuple t3 = new Tuple("true", KeyConst.TRUE.getKey(), 1);
        Tuple t4 = new Tuple(")", LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name(), 1);
        Tuple t5 = new Tuple(";", LexicalAnalyzer.tokenTypesEnum.EOT.name(), 1);

        LexicalAnalyzer lexicalAnalyzer1 = new LexicalAnalyzer();
        lexicalList.add(t1);
        lexicalList.add(t2);
        lexicalList.add(t3);
        lexicalList.add(t4);
        lexicalList.add(t5);
        lexicalAnalyzer1.setLexicalList(lexicalList);

        compiler = new Compiler(lexicalAnalyzer1);
        assertTrue(compiler.assignment_expression());
        assertTrue(compiler.getErrorList().isEmpty());
        assertEquals(t5, compiler.getLexicalAnalyzer().getToken());
    }
}
