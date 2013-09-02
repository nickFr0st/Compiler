package project;

import java.lang.*;
import java.util.LinkedHashMap;

/**
 * Created by IntelliJ IDEA.
 * User: nmalloch
 * Date: 1/8/13
 * Time: 11:39 AM
 */
public class ProgramDriver {
    public static void main(String[] args) {
        LexicalAnalyzer lexicalAnalyzer = new LexicalAnalyzer();
        lexicalAnalyzer.processFile("t1.kxi");
        Compiler compiler = new Compiler(lexicalAnalyzer);
        compiler.evaluate();
    }
}
