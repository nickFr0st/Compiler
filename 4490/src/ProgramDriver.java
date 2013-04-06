import java.util.LinkedHashMap;

/**
 * Created by IntelliJ IDEA.
 * User: nmalloch
 * Date: 1/8/13
 * Time: 11:39 AM
 */
public class ProgramDriver {
    public static void main(String[] args) {
        LinkedHashMap<String, Symbol> symbolTable = new LinkedHashMap<String, Symbol>();
        LexicalAnalyzer lexicalAnalyzer = new LexicalAnalyzer();
        lexicalAnalyzer.processFile("test2.kxi");
        Compiler syntaxAnalyzer = new Compiler(lexicalAnalyzer, symbolTable);
        syntaxAnalyzer.evaluate();

        System.out.println("Success");
    }
}
