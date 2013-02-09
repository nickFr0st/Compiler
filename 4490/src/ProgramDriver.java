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
        lexicalAnalyzer.processFile("testFile.kxi");

        SyntaxAndSemanticAnalyzer syntaxAnalyzer = new SyntaxAndSemanticAnalyzer(lexicalAnalyzer);
        syntaxAnalyzer.evaluate(symbolTable);

        for (String s : symbolTable.keySet()) {
            System.out.println(symbolTable.get(s).getValue() + " " + symbolTable.get(s).getScope() + " " + symbolTable.get(s).getSymId() );
        }

        System.out.println("Success");
    }
}
