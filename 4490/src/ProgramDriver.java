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

//        System.out.printf("%-15s %-15s %-15s %-15s", "Type", "Name", "Scope", "Id");
//        System.out.println();
//        for (String s : symbolTable.keySet()) {
//            String s1 = String.format("%-15s %-15s %-15s %-15s", symbolTable.get(s).getKind(), symbolTable.get(s).getValue(), symbolTable.get(s).getScope(), symbolTable.get(s).getSymId());
//            System.out.println(s1);
//        }

        System.out.println("Success");
    }
}
