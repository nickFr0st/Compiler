/**
 * Created by IntelliJ IDEA.
 * User: nmalloch
 * Date: 1/8/13
 * Time: 11:39 AM
 */
public class ProgramDriver {
    public static void main(String[] args) {
        LexicalAnalyzer lexicalAnalyzer = new LexicalAnalyzer();
        lexicalAnalyzer.processFile("testFile.kxi");

        SyntaxAnalyzer syntaxAnalyzer = new SyntaxAnalyzer(lexicalAnalyzer);
        syntaxAnalyzer.evaluate();

        System.out.println("Success");
    }
}
