/**
 * Created by IntelliJ IDEA.
 * User: nmalloch
 * Date: 1/8/13
 * Time: 11:39 AM
 */
public class ProgramDriver {
    public static void main(String[] args) {
        LexicalAnalyzer lexicalAnalyzer = new LexicalAnalyzer();
        lexicalAnalyzer.processFile("lexicalTest.kxi");

        SyntaxAnalyzer syntaxAnalyzer = new SyntaxAnalyzer(lexicalAnalyzer);

        try {
            syntaxAnalyzer.evaluate();
        } catch (IllegalArgumentException e) {
            System.out.print(e.getMessage());
            System.exit(0);
        }
        System.out.println("Success");
    }
}
