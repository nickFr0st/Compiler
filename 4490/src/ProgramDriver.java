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

        while (lexicalAnalyzer.hasNext()) {
            Tuple<String, String,Integer> temp = lexicalAnalyzer.getNext();
            System.out.printf("%-15s %s %n", temp.lexi, temp.type);
        }
    }
}
