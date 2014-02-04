package project;

import java.lang.*;

/**
 * Created by IntelliJ IDEA.
 * User: nmalloch
 * Date: 1/8/13
 * Time: 11:39 AM
 */
public class ProgramDriver {
    public static void main(String[] args) {
        String fileName;
        boolean testing = false;

        if (args.length == 0) {
            fileName = "t1.kxi";
            testing = true;
        } else {
            fileName = args[0];
        }

        LexicalAnalyzer lexicalAnalyzer = new LexicalAnalyzer();
        lexicalAnalyzer.processFile(fileName);

        Compiler compiler = new Compiler(lexicalAnalyzer);
        compiler.evaluate(testing);
    }
}
