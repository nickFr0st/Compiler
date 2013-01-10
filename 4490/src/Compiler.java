import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.StringTokenizer;

/**
 * Created by IntelliJ IDEA.
 * User: nmalloch
 * Date: 1/8/13
 * Time: 12:05 PM
 */
public class Compiler {
    private List<String> keyWords;

    public enum tokenTypesEnum {
        NUMBER,
        CHARACTER,
        IDENTIFIER,
        PUNCTUATION,
        KEYWORD,
        MATH_OPR,
        RELATIONAL_OPR,
        BOOLEAN_OPR,
        ASSIGNMENT_OPR,
        ARRAY_BEGIN,
        ARRAY_END,
        BLOCK_BEGIN,
        BLOCK_END,
        PAREN_OPEN,
        PAREN_CLOSE,
        UNKNOWN,
        EOF,
        EOT
    }

    protected void setKeyWords() {
        keyWords = new ArrayList<String>();
        keyWords.add("atoi");
        keyWords.add("bool");
        keyWords.add("class");
        keyWords.add("char");
        keyWords.add("cin");
        keyWords.add("cout");
        keyWords.add("else");
        keyWords.add("false");
        keyWords.add("if");
        keyWords.add("int");
        keyWords.add("itoa");
        keyWords.add("main");
        keyWords.add("new");
        keyWords.add("null");
        keyWords.add("object");
        keyWords.add("public");
        keyWords.add("private");
        keyWords.add("return");
        keyWords.add("string");
        keyWords.add("this");
        keyWords.add("true");
        keyWords.add("void");
        keyWords.add("while");
    }

    public void runCompiler(String fileName) {
        setKeyWords();
        BufferedReader file = null;
        int lineCount = 0;

        try {
            file = new BufferedReader(new InputStreamReader(new FileInputStream(fileName)));
        } catch (FileNotFoundException e) {
            System.out.println("Cannot find File");
            System.exit(0);
        }

        String line;

        try {
            while ((line = file.readLine()) != null) {
                if (line.trim().isEmpty()) {
                    continue;
                }

                // check for and remove comments
                if (line.trim().contains("//")) {
                    line = line.trim().substring(0, line.indexOf("//"));
                }

                // after removing comments re-check for an empty string
                if (line.trim().isEmpty()) {
                    continue;
                }

                // break up string by spaces
                StringTokenizer tokenizer = new StringTokenizer(line);

                while (tokenizer.hasMoreTokens()) {
                    String runner = tokenizer.nextElement().toString();
                    if (runner.matches("^[-|\\+]?[0-9]+$")) {
                        System.out.println(runner + ":\t" + tokenTypesEnum.NUMBER);
                    } else if (runner.length() == 1 && runner.matches("^[a-zA-Z]?$")) {
                        System.out.println(runner + ":\t" + tokenTypesEnum.CHARACTER);
                    } else if (runner.substring(0,1).matches("[a-zA-Z]") && runner.length() < 80) {
                        System.out.println(runner + ":\t" + tokenTypesEnum.IDENTIFIER);
                    }

                }
                //TODO: all the work

                lineCount++;
            }
        } catch (IOException e) {
            System.out.println("There was an error reading in the file.  Failed on line " + lineCount);
            System.exit(0);
        }
    }
}
