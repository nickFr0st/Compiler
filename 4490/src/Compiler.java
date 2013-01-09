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

    int lineCount = 0;
    HashMap<tokenTypesEnum, List<String>> tokenTypes = new HashMap<tokenTypesEnum, List<String>>();

    public void runCompiler(String fileName) {
        BufferedReader file = null;

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
                List<String> numberList = new ArrayList<String>();
                List<String> charList = new ArrayList<String>();
                List<String> identifierList = new ArrayList<String>();


                while (tokenizer.hasMoreTokens()) {
                    String runner = tokenizer.nextElement().toString();
                    if (runner.matches("^[-|\\+]?[0-9]+$")) {
                        numberList.add(runner);
                        tokenTypes.put(tokenTypesEnum.NUMBER, numberList);
                        System.out.println(tokenTypes.get(tokenTypesEnum.NUMBER).get(tokenTypes.get(tokenTypesEnum.NUMBER).size()-1));
                    } else if (runner.length() == 1 && runner.matches("^[a-zA-Z]?$")) {
                        charList.add(runner);
                        tokenTypes.put(tokenTypesEnum.CHARACTER, charList);
                        System.out.println(tokenTypes.get(tokenTypesEnum.CHARACTER).get(tokenTypes.get(tokenTypesEnum.CHARACTER).size()-1));
                    } else if (runner.substring(0,1).matches("[a-zA-Z]") && runner.length() < 80) {
                        identifierList.add(runner);
                        tokenTypes.put(tokenTypesEnum.IDENTIFIER, identifierList);
                        System.out.println(tokenTypes.get(tokenTypesEnum.IDENTIFIER).get(tokenTypes.get(tokenTypesEnum.IDENTIFIER).size()-1));
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
