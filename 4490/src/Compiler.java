import java.io.*;
import java.util.ArrayList;
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
    private List<String> tokenList = new ArrayList<String>();
    private List<String> symbolCheck;

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

    protected void setSymbolCheck() {
        symbolCheck = new ArrayList<String>();
        symbolCheck.add("<");
        symbolCheck.add(">");
        symbolCheck.add(")");
        symbolCheck.add("(");
        symbolCheck.add("[");
        symbolCheck.add("]");
        symbolCheck.add("{");
        symbolCheck.add("}");
        symbolCheck.add(",");
        symbolCheck.add(".");
        symbolCheck.add(";");
        symbolCheck.add(":");
        symbolCheck.add("=");
        symbolCheck.add("+");
        symbolCheck.add("-");
        symbolCheck.add("/");
        symbolCheck.add("*");
    }

    public void runCompiler(String fileName) {
        setKeyWords();
        setSymbolCheck();
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
                // check for empty or pure comment lines
                if (line.trim().isEmpty() || line.trim().startsWith("//")) {
                    continue;
                }

                // check for and remove comments
                if (line.trim().contains("//")) {
                    line = line.trim().substring(0, line.indexOf("//"));
                }

                // break up string by spaces
                String[] tokenizer = line.split("[\\s\t]");

                for (String item : tokenizer) {
                    if (item.trim().isEmpty()) {
                        continue;
                    }

                    int originalListSize = tokenList.size();
                    for (String s : symbolCheck) {
                        while (item.contains(s) && item.length() > 1) {
                            item = breakDownToken(item, s);
                        }
                    }

                    if (originalListSize == tokenList.size()) {
                        tokenList.add(item);
                    }
                }
                lineCount++;
            }

            for (String token : tokenList) {

                if (token.trim().isEmpty()) {
                    continue;
                }

                if (keyWords.contains(token.trim())) {
                    System.out.printf("%-15s %s %n", token, tokenTypesEnum.KEYWORD);
                } else if (token.matches("^[-|\\+]?[0-9]+$")) {
                    System.out.printf("%-15s %s %n", token, tokenTypesEnum.NUMBER);
                } else if (token.length() == 1 && token.matches("^[a-zA-Z]?$")) {
                    System.out.printf("%-15s %s %n", token, tokenTypesEnum.CHARACTER);
                } else if (token.substring(0, 1).matches("[a-zA-Z]") && token.length() < 80) {
                    System.out.printf("%-15s %s %n", token, tokenTypesEnum.IDENTIFIER);
                } else {
                    System.out.printf("%-15s %s %n", token, tokenTypesEnum.UNKNOWN);
                }
            }
        } catch (IOException e) {
            System.out.println("There was an error reading in the file.  Failed on line " + lineCount);
            System.exit(0);
        }
    }

    private String breakDownToken(String item, String breakDownItem) {
        if (item.contains(breakDownItem) && item.trim().length() > 1) {
            tokenList.add(item.trim().substring(0, item.indexOf(breakDownItem)));
            tokenList.add(breakDownItem);
            item = item.trim().substring(item.indexOf(breakDownItem)+1, item.length());
            for (String s : symbolCheck) {
                while (item.contains(s) && item.length() > 1) {
                    item = breakDownToken(item, s);
                }
            }
            tokenList.add(item.substring(item.indexOf(breakDownItem)+1, item.length()));

            return item;
        }
        return item;
    }
}
