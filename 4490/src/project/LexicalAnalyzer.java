package project;

import java.io.*;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 4/27/13
 * Time: 7:55 AM
 */
public class LexicalAnalyzer {
    private List<String> keyWords;
    private List<String> tokenList;
    private List<String> symbolCheck;

    private List<Tuple> lexicalList;
    private int lexPtr;

    public LexicalAnalyzer() {
        lexicalList = new ArrayList<Tuple>();
        lexPtr = 0;
    }

    public List<Tuple> getLexicalList() {
        return lexicalList;
    }

    public void setLexicalList(List<Tuple> lexicalList) {
        this.lexicalList = lexicalList;
    }

    public int getLexPtr() {
        return lexPtr;
    }

    public void setLexPtr(int lexPtr) {
        this.lexPtr = lexPtr;
    }

    public static enum tokenTypesEnum {
        NUMBER,
        CHARACTER,
        IDENTIFIER,
        PUNCTUATION,
        MATH_OPR,
        BOOLEAN_OPR,
        LOGICAL_OPR,
        ASSIGNMENT_OPR,
        ARRAY_BEGIN,
        ARRAY_END,
        BLOCK_BEGIN,
        BLOCK_END,
        PAREN_OPEN,
        PAREN_CLOSE,
        UNKNOWN,
        EOT,
        IO_OPR
    }

    public List<String> getKeyWords() {
        return keyWords;
    }

    public void setKeyWords() {
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

    public void setSymbolCheck() {
        symbolCheck = new ArrayList<String>();
        // boolean operators
        symbolCheck.add("&&");
        symbolCheck.add("||");

        // IO operators
        symbolCheck.add("<<");
        symbolCheck.add(">>");

        symbolCheck.add("\'");

        // relational operators
        symbolCheck.add("<=");
        symbolCheck.add(">=");
        symbolCheck.add("==");
        symbolCheck.add("<");
        symbolCheck.add(">");
        symbolCheck.add("!=");

        // assignment operator
        symbolCheck.add("=");

        // Parentheses
        symbolCheck.add(")");
        symbolCheck.add("(");

        // Arrays
        symbolCheck.add("[");
        symbolCheck.add("]");

        // Blocks
        symbolCheck.add("{");
        symbolCheck.add("}");

        // punctuation
        symbolCheck.add("\"");
        symbolCheck.add(",");
        symbolCheck.add(".");

        // math operators
        symbolCheck.add("+");
        symbolCheck.add("-");
        symbolCheck.add("/");
        symbolCheck.add("*");
        symbolCheck.add("%");

        // end of line
        symbolCheck.add(";");
    }

    public List<String> getSymbolCheck() {
        return symbolCheck;
    }

    public void processFile(String fileName, boolean printList) {
        setKeyWords();
        setSymbolCheck();
        BufferedReader file = null;
        int lineCount = 1;

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
                    lineCount++;
                    continue;
                }

                // check for and remove comments
                if (line.trim().contains("//")) {
                    String[] tempLine = line.split("//");
                    line = tempLine[0].trim();
                }

                tokenList = new ArrayList<String>();
                // break up string by spaces
                for (int i = 0; i < line.length(); i++) {
                    if (i > 0 && i < line.length()) {
                        if (line.toCharArray()[i] == ' ' && line.toCharArray()[i - 1] == '\'' && line.toCharArray()[i + 1] == '\'') {
                            StringBuilder sb = new StringBuilder(line);
                            sb.setCharAt(i, 'ƒ');
                            line = sb.toString();
                        }
                    }
                }

                String[] tokenizer = line.split("[\\s\\t]");

                for (int j = 0; j < tokenizer.length; j++) {
                    if (tokenizer[j].contains("ƒ")) {
                        for (int i = 0; i < tokenizer[j].length(); i++) {
                            if (i > 0 && i < tokenizer[j].length()) {
                                if (tokenizer[j].toCharArray()[i] == 'ƒ' && tokenizer[j].toCharArray()[i - 1] == '\'' && tokenizer[j].toCharArray()[i + 1] == '\'') {
                                    StringBuilder sb = new StringBuilder(tokenizer[j]);
                                    sb.setCharAt(i, ' ');
                                    tokenizer[j] = sb.toString();
                                }
                            }
                        }
                    }
                }

                for (int i = 0; i < tokenizer.length; i++) {
                    if (tokenizer.length < 1 && tokenizer[1].trim().isEmpty()) {
                        lineCount++;
                        continue;
                    }

                    int originalListSize = tokenList.size();
                    for (String s : symbolCheck) {
                        while (tokenizer[i].contains(s) && tokenizer[i].length() > 0) {
                            if (s.equals("'")) {
                                s = checkForChars(tokenizer[i], s);
                            }

                            if (s.equals("-") && tokenizer[i].startsWith("-") && tokenizer[i].length() > 1) {
                                String breaker = tokenizer[i];
                                if (tokenizer[i].endsWith(";")) {
                                    breaker = breaker.substring(0, breaker.length() - 1);
                                }
                                tokenizer[i] = breakDownToken(tokenizer[i], breaker);
                                continue;
                            }

                            if (s.equals("+") && tokenizer[i].startsWith("+") && tokenizer[i].length() > 1 && tokenizer[i].substring(1, tokenizer[i].length()).matches("^[0-9]+?")) {
                                String breaker = tokenizer[i];
                                if (tokenizer[i].endsWith(";")) {
                                    breaker = breaker.substring(0, breaker.length() - 1);
                                }
                                tokenizer[i] = breakDownToken(tokenizer[i], breaker);
                                continue;
                            }

                            tokenizer[i] = breakDownToken(tokenizer[i], s);
                        }
                    }

                    if (originalListSize == tokenList.size()) {
                        tokenList.add(tokenizer[i]);
                    }
                }

                for (String token : tokenList) {

                    if (token.trim().isEmpty()) {
                        continue;
                    }

                    String tokenType = tokenTypesEnum.UNKNOWN.toString();

                    if (keyWords.contains(token.trim())) {
                        String temp = token.trim();
                        if (temp.equals(KeyConst.PRIVATE.getKey()) || temp.equals(KeyConst.PUBLIC.getKey())) {
                            tokenType = KeyConst.MODIFIER.getKey();
                        } else {
                            tokenType = temp;
                        }
                    } else if (token.matches("^[-|\\+]?[0-9]+$")) {
                        tokenType = tokenTypesEnum.NUMBER.toString();
                    } else if (token.equals("&&") || token.equals("||")) {
                        tokenType = tokenTypesEnum.LOGICAL_OPR.toString();
                    } else if (token.equals(">>") || token.equals("<<")) {
                        tokenType = tokenTypesEnum.IO_OPR.toString();
                    } else if (token.equals("<=") || token.equals(">=") || token.equals("==") || token.equals("<") || token.equals(">") || token.equals("!=")) {
                        tokenType = tokenTypesEnum.BOOLEAN_OPR.toString();
                    } else if (token.equals("=")) {
                        tokenType = tokenTypesEnum.ASSIGNMENT_OPR.toString();
                    } else if (token.equals("(")) {
                        tokenType = tokenTypesEnum.PAREN_OPEN.toString();
                    } else if (token.equals(")")) {
                        tokenType = tokenTypesEnum.PAREN_CLOSE.toString();
                    } else if (token.equals("{")) {
                        tokenType = tokenTypesEnum.BLOCK_BEGIN.toString();
                    } else if (token.equals("}")) {
                        tokenType = tokenTypesEnum.BLOCK_END.toString();
                    } else if (token.equals("[")) {
                        tokenType = tokenTypesEnum.ARRAY_BEGIN.toString();
                    } else if (token.equals("]")) {
                        tokenType = tokenTypesEnum.ARRAY_END.toString();
                    } else if (token.matches("[,.]")) {
                        tokenType = tokenTypesEnum.PUNCTUATION.toString();
                    } else if (token.equals(";")) {
                        tokenType = tokenTypesEnum.EOT.toString();
                    } else if (token.matches("[\\*\\+-/%]")) {
                        tokenType = tokenTypesEnum.MATH_OPR.toString();
                    } else if (token.matches("'" + "\\p{Print}" + "'") || token.matches("'\\" + "\\[n|s]" + "'")) {
                        tokenType = tokenTypesEnum.CHARACTER.toString();
                    } else if (token.matches("^[a-zA-Z]+[a-zA-Z0-9_]*$") && token.length() < 80) {
                        if (Character.isUpperCase(token.toCharArray()[0])) {
                            tokenType = KeyConst.CLASS_NAME.getKey();
                        } else {
                            tokenType = tokenTypesEnum.IDENTIFIER.toString();
                        }
                    }

                    lexicalList.add(new Tuple(token, tokenType, lineCount));
                }

                lineCount++;
            }

        } catch (IOException e) {
            System.out.println("There was an error reading in the file. Failed on line " + lineCount);
            System.exit(0);
        }

        if (printList) {
            StringBuilder sb = new StringBuilder();
            sb.append(String.format("%-20s %-20s %-20s %s", new String[]{"Name", "Type", "LineNumber", "\n"}));

            for (Tuple t : lexicalList) {
                sb.append(String.format("%-20s %-20s %-20s %s", t.toArray()));
            }

            System.out.print(sb.toString());
        }
    }

    private String checkForChars(String item, String breakDownItem) {
        int size = item.length();
        int index = item.indexOf(breakDownItem);

        if (index < size - 3) {
            if (item.charAt(index + 2) == '\'') {
                if (Pattern.matches("\\p{Print}", item.substring(index + 1, index + 2))) {
                    breakDownItem = item.substring(index, index + 3);
                }
            } else if (item.charAt(index + 1) == '\\' && item.charAt(index + 3) == '\'') {
                if (item.substring(index + 2, index + 3).matches("[n|s]")) {
                    breakDownItem = item.substring(index, index + 4);
                }
            }
            return breakDownItem;
        }

        if (index < size - 2) {
            if (item.charAt(index + 2) == '\'') {
                if (item.substring(index + 1, index + 2).matches("\\p{Print}]")) {
                    breakDownItem = item.substring(index, index + 3);
                }
            }
        }

        return breakDownItem;
    }

    private String breakDownToken(String item, String breakDownItem) {
        if (item.contains(breakDownItem) && item.trim().length() > 0) {
            int size = tokenList.size();
            int breakLength = 1;
            if (breakDownItem.equals("'")) {
                breakDownItem = checkForChars(item, breakDownItem);
            }
            if (breakDownItem.length() > 1) {
                breakLength = breakDownItem.length();
            }

            String temp = item.trim().substring(0, item.indexOf(breakDownItem));
            if (!temp.trim().isEmpty()) {
                for (String s : symbolCheck) {
                    while (temp.contains(s) && temp.length() > 0) {
                        temp = breakDownToken(temp, s);
                    }
                }
                if (size == tokenList.size()) {
                    tokenList.add(temp);
                }
            }
            if (breakDownItem.startsWith("+") && breakDownItem.substring(1, breakDownItem.length()).matches("^[0-9]+?")) {
                tokenList.add(breakDownItem.substring(1, breakDownItem.length()));
            } else {
                tokenList.add(breakDownItem);
            }

            size = tokenList.size();
            item = item.trim().substring(item.indexOf(breakDownItem) + breakLength, item.length());
            if (!item.trim().isEmpty()) {
                for (String s : symbolCheck) {
                    while (item.contains(s) && item.length() > 0) {
                        item = breakDownToken(item, s);
                    }
                }

                if (size == tokenList.size()) {
                    tokenList.add(item.substring(item.indexOf(breakDownItem) + 1, item.length()));
                }
            }
            return item;
        }
        return item;
    }

    public Tuple getToken() {
        try {
            return lexicalList.get(lexPtr);
        } catch (Exception e) {
            return new NullTuple();

        }
    }

    public Tuple peek() {
        try {
            return lexicalList.get(lexPtr + 1);
        } catch (Exception e) {
            return new NullTuple();
        }
    }

    public Tuple peekPreviousToken() {
        try {
            return lexicalList.get(lexPtr - 1);
        } catch (Exception e) {
            return new NullTuple();
        }
    }

    public Tuple previousToken() {
        if (lexPtr == 0) {
            return new NullTuple();
        }
        return lexicalList.get(--lexPtr);
    }

    public void nextToken() {
        lexPtr++;
    }

    public void resetList() {
        lexPtr = 0;
    }
}
