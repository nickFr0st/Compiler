import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 1/22/13
 * Time: 8:59 PM
 */
public class SyntaxAnalyzer {
    private LexicalAnalyzer lexicalAnalyzer;
    private List<Tuple<String, String, Integer>> openParens = new ArrayList<Tuple<String, String, Integer>>();

    public SyntaxAnalyzer(LexicalAnalyzer lexicalAnalyzer) {
        this.lexicalAnalyzer = lexicalAnalyzer;
    }

    public void evaluate() throws IllegalArgumentException {
        Tuple<String, String, Integer> currentLex = null;
        Tuple<String, String, Integer> previousLex = null;
        Tuple<String, String, Integer> nextLex = null;

        while (lexicalAnalyzer.hasNext()) {

            currentLex = lexicalAnalyzer.getNext();
            if (currentLex.type.equals(LexicalAnalyzer.tokenTypesEnum.ASSIGNMENT_OPR.name())) {
                validateAssignmentOpr(currentLex, previousLex, lexicalAnalyzer.peekNext());
            } else if (currentLex.type.equals(LexicalAnalyzer.tokenTypesEnum.MATH_OPR.name())) {
                validateMathOpr(currentLex, previousLex, lexicalAnalyzer.peekNext());
            } else if (currentLex.type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name())) {
                openParens.add(currentLex);
            } else if (currentLex.type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                if (openParens.size() == 0) {
                    throw new IllegalArgumentException("Invalid closing paren on line: " + currentLex.lineNum);
                }
                openParens.remove(openParens.size() -1);
            }

            previousLex = currentLex;
        }

        if (openParens.size() > 0) {
            String errorMessage = "Incomplete statements (missing closing parens) on the following lines: \n";

            for (Tuple<String, String, Integer> item : openParens) {
                errorMessage += item.lineNum + "\n";
            }
            throw new IllegalArgumentException(errorMessage);
        }
    }

    private void validateMathOpr(Tuple<String, String, Integer> currentLex, Tuple<String, String, Integer> previousLex, Tuple<String, String, Integer> nextLex) {
        if (lexicalAnalyzer.peekNext() == null || lexicalAnalyzer.peekNext().type.equals(LexicalAnalyzer.tokenTypesEnum.EOT.name()) || previousLex == null) {
            throw new IllegalArgumentException("Mathematical operators require a right hand value. Line: " + currentLex.lineNum);
        }

        if ((previousLex.type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name()) || previousLex.type.equals(LexicalAnalyzer.tokenTypesEnum.NUMBER.name())) && (nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name()) || nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.NUMBER.name()))) {
            return;
        }
        throw new IllegalArgumentException("Both side of mathematical operation must be either an Identifier or a Number. Line:" + previousLex.lineNum);
    }

    private void validateAssignmentOpr(Tuple<String, String, Integer> currentLex, Tuple<String, String, Integer> previousLex, Tuple<String, String, Integer> nextLex) throws IllegalArgumentException {
        if (lexicalAnalyzer.peekNext() == null || lexicalAnalyzer.peekNext().type.equals(LexicalAnalyzer.tokenTypesEnum.EOT.name()) || previousLex == null) {
            throw new IllegalArgumentException("There must be a valid type on both sides of the assignment operator. Line: " + currentLex.lineNum);
        }

        if (isNotValid1(previousLex)) {
            throw new IllegalArgumentException("Left hand side of assignment operation must be an Identifier. Line:" + previousLex.lineNum);
        }

        if (isNotValid2(lexicalAnalyzer.peekPrevious())) {
            throw new IllegalArgumentException("There can only be one variable or Identifier on the left side of the assignment operator. Line: " + currentLex.lineNum);
        }

        if (isValid1(nextLex)) {
            return;
        }
        throw new IllegalArgumentException("Right hand side of assignment operation must be either an Identifier, Number, or Character. Line: " + currentLex.lineNum);
    }

    private boolean isValid1(Tuple<String, String, Integer> nextLex) {
        if (nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name()))
            return true;
        else if (nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.NUMBER.name()))
            return true;
        else if (nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.CHARACTER.name()))
            return true;
        else if (nextLex.lexi.equals("true") || nextLex.lexi.equals("false") || nextLex.lexi.equals("null"))
            return true;
        else if (nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name())) {
            return true;
        }

        return false;
    }

    private boolean isNotValid1(Tuple<String, String, Integer> nextLex) {
        if (nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name()))
            return false;
        else if (nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name())) {
            return false;
        }else if (nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
            return false;
        }

        return true;
    }

    private boolean isNotValid2(Tuple<String, String, Integer> peekPrevious) {
        if (peekPrevious == null)
            return false;
        else if (!peekPrevious.type.equals(LexicalAnalyzer.tokenTypesEnum.BLOCK_END.name()))
            return false;
        else if (!peekPrevious.type.equals(LexicalAnalyzer.tokenTypesEnum.EOT.name()))
            return false;

        return true;
    }
}