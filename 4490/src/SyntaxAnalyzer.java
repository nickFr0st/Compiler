/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 1/22/13
 * Time: 8:59 PM
 */
public class SyntaxAnalyzer {
    LexicalAnalyzer lexicalAnalyzer;

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
                if (lexicalAnalyzer.peekNext() == null || lexicalAnalyzer.peekNext().type.equals(LexicalAnalyzer.tokenTypesEnum.EOT.name()) || previousLex == null) {
                    throw new IllegalArgumentException("There must be a valid type on both sides of the assignment operator. Line: " + currentLex.lineNum);
                }
                validateAssignmentOpr(currentLex, previousLex, lexicalAnalyzer.peekNext());
            }

            previousLex = currentLex;
        }
    }

    private void validateAssignmentOpr(Tuple<String, String, Integer> currentLex, Tuple<String, String, Integer> previousLex, Tuple<String, String, Integer> nextLex) throws IllegalArgumentException {
        if (!previousLex.type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name())) {
            throw new IllegalArgumentException("Left hand side of assignment operation must be an Identifier. Line:" + previousLex.lineNum);
        }
        if (nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name()) || nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.NUMBER.name()) || nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.CHARACTER.name())) {
            return;
        }
        throw new IllegalArgumentException("Right hand side of assigment operation must be either an Identifier, Number, or Character");
    }
}