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
    private List<Tuple<String, String, Integer>> openBlocks = new ArrayList<Tuple<String, String, Integer>>();
    String errorList = "";

    public SyntaxAnalyzer(LexicalAnalyzer lexicalAnalyzer) {
        this.lexicalAnalyzer = lexicalAnalyzer;
    }

    public void evaluate() throws IllegalArgumentException {
        Tuple<String, String, Integer> currentLex;
        Tuple<String, String, Integer> previousLex = null;

        while (lexicalAnalyzer.hasNext()) {
            Tuple<String, String, Integer> temp;
            List<Tuple<String, String, Integer>> tempList = new ArrayList<Tuple<String, String, Integer>>();

            temp = lexicalAnalyzer.getNext();
            while (canAddToList(temp)) {
                tempList.add(temp);
                temp = lexicalAnalyzer.getNext();
            }
            tempList.add(temp);

            if (tempList.isEmpty()) {
                continue;
            }

            for (int i = 0; i < tempList.size(); i++) {
                currentLex = tempList.get(i);

                if (currentLex.type.equals(LexicalAnalyzer.tokenTypesEnum.UNKNOWN.name())) {
                    errorList += "Unknown object. Line: " + currentLex.lineNum + "\n";
                }

                Tuple<String, String, Integer> nextLexi = (i + 1 == tempList.size() ? null : tempList.get(i + 1));
                Tuple<String, String, Integer> peekTwoPrevious = (i - 3 < 0 ? null : tempList.get(i - 3));
                Tuple<String, String, Integer> peekPrevious = (i - 2 < 0 ? null : tempList.get(i - 2));


                // validate expressions
                if (currentLex.type.equals(LexicalAnalyzer.tokenTypesEnum.ASSIGNMENT_OPR.name())) {
                    validateAssignmentOpr(currentLex, previousLex, nextLexi, peekPrevious);
                } else if (currentLex.type.equals(LexicalAnalyzer.tokenTypesEnum.MATH_OPR.name())) {
                    validateMathOpr(currentLex, previousLex, nextLexi);
                } else if (currentLex.type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name())) {
                    openParens.add(currentLex);
                } else if (currentLex.type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                    if (openParens.size() == 0) {
                        errorList += "Invalid closing paren on line: " + currentLex.lineNum + "\n";
                    } else {
                        openParens.remove(openParens.size() - 1);
                    }
                } else if (currentLex.type.equals(LexicalAnalyzer.tokenTypesEnum.RELATIONAL_OPR.name())) {
                    validateRelationalOpr(currentLex, previousLex, nextLexi);
                } else if (currentLex.type.equals(LexicalAnalyzer.tokenTypesEnum.BLOCK_BEGIN.name())) {
                    openBlocks.add(currentLex);
                } else if (currentLex.type.equals(LexicalAnalyzer.tokenTypesEnum.BLOCK_END.name())) {
                    if (openBlocks.size() == 0) {
                        errorList += "Invalid closing block on line: " + currentLex.lineNum + "\n";
                    } else {
                        openBlocks.remove(openBlocks.size() - 1);
                    }
                } else if (currentLex.type.equals(LexicalAnalyzer.tokenTypesEnum.BOOLEAN_OPR.name())) {
                    validateBooleanOpr(currentLex, previousLex, nextLexi);
                } else if (currentLex.type.equals(LexicalAnalyzer.tokenTypesEnum.IO_OPR.name())) {
                    validateIOOpr(currentLex, previousLex, nextLexi, peekPrevious);
                } else if (currentLex.lexi.equals(",")) {
                    validateComma(currentLex, previousLex, nextLexi);
                }


                // validate statements
                if (currentLex.type.equals(LexicalAnalyzer.tokenTypesEnum.KEYWORD.name())) {
                    if (currentLex.lexi.equals("return")) {
                        if (!tempList.get(0).lexi.equals("return")) {
                            errorList += "return must be the first statement on the line. Line: " + currentLex.lineNum + "\n";
                        }
                        if (!tempList.get(tempList.size() - 1).type.equals(LexicalAnalyzer.tokenTypesEnum.EOT.name())) {
                            errorList += "return statement must end with a end of line token (;). Line: " + currentLex.lineNum + "\n";
                        }
                        validateReturnStatement(currentLex, previousLex, nextLexi, peekPrevious);
                    }

                    if (currentLex.lexi.equals("class")) {
                        if (!tempList.get(0).lexi.equals("class")) {
                            errorList += "class declaration must be the first statement on the line. Line: " + currentLex.lineNum + "\n";
                        }
                        if (!tempList.get(tempList.size() - 1).type.equals(LexicalAnalyzer.tokenTypesEnum.BLOCK_BEGIN.name())) {
                            errorList += "class declaration line must end with a begin block token ({). Line: " + currentLex.lineNum + "\n";
                        }
                        if (tempList.size() > 3) {
                            errorList += "two many arguments in class declaration. Line: " + currentLex.lineNum + "\n";
                        }
                        if (tempList.size() < 3) {
                            errorList += "Missing arguments in class declaration. Line: " + currentLex.lineNum + "\n";
                        } else if (!tempList.get(i + 1).type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name())) {
                            errorList += "class name must be an Identifier. Line: " + currentLex.lineNum + "\n";
                        } else if (!Character.isUpperCase(tempList.get(i + 1).lexi.toCharArray()[0])) {
                            errorList += "class name must start with an uppercase letter. Line: " + currentLex.lineNum + "\n";
                        }
                    }

                    // TODO: change how this works -> look at in stages
                    if (currentLex.lexi.equals("private") || currentLex.lexi.equals("public")) {
                        if (i != 0) {
                            errorList += "Modifiers must be at the beginning of function declarations. Line: " + currentLex.lineNum + "\n";
                        } else {
                            if (tempList.size() < 6) {
                                if (tempList.size() < 4) {
                                    errorList += "There are to few arguments in function declaration. Line: " + currentLex.lineNum + "\n";
                                }
                                if (tempList.size() == 4) {
                                    if (tempList.get(1).lexi.equals("int") || tempList.get(1).lexi.equals("char") || tempList.get(1).lexi.equals("bool") || tempList.get(1).type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name())) {
                                        if (!tempList.get(2).type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name())) {
                                            errorList += "Type name must be an Identifier. Line: " + currentLex.lineNum + "\n";
                                        }
                                    } else {
                                        errorList += "Type declaration needs a valid type. Line: " + currentLex.lineNum + "\n";
                                    }
                                }
                                if (tempList.size() == 5) {

                                }

                            } else {
                                if (!tempList.get(tempList.size() - 1).type.equals(LexicalAnalyzer.tokenTypesEnum.BLOCK_BEGIN.name())) {
                                    errorList += "Function declaration line must end in a begin block token ({). Line: " + currentLex.lineNum + "\n";
                                }
                                if (!isValidReturnType(tempList.get(1).lexi, tempList.get(i + 1).type)) {
                                    errorList += "Function must have a valid return type. Line: " + currentLex.lineNum + "\n";
                                }
                                if (!tempList.get(2).type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name())) {
                                    errorList += "Function name must be an Identifier. Line: " + currentLex.lineNum + "\n";
                                }
                                if (!tempList.get(3).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name())) {
                                    errorList += "An open paren must directly proceed the function name. Line: " + currentLex.lineNum + "\n";
                                }
                                if (!tempList.get(tempList.size() - 2).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                                    errorList += "Must close function argument list before starting function block. Line: " + currentLex.lineNum + "\n";
                                }
                                if (!tempList.get(4).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                                    if (!isValidType(tempList, 4)) {
                                        errorList += "Function '" + tempList.get(2).lexi + "' contains an invalid argument list. Line: " + currentLex.lineNum + "\n";
                                    }
                                }
                            }
                        }
                    }


                    if (currentLex.lexi.equals("int") || currentLex.lexi.equals("char") || currentLex.lexi.equals("bool")) {
                        if (!isTypeDeclarationValid(tempList.get(0).lexi, i)) {
                            errorList += "Type declaration line must start with the identifier type. Line: " + currentLex.lineNum + "\n";
                        } else if (i == 0) {
                            if (!tempList.get(tempList.size() - 1).type.equals(LexicalAnalyzer.tokenTypesEnum.EOT.name())) {
                                errorList += "Type declaration must end with a end of line token (;). Line: " + currentLex.lineNum + "\n";
                            }
                            if (tempList.size() < 3) {
                                errorList += "Missing arguments in type declaration. Line: " + currentLex.lineNum + "\n";
                            } else {
                                if (!tempList.get(i + 1).type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name())) {
                                    errorList += "Type name must be an Identifier. Line: " + currentLex.lineNum + "\n";
                                }
                                if (!Character.isLowerCase(tempList.get(i + 1).lexi.toCharArray()[0])) {
                                    errorList += "Type name must start with a lowercase letter. Line: " + currentLex.lineNum + "\n";
                                }
                                if (tempList.size() > 3) {
                                    if (!tempList.get(i + 2).type.equals(LexicalAnalyzer.tokenTypesEnum.ASSIGNMENT_OPR.name())) {
                                        errorList += "The left hand side of type declaration and assignment must only contain the type name and object name. Line: " + currentLex.lineNum + "\n";
                                    }
                                }
                            }
                        }
                    }

                    if (currentLex.lexi.equals("void")) {

                    }
                }

                previousLex = currentLex;
            }

        }

        if (openParens.size() > 0) {
            String errorMessage = "Incomplete statements (missing closing parens) on the following lines: ";

            for (Tuple<String, String, Integer> item : openParens) {
                errorMessage += item.lineNum + ", ";
            }
            errorList += errorMessage + "\n";
        }

        if (openBlocks.size() > 0) {
            String errorMessage = "Incomplete statements (missing closing blocks) on the following lines: ";

            for (Tuple<String, String, Integer> item : openBlocks) {
                errorMessage += item.lineNum + ", ";
            }
            errorList += errorMessage + "\n";
        }

        if (!errorList.isEmpty()) {
            throw new IllegalArgumentException(errorList);
        }
    }

    private boolean isTypeDeclarationValid(String lexi, int index) {
        if (index != 0)
            if (lexi.equals("private") || lexi.equals("public"))
                return true;
            else
                return false;
        return true;
    }

    private void validateComma(Tuple<String, String, Integer> currentLex, Tuple<String, String, Integer> previousLex, Tuple<String, String, Integer> nextLexi) {
        if (previousLex.equals(null) || nextLexi.equals(null) || nextLexi.type.equals(LexicalAnalyzer.tokenTypesEnum.EOT.name())) {
            errorList += "There must be a value on both sides of comma. Line: " + currentLex.lineNum + "\n";
        }

        if ((previousLex.type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name())
                || previousLex.lexi.equals("true") || previousLex.lexi.equals("false")
                || previousLex.type.equals(LexicalAnalyzer.tokenTypesEnum.NUMBER.name())
                || previousLex.type.equals(LexicalAnalyzer.tokenTypesEnum.CHARACTER.name())) &&
                (nextLexi.type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name())
                || nextLexi.lexi.equals("true")
                || nextLexi.lexi.equals("false")
                || nextLexi.type.equals(LexicalAnalyzer.tokenTypesEnum.NUMBER.name())
                || nextLexi.type.equals(LexicalAnalyzer.tokenTypesEnum.CHARACTER.name()))
                || nextLexi.lexi.equals("int")
                || nextLexi.lexi.equals("char")
                || nextLexi.lexi.equals("bool")) {
            return;
        }
        errorList += "There must be a value on both sides of comma. Line: " + currentLex.lineNum + "\n";
    }

    private boolean isValidType(List<Tuple<String, String, Integer>> tempList, int index) {
        if (tempList.get(index).lexi.equals("bool") || tempList.get(index).lexi.equals("int") || tempList.get(index).lexi.equals("char") || tempList.get(index).type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name())) {
            if (!tempList.get(index + 1).type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name())) {
                return false;
            }
            if (tempList.get(index + 2).lexi.equals(",")) {
                return isValidType(tempList, index + 3);
            } else if (tempList.get(index + 2).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                return true;
            } else {
                return false;
            }
        }
        return false;
    }

    private boolean isValidReturnType(String retName, String retType) {
        return (retName.equals("bool") || retName.equals("int") || retName.equals("char") || retName.equals("void") || retType.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name()));
    }

    private void validateIOOpr(Tuple<String, String, Integer> currentLex, Tuple<String, String, Integer> previousLex, Tuple<String, String, Integer> nextLex, Tuple<String, String, Integer> peekPrevious) {
        if (currentLex.lexi.equals("<<")) {
            if (previousLex == null) {
                errorList += "Must start an ostream operation with 'cout'. Line: " + currentLex.lineNum + "\n";
            }

            if (!previousLex.lexi.equals("cout")) {
                errorList += "Must start an ostream operation with 'cout'. Line: " + currentLex.lineNum + "\n";
            }
        }

        if (currentLex.lexi.equals(">>")) {
            if (previousLex == null) {
                errorList += "Must start an istream operation with 'cin'. Line: " + currentLex.lineNum + "\n";
            }

            if (!previousLex.lexi.equals("cin")) {
                errorList += "Must start an istream operation with 'cin'. Line: " + currentLex.lineNum + "\n";
            }
        }

        if (nextLex == null || nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.EOT.name())) {
            errorList += "IO operators requires a right hand value. Line: " + currentLex.lineNum + "\n";
        }

        if (!isLHSinValidFormat(peekPrevious)) {
            errorList += "Only the IO statement can occupy the line (there should not be anything before it). Line: " + currentLex.lineNum + "\n";
        }

        if (currentLex.lexi.equals(">>")) {
            if (nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name())) {
                return;
            }
            errorList += "Right hand side of istream operator must be an Identifier. Line: " + currentLex.lineNum + "\n";
        } else {
            if (isRHSinValidFormatAssignment(nextLex)) {
                return;
            }
            errorList += "Right hand side of ostream operator must be either an Identifier, Number, or Character. Line: " + currentLex.lineNum + "\n";
        }
    }

    private boolean canAddToList(Tuple<String, String, Integer> temp) {
        return !(temp.type.equals(LexicalAnalyzer.tokenTypesEnum.EOT.name()) || temp.type.equals(LexicalAnalyzer.tokenTypesEnum.BLOCK_END.name()) || temp.type.equals(LexicalAnalyzer.tokenTypesEnum.BLOCK_BEGIN.name()));
    }

    private void validateBooleanOpr(Tuple<String, String, Integer> currentLex, Tuple<String, String, Integer> previousLex, Tuple<String, String, Integer> nextLex) {
        if (nextLex == null || nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.EOT.name()) || previousLex == null) {
            errorList += "There must be a valid type on both sides of the boolean operator. Line: " + currentLex.lineNum + "\n";
        }

        if (!isLHSinValidFormatRelationShip(previousLex)) {
            errorList += "Left hand side of the boolean operator must be an Identifier, Number, or Character. Line: " + previousLex.lineNum + "\n";
        }

        if (isRHSinValidFormatAssignment(nextLex)) {
            return;
        }
        errorList += "Right hand side of boolean operator must be either an Identifier, Number, or Character. Line: " + currentLex.lineNum + "\n";
    }

    private void validateReturnStatement(Tuple<String, String, Integer> currentLex, Tuple<String, String, Integer> previousLex, Tuple<String, String, Integer> nextLex, Tuple<String, String, Integer> peekPrevious) {
        if (!isReturnValueValid(nextLex)) {
            errorList += "Return statement must either be followed by a value or an end of line token (;). Line: " + currentLex.lineNum + "\n";
        }

        if (!isLHSinValidFormat(peekPrevious)) {
            errorList += "Only the return statement can occupy the line (there should not be anything before it). Line: " + currentLex.lineNum + "\n";
        }
    }

    private boolean isReturnValueValid(Tuple<String, String, Integer> nextLex) {
        if (nextLex == null)
            return false;
        else if (nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.NUMBER.name()) || nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name()) || nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.CHARACTER.name()) || nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.EOT.name()) || nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name()) || nextLex.lexi.equals("true") || nextLex.lexi.equals("false"))
            return true;
        return false;
    }

    private void validateRelationalOpr(Tuple<String, String, Integer> currentLex, Tuple<String, String, Integer> previousLex, Tuple<String, String, Integer> nextLex) {
        if (nextLex == null || nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.EOT.name()) || previousLex == null) {
            errorList += "There must be a valid type on both sides of the relational operator. Line: " + currentLex.lineNum + "\n";
        }

        if (!isLHSinValidFormatRelationShip(previousLex)) {
            errorList += "Left hand side of the relational operator must be an Identifier, Number, or Character. Line: " + previousLex.lineNum + "\n";
        }

        if (isRHSinValidFormatAssignment(nextLex)) {
            return;
        }
        errorList += "Right hand side of relational operatior must be either an Identifier, Number, or Character. Line: " + currentLex.lineNum + "\n";
    }

    private void validateMathOpr(Tuple<String, String, Integer> currentLex, Tuple<String, String, Integer> previousLex, Tuple<String, String, Integer> nextLex) {
        if (nextLex == null || nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.EOT.name()) || previousLex == null) {
            errorList += "Mathematical operators require a right hand value. Line: " + currentLex.lineNum + "\n";
        }

        if ((previousLex.type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name()) || previousLex.type.equals(LexicalAnalyzer.tokenTypesEnum.NUMBER.name()) || previousLex.lexi.equals(")")) && (nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name()) || nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.NUMBER.name()) || nextLex.lexi.equals("("))) {
            return;
        }
        errorList += "Both side of mathematical operation must be either an Identifier or a Number. Line: " + previousLex.lineNum + "\n";
    }

    private void validateAssignmentOpr(Tuple<String, String, Integer> currentLex, Tuple<String, String, Integer> previousLex, Tuple<String, String, Integer> nextLex, Tuple<String, String, Integer> previousTwoLexi) throws IllegalArgumentException {
        if (nextLex == null || nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.EOT.name()) || previousLex == null) {
            errorList += "There must be a valid type on both sides of the assignment operator. Line: " + currentLex.lineNum + "\n";
        }

        if (!isPreviousLexiValidAssignment(previousLex)) {
            errorList += "Left hand side of assignment operation must be an Identifier. Line: " + previousLex.lineNum + "\n";
        }

//        if (!isLHSinValidFormatAssignment(previousTwoLexi, previousLex)) {
//            errorList += "There can only be one variable or Identifier on the left side of the assignment operator. Line: " + currentLex.lineNum + "\n";
//        }

        if (isRHSinValidFormatAssignment(nextLex)) {
            return;
        }
        errorList += "Right hand side of assignment operation must be either an Identifier, Number, or Character. Line: " + currentLex.lineNum + "\n";
    }

    private boolean isRHSinValidFormatAssignment(Tuple<String, String, Integer> nextLex) {
        if (nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name()))
            return true;
        else if (nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.NUMBER.name()))
            return true;
        else if (nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.CHARACTER.name()))
            return true;
        else if (nextLex.lexi.equals("true") || nextLex.lexi.equals("false") || nextLex.lexi.equals("null") || nextLex.lexi.equals("this") || nextLex.lexi.equals("new"))
            return true;
        else if (nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name())) {
            return true;
        }

        return false;
    }

    private boolean isLHSinValidFormatRelationShip(Tuple<String, String, Integer> nextLex) {
        if (nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name()))
            return true;
        else if (nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.NUMBER.name()))
            return true;
        else if (nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.CHARACTER.name()))
            return true;
        else if (nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
            return true;
        }

        return false;
    }

    private boolean isPreviousLexiValidAssignment(Tuple<String, String, Integer> previousLex) {
        if (previousLex.type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name()))
            return true;
        else if (previousLex.type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
            return true;
        }

        return false;
    }

    private boolean isLHSinValidFormat(Tuple<String, String, Integer> peekPrevious) {
        if (peekPrevious == null)
            return true;
        else if (peekPrevious.type.equals(LexicalAnalyzer.tokenTypesEnum.BLOCK_END.name()))
            return true;
        else if (peekPrevious.type.equals(LexicalAnalyzer.tokenTypesEnum.EOT.name()))
            return true;

        return false;
    }

    private boolean isLHSinValidFormatAssignment(Tuple<String, String, Integer> peekPrevious, Tuple<String, String, Integer> previousLex) {
        if (peekPrevious == null)
            return true;
        else if (peekPrevious.type.equals(LexicalAnalyzer.tokenTypesEnum.BLOCK_END.name()))
            return true;
        else if (peekPrevious.type.equals(LexicalAnalyzer.tokenTypesEnum.EOT.name()))
            return true;
        else if (peekPrevious.type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name()))
            return true;
        else if (peekPrevious.type.equals(LexicalAnalyzer.tokenTypesEnum.BLOCK_BEGIN.name()))
            return true;
        else if (peekPrevious.type.equals(LexicalAnalyzer.tokenTypesEnum.BLOCK_END.name()))
            return true;
        else if (previousLex.type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name()))
            return true;
        else if (previousLex.lexi.equals("."))
            return true;

        return false;
    }
}