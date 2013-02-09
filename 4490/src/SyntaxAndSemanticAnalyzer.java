import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 1/22/13
 * Time: 8:59 PM
 */
public class SyntaxAndSemanticAnalyzer {
    // todo: add symbol table
    private LexicalAnalyzer lexicalAnalyzer;
    private List<Tuple<String, String, Integer>> openParens = new ArrayList<Tuple<String, String, Integer>>();
    private List<Tuple<String, String, Integer>> openBlocks = new ArrayList<Tuple<String, String, Integer>>();
    private String errorList = "";
    private String scope = "g";
    private int statemetInr = 1;
    private int symIdInr = 100;

    public SyntaxAndSemanticAnalyzer(LexicalAnalyzer lexicalAnalyzer) {
        this.lexicalAnalyzer = lexicalAnalyzer;
    }

    public void evaluate(LinkedHashMap<String, Symbol> symbolTable) {
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
                    previousLex = currentLex;
                    continue;
                }

                if (currentLex.lexi.equals("object") || currentLex.lexi.equals("string")) {
                    errorList += currentLex.lexi + " is a reserved word not currently in use by the language. Line: " + currentLex.lineNum + "\n";
                    previousLex = currentLex;
                    continue;
                }

                Tuple<String, String, Integer> nextLexi = (i + 1 == tempList.size() ? null : tempList.get(i + 1));
                Tuple<String, String, Integer> peekPrevious = (i - 2 < 0 ? null : tempList.get(i - 2));


                // validate expressions
                if (currentLex.type.equals(LexicalAnalyzer.tokenTypesEnum.ASSIGNMENT_OPR.name())) {
                    validateAssignmentOpr(currentLex, previousLex, nextLexi);
                } else if (currentLex.type.equals(LexicalAnalyzer.tokenTypesEnum.MATH_OPR.name())) {
                    validateMathOpr(currentLex, previousLex, nextLexi);
                } else if (currentLex.type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name())) {
                    if (previousLex.type.equals(LexicalAnalyzer.tokenTypesEnum.NUMBER.name()) || previousLex.type.equals(LexicalAnalyzer.tokenTypesEnum.CHARACTER.name())) {
                        errorList += "Invalid function call on line: " + currentLex.lineNum + "\n";
                    }
                    openParens.add(currentLex);
                } else if (currentLex.type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                    if (nextLexi.type.equals(LexicalAnalyzer.tokenTypesEnum.NUMBER.name()) || nextLexi.type.equals(LexicalAnalyzer.tokenTypesEnum.CHARACTER.name())) {
                        errorList += "Invalid argument after closing paran. Line: " + currentLex.lineNum + "\n";
                    }
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
                        if (!scope.equals("g")) {
                            scope = scope.substring(0,scope.lastIndexOf('.'));
                        }
                        openBlocks.remove(openBlocks.size() - 1);
                    }
                } else if (currentLex.type.equals(LexicalAnalyzer.tokenTypesEnum.BOOLEAN_OPR.name())) {
                    validateBooleanOpr(currentLex, previousLex, nextLexi);
                } else if (currentLex.type.equals(LexicalAnalyzer.tokenTypesEnum.IO_OPR.name())) {
                    validateIOOpr(currentLex, previousLex, nextLexi, peekPrevious);
                } else if (currentLex.lexi.equals(",")) {
                    validateComma(currentLex, previousLex, nextLexi);
                } else if (currentLex.lexi.equals(".")) {
                    if (i == 0) {
                        errorList += "Invalid dot operator. Line: " + currentLex.lineNum + "\n";
                        previousLex = currentLex;
                        continue;
                    }
                    if (!isLHSofDotValid(tempList, i - 1)) {
                        errorList += "LHS of dot operator is invalid. Line: " + currentLex.lineNum + "\n";
                        previousLex = currentLex;
                        continue;
                    }
                    if (!nextLexi.type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name()) || Character.isUpperCase(nextLexi.lexi.toCharArray()[0])) {
                        errorList += "RHS of dot operator is invalid. Line: " + currentLex.lineNum + "\n";
                        previousLex = currentLex;
                        continue;
                    }
                }


                /**\
                 * validate keywords
                 */
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
                        int errCount = errorList.length();
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
                        if (errCount != errorList.length()) {
                            previousLex = currentLex;
                            continue;
                        }
                        symbolTable.put("C" + symIdInr++, new Symbol(scope, "C" + symIdInr, nextLexi.lexi, "Class", new ClassData()));
                        scope += "." + tempList.get(1).lexi;
                    }

                    /**
                     *  check for valid function, object and class declarations
                     **/
                    if (currentLex.lexi.equals("private") || currentLex.lexi.equals("public")) {
                        if (i != 0) {
                            errorList += "Modifiers must be at the beginning of function declarations. Line: " + currentLex.lineNum + "\n";
                        } else {
                            if (tempList.size() < 4) {
                                errorList += "There are to few arguments in declaration. Line: " + currentLex.lineNum + "\n";
                            } else {
                                if (!isValidReturnType(tempList.get(1).lexi, tempList.get(i + 1).type)) {
                                    errorList += "Function or object requires a valid type. Line: " + currentLex.lineNum + "\n";
                                } else {
                                    if (!isStepValid(tempList.get(2).type, tempList.get(2).lexi)) {
                                        errorList += "Missing function name or argument list(also check that functions and variables start with lower case letters). Line: " + currentLex.lineNum + "\n";
                                    } else if (tempList.get(2).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name()) && !tempList.get(3).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                                        if (!isValidParameterDeclarationType(tempList, 3)) {
                                            errorList += "Function '" + tempList.get(1).lexi + "' contains an invalid argument list. Line: " + currentLex.lineNum + "\n";
                                        }
                                    } else if (tempList.get(2).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name()) && tempList.get(3).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name()) && tempList.get(4).type.equals(LexicalAnalyzer.tokenTypesEnum.BLOCK_BEGIN.name())) {
                                        // todo: need fix array list of parameters area
                                        symbolTable.put("F" + symIdInr++, new Symbol(scope, "F" + symIdInr, tempList.get(1).lexi, "Function", new FunctionData(tempList.get(0).lexi, new ArrayList<String>(), tempList.get(1).lexi)));
                                        scope += "." + tempList.get(1).lexi;
                                        // all good
                                    } else {
                                        if (tempList.get(3).type.equals(LexicalAnalyzer.tokenTypesEnum.EOT.name())) {
                                            symbolTable.put("V" + symIdInr++, new Symbol(scope, "V" + symIdInr, tempList.get(2).lexi, "ivar", new VaribleData(tempList.get(1).lexi, tempList.get(0).lexi)));
                                            // all good
                                        } else if (tempList.get(3).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name()) && !tempList.get(4).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                                            if (!isValidParameterDeclarationType(tempList, 4)) {
                                                errorList += "Function '" + tempList.get(2).lexi + "' contains an invalid argument list. Line: " + currentLex.lineNum + "\n";
                                            } else {
                                                symbolTable.put("F" + symIdInr++, new Symbol(scope, "F" + symIdInr, tempList.get(2).lexi, "Function", new FunctionData(tempList.get(0).lexi, new ArrayList<String>(), tempList.get(1).lexi)));
                                                scope += "." + tempList.get(2).lexi;
                                            }
                                        } else if (tempList.get(3).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name()) && tempList.get(4).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name()) && tempList.get(5).type.equals(LexicalAnalyzer.tokenTypesEnum.BLOCK_BEGIN.name())) {
                                            symbolTable.put("F" + symIdInr++, new Symbol(scope, "F" + symIdInr, tempList.get(2).lexi, "Function", new FunctionData(tempList.get(0).lexi, null, tempList.get(1).lexi)));
                                            scope += "." + tempList.get(2).lexi;
                                            // all good
                                        } else {
                                            errorList += "Function or object has been incorrectly formatted. Line: " + currentLex.lineNum + "\n";
                                        }
                                    }
                                }
                            }
                        }
                    }

                    /**
                     * validate new operator
                     */
                    if (currentLex.lexi.equals("new")) {
                        if (!isValidObjectTypeExpression(nextLexi)) {
                            errorList += "Invalid use of the new operator. Line: " + currentLex.lineNum + "\n";
                            previousLex = currentLex;
                            continue;
                        }
                        if (tempList.get(i + 2).type.equals(LexicalAnalyzer.tokenTypesEnum.EOT.name())) {
                            // all good
                            previousLex = currentLex;
                            continue;
                        }
                        if (tempList.get(i + 2).type.equals(LexicalAnalyzer.tokenTypesEnum.ARRAY_BEGIN.name())) {
                            if (tempList.size() < 11) {
                                errorList += "Invalid array initialization. Line: " + currentLex.lineNum + "\n";
                                previousLex = currentLex;
                                continue;
                            }
                            if ((tempList.get(i + 3).type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name()) || tempList.get(i + 3).type.equals(LexicalAnalyzer.tokenTypesEnum.NUMBER.name())) && tempList.get(i + 4).type.equals(LexicalAnalyzer.tokenTypesEnum.ARRAY_END.name()) && tempList.get(i + 5).type.equals(LexicalAnalyzer.tokenTypesEnum.EOT.name())) {
                                // all good
                            } else {
                                errorList += "Invalid array initialization. Line: " + currentLex.lineNum + "\n";
                                previousLex = currentLex;
                                continue;
                            }
                        } else if (tempList.get(i + 2).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name())) {
                            if (!tempList.get(i + 3).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                                if (!isValidCalledParameterType(tempList, i + 3)) {
                                    errorList += "Function '" + tempList.get(i + 1).lexi + "' contains an invalid argument list. Line: " + currentLex.lineNum + "\n";
                                    previousLex = currentLex;
                                    continue;
                                } else {
                                    // all good
                                }
                            }

                        } else {
                            errorList += "Invalid object initialization. Line: " + currentLex.lineNum + "\n";
                            previousLex = currentLex;
                            continue;
                        }
                    }

                    /**
                     * validate itoa and atoi
                     */
                    if (currentLex.lexi.equals("atoi")) {
                        if (nextLexi.type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name())) {
                            if (tempList.get(i + 2).type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name()) && Character.isLowerCase(tempList.get(i + 2).lexi.toCharArray()[0])) {
                                if (!tempList.get(i + 3).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                                    errorList += "Incorrect parameter list for atoi function. Line: " + currentLex.lineNum + "\n";
                                    previousLex = currentLex;
                                    continue;
                                }
                            } else {
                                errorList += "Incorrect parameter list for atoi function. Line: " + currentLex.lineNum + "\n";
                                previousLex = currentLex;
                                continue;
                            }
                        } else {
                            errorList += "Incorrect parameter list for atoi function. Line: " + currentLex.lineNum + "\n";
                            previousLex = currentLex;
                            continue;
                        }
                    }
                    if (currentLex.lexi.equals("itoa")) {
                        if (nextLexi.type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name())) {
                            if (!tempList.get(i + 2).lexi.equals(")")) {
                                if (!isValidCalledParameterTypeITOA(tempList, i + 2)) {
                                    errorList += "Incorrect parameter list for atoi function. Line: " + currentLex.lineNum + "\n";
                                    previousLex = currentLex;
                                    continue;
                                }
                                if (tempList.get(i + 3).lexi.equals(",") && isValidCalledParameterTypeITOA(tempList, i + 4) && tempList.get(i + 5).lexi.equals(",") && isValidCalledParameterTypeITOALast(tempList, i + 6)) {
                                    // all good
                                } else {
                                    errorList += "Incorrect parameter list for atoi function. Line: " + currentLex.lineNum + "\n";
                                    previousLex = currentLex;
                                    continue;
                                }
                            }
                        } else {
                            errorList += "Incorrect parameter list for atoi function. Line: " + currentLex.lineNum + "\n";
                            previousLex = currentLex;
                            continue;
                        }
                    }

                    /**
                     * validate if statement
                     */
                    if (currentLex.lexi.equals("if")) {
                        if (!tempList.get(i + 1).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name())) {
                            errorList += "Invalid if statement argument. Line: " + currentLex.lineNum + "\n";
                            previousLex = currentLex;
                            continue;
                        }

                        if (tempList.get(i + 2).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                            errorList += "Missing if statement arguments. Line: " + currentLex.lineNum + "\n";
                            previousLex = currentLex;
                            continue;
                        }

                        if (!isValidRelationParameterType(tempList, i + 2)) {
                            errorList += "If statement contains an invalid argument list. Line: " + currentLex.lineNum + "\n";
                            previousLex = currentLex;
                            continue;
                        }
                        previousLex = currentLex;
                        continue;
                    }

                    if (currentLex.lexi.equals("else")) {

                        if (tempList.get(i + 1).lexi.equals("if")) {
                            if (!tempList.get(i + 2).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name())) {
                                errorList += "Invalid if statement argument. Line: " + currentLex.lineNum + "\n";
                                previousLex = currentLex;
                                continue;
                            }

                            if (tempList.get(i + 3).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                                errorList += "Missing if statement arguments. Line: " + currentLex.lineNum + "\n";
                                previousLex = currentLex;
                                continue;
                            }

                            if (!isValidRelationParameterType(tempList, i + 3)) {
                                errorList += "If statement contains an invalid argument list. Line: " + currentLex.lineNum + "\n";
                                previousLex = currentLex;
                                continue;
                            }
                            previousLex = currentLex;
                            continue;
                        }

                        if (!tempList.get(i + 1).type.equals(LexicalAnalyzer.tokenTypesEnum.BLOCK_BEGIN.name())) {
                            errorList += "Invalid else statement. Line: " + currentLex.lineNum + "\n";
                            previousLex = currentLex;
                            continue;
                        }
                        previousLex = currentLex;
                        continue;
                    }

                    if (currentLex.lexi.equals("while")) {
                        if (!tempList.get(i + 1).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name())) {
                            errorList += "Invalid while argument statement. Line: " + currentLex.lineNum + "\n";
                            previousLex = currentLex;
                            continue;
                        }

                        if (tempList.get(i + 2).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                            errorList += "Missing while statement arguments. Line: " + currentLex.lineNum + "\n";
                            previousLex = currentLex;
                            continue;
                        }

                        if (!isValidRelationParameterType(tempList, i + 2)) {
                            errorList += "While statement contains an invalid argument list. Line: " + currentLex.lineNum + "\n";
                            previousLex = currentLex;
                            continue;
                        }
                        previousLex = currentLex;
                        continue;
                    }

                }


                /**
                 * validate object type declaration
                 */
                if (i == 0 && (currentLex.lexi.equals("void") || currentLex.lexi.equals("int") || currentLex.lexi.equals("char") || currentLex.lexi.equals("bool") || (currentLex.type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name()) && Character.isUpperCase(currentLex.lexi.toCharArray()[0])))) {
                    if (tempList.size() < 3) {
                        errorList += "Incorrect format, too few arguments. Line: " + currentLex.lineNum + "\n";
                        previousLex = currentLex;
                        continue;
                    }
                    if (tempList.get(i + 1).lexi.equals("main")) {
                        if (!tempList.get(i + 2).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name())) {
                            errorList += "Main function is missing the argument list. Line: " + currentLex.lineNum + "\n";
                            previousLex = currentLex;
                            continue;
                        } else {
                            if (tempList.get(i + 3).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                                if (!tempList.get(i + 4).type.equals(LexicalAnalyzer.tokenTypesEnum.BLOCK_BEGIN.name())) {
                                    errorList += "Main function declaration must end with an block begin token ({). Line: " + currentLex.lineNum + "\n";
                                    previousLex = currentLex;
                                    continue;
                                }
                            } else {
                                errorList += "Main function declaration must end with an block begin token ({). Line: " + currentLex.lineNum + "\n";
                                previousLex = currentLex;
                                continue;
                            }
                        }
                        previousLex = currentLex;
                        continue;
                    }
                    if (tempList.size() < 3) {
                        errorList += "Missing arguments in type declaration. Line: " + currentLex.lineNum + "\n";
                    } else {
                        if (tempList.get(i + 1).type.equals(LexicalAnalyzer.tokenTypesEnum.ARRAY_BEGIN.name())) {
                            if (!tempList.get(i + 2).type.equals(LexicalAnalyzer.tokenTypesEnum.ARRAY_END.name())) {
                                errorList += "Incorrectly defined array. Line: " + currentLex.lineNum + "\n";
                                previousLex = currentLex;
                                continue;
                            }
                            if (tempList.get(i + 2).type.equals(LexicalAnalyzer.tokenTypesEnum.ARRAY_END.name())) {
                                if (tempList.get(i + 3).type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name()) && Character.isLowerCase(tempList.get(i + 3).lexi.toCharArray()[0])) {
                                    if (tempList.get(i + 4).type.equals(LexicalAnalyzer.tokenTypesEnum.EOT.name())) {
                                        // all good
                                    }
                                    if (tempList.get(i + 4).type.equals(LexicalAnalyzer.tokenTypesEnum.ASSIGNMENT_OPR.name())) {
                                        // all good
                                    } else {
                                        errorList += "Incorrectly defined array. Line: " + currentLex.lineNum + "\n";
                                    }
                                }
                            }
                        } else if (tempList.get(i + 1).type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name()) && Character.isLowerCase(tempList.get(i + 1).lexi.toCharArray()[0])) {
                            if (tempList.get(i + 2).type.equals(LexicalAnalyzer.tokenTypesEnum.EOT.name()) || tempList.get(i + 2).type.equals(LexicalAnalyzer.tokenTypesEnum.ASSIGNMENT_OPR.name())) {
                                // all good
                            } else if (tempList.get(i + 2).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name())) {
                                if (!tempList.get(i + 3).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                                    if (!isValidParameterDeclarationType(tempList, i + 3)) {
                                        errorList += "Function '" + tempList.get(1).lexi + "' contains an invalid argument list. Line: " + currentLex.lineNum + "\n";
                                    }
                                } else if (tempList.get(i + 3).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name()) && (tempList.get(i + 4).type.equals(LexicalAnalyzer.tokenTypesEnum.EOT.name()) || tempList.get(i + 4).type.equals(LexicalAnalyzer.tokenTypesEnum.BLOCK_BEGIN.name()))) {
                                    // all good
                                } else {
                                    errorList += "Incorrect function definition format. Line: " + currentLex.lineNum + "\n";
                                }
                            } else {
                                errorList += "Incorrectly formatted object definition. Line: " + currentLex.lineNum + "\n";
                            }
                        } else {
                            errorList += "Incorrectly defined object. Line: " + currentLex.lineNum + "\n";
                        }
                    }
                }

                /**
                 * validates function and variable use
                 */
                if (currentLex.type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name()) && i == 0 && Character.isLowerCase(currentLex.lexi.toCharArray()[0])) {
                    if (tempList.size() < 4) {
                        errorList += "Too few arguments. Line: " + currentLex.lineNum + "\n";
                        previousLex = currentLex;
                        continue;
                    }
                    if (tempList.get(i + 1).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name())) {
                        if (!tempList.get(i + 2).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                            if (!isValidCalledParameterType(tempList, i + 2)) {
                                errorList += "Function '" + tempList.get(0).lexi + "' contains an invalid argument list. Line: " + currentLex.lineNum + "\n";
                                previousLex = currentLex;
                                continue;
                            }
                        } else if (tempList.get(i + 2).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name()) && tempList.get(i + 3).type.equals(LexicalAnalyzer.tokenTypesEnum.EOT.name())) {
                            // all good
                        } else {
                            errorList += "Syntax error on function call format. Line: " + currentLex.lineNum + "\n";
                            previousLex = currentLex;
                            continue;
                        }
                    } else if (tempList.get(i + 1).type.equals(LexicalAnalyzer.tokenTypesEnum.ASSIGNMENT_OPR.name())) {
                        // all good
                    } else if (tempList.get(i + 1).type.equals(LexicalAnalyzer.tokenTypesEnum.ARRAY_BEGIN.name())) {
                        if (tempList.size() < 6) {
                            errorList += "Too few arguments. Line: " + currentLex.lineNum + "\n";
                            previousLex = currentLex;
                            continue;
                        }
                        if (tempList.get(i + 2).type.equals(LexicalAnalyzer.tokenTypesEnum.NUMBER.name()) || tempList.get(i + 2).type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name()) && (tempList.get(i + 3).type.equals(LexicalAnalyzer.tokenTypesEnum.ARRAY_END.name()) && tempList.get(i + 4).type.equals(LexicalAnalyzer.tokenTypesEnum.ASSIGNMENT_OPR.name()))) {
                            // all good
                        } else {
                            errorList += "Syntax error on array call format. Line: " + currentLex.lineNum + "\n";
                            previousLex = currentLex;
                            continue;
                        }
                    } else if (tempList.get(i + 1).lexi.equals(".") && (tempList.get(i + 2).type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name()) && Character.isLowerCase(tempList.get(i + 2).lexi.toCharArray()[0]))) {
                        if (tempList.get(i + 3).type.equals(LexicalAnalyzer.tokenTypesEnum.ASSIGNMENT_OPR.name())) {
                            // all good
                        }
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

        try {
            if (!errorList.isEmpty()) {
                throw new IllegalArgumentException(errorList);
            }
        } catch (IllegalArgumentException e) {
            System.out.print(e.getMessage());
            System.exit(0);
        }
    }

    private boolean isLHSofDotValid(List<Tuple<String, String, Integer>> tempList, int index) {
        return (tempList.get(index).lexi.equals("this") || tempList.get(index).type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name()) || tempList.get(index).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name()) || tempList.get(index).type.equals(LexicalAnalyzer.tokenTypesEnum.ARRAY_END.name()));
    }

    private boolean isValidObjectTypeExpression(Tuple<String, String, Integer> lexi) {
        return (lexi.lexi.equals("char") || lexi.lexi.equals("int") || lexi.lexi.equals("bool") || (lexi.type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name()) && Character.isUpperCase(lexi.lexi.toCharArray()[0])));
    }

    private boolean isStepValid(String type, String name) {
        return ((type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name()) && Character.isLowerCase(name.toCharArray()[0])) || type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name()));
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

    private boolean isValidParameterDeclarationType(List<Tuple<String, String, Integer>> tempList, int index) {
        if (tempList.get(index).lexi.equals("bool") || tempList.get(index).lexi.equals("int") || tempList.get(index).lexi.equals("char") || (tempList.get(index).type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name()) && Character.isUpperCase(tempList.get(index).lexi.toCharArray()[0]))) {
            if (!tempList.get(index + 1).type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name())) {
                return false;
            }
            if (!Character.isLowerCase(tempList.get(index + 1).lexi.toCharArray()[0])) {
                errorList += "Parameter names must be lowercase. Line: " + tempList.get(index).lineNum + "\n";
                return false;
            }
            if (tempList.get(index + 2).lexi.equals(",")) {
                return isValidParameterDeclarationType(tempList, index + 3);
            } else if (tempList.get(index + 2).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                return true;
            } else {
                return false;
            }
        }
        return false;
    }

    private boolean isValidCalledParameterType(List<Tuple<String, String, Integer>> tempList, int index) {
        if ((tempList.get(index).type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name()) && Character.isLowerCase(tempList.get(index).lexi.toCharArray()[0])) || tempList.get(index).type.equals(LexicalAnalyzer.tokenTypesEnum.NUMBER.name()) || tempList.get(index).type.equals(LexicalAnalyzer.tokenTypesEnum.CHARACTER.name())) {
            if (tempList.get(index + 1).lexi.equals(".")) {
                index = index + 2;
                if (!isValidCalledParameterType(tempList, index)) {
                    return false;
                }
            }
            if (tempList.get(index + 1).lexi.equals("(")) {
                if (!tempList.get(index + 2).lexi.equals(")")) {
                    index = index + 2;
                    if (!isValidCalledParameterType(tempList, index)) {
                        return false;
                    }
                } else {
                    index++;
                }
            }
            if (tempList.get(index + 1).lexi.equals(",")) {
                return isValidCalledParameterType(tempList, index + 2);
            } else if (tempList.get(index + 1).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                return true;
            } else {
                return false;
            }
        }
        return false;
    }

    private boolean isValidRelationParameterType(List<Tuple<String, String, Integer>> tempList, int index) {
        if ((tempList.get(index).type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name()) && Character.isLowerCase(tempList.get(index).lexi.toCharArray()[0])) || tempList.get(index).type.equals(LexicalAnalyzer.tokenTypesEnum.NUMBER.name()) || tempList.get(index).type.equals(LexicalAnalyzer.tokenTypesEnum.CHARACTER.name()) || tempList.get(index).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name()) || tempList.get(index).lexi.equals("true") || tempList.get(index).lexi.equals("false")) {
            if (tempList.get(index).lexi.equals("(")) {
                index++;
            }
            if (tempList.get(index + 1).type.equals(LexicalAnalyzer.tokenTypesEnum.ARRAY_BEGIN.name())) {
                if (!tempList.get(index + 2).type.equals(LexicalAnalyzer.tokenTypesEnum.NUMBER.name())) {
                    return false;
                }
                if (!tempList.get(index + 3).type.equals(LexicalAnalyzer.tokenTypesEnum.ARRAY_END.name())) {
                    return false;
                }
                index = index + 3;
            }
            if (tempList.get(index + 1).lexi.equals(".")) {
                index = index + 2;
                if (!isValidRelationParameterType(tempList, index)) {
                    return false;
                }
            }
            if (tempList.get(index + 1).lexi.equals("(")) {
                if (!tempList.get(index + 2).lexi.equals(")")) {
                    index = index + 2;
                    if (!isValidRelationParameterType(tempList, index)) {
                        return false;
                    }
                } else {
                    index++;
                }
            }
            if (tempList.get(index + 1).type.equals(LexicalAnalyzer.tokenTypesEnum.BOOLEAN_OPR.name()) || tempList.get(index + 1).type.equals(LexicalAnalyzer.tokenTypesEnum.RELATIONAL_OPR.name()) || tempList.get(index + 1).type.equals(LexicalAnalyzer.tokenTypesEnum.MATH_OPR.name())) {
                return isValidRelationParameterType(tempList, index + 2);
            } else if (tempList.get(index + 1).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                return true;
            } else {
                return false;
            }
        }
        return false;
    }

    private boolean isValidCalledParameterTypeITOA(List<Tuple<String, String, Integer>> tempList, int index) {
        if ((tempList.get(index).type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name()) && Character.isLowerCase(tempList.get(index).lexi.toCharArray()[0])) || tempList.get(index).type.equals(LexicalAnalyzer.tokenTypesEnum.NUMBER.name()) || tempList.get(index).type.equals(LexicalAnalyzer.tokenTypesEnum.CHARACTER.name())) {
            if (tempList.get(index + 1).lexi.equals(".")) {
                index = index + 2;
                if (!isValidCalledParameterTypeITOA(tempList, index)) {
                    return false;
                }
            }
            if (tempList.get(index + 1).lexi.equals("(")) {
                if (!tempList.get(index + 2).lexi.equals(")")) {
                    index = index + 2;
                    if (!isValidCalledParameterTypeITOA(tempList, index)) {
                        return false;
                    }
                } else {
                    index++;
                }
            } else if (tempList.get(index + 1).lexi.equals(",")) {
                return true;
            }
            if (tempList.get(index + 1).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                return true;
            }
        }
        return false;
    }

    private boolean isValidCalledParameterTypeITOALast(List<Tuple<String, String, Integer>> tempList, int index) {
        if ((tempList.get(index).type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name()) && Character.isLowerCase(tempList.get(index).lexi.toCharArray()[0])) || tempList.get(index).type.equals(LexicalAnalyzer.tokenTypesEnum.NUMBER.name()) || tempList.get(index).type.equals(LexicalAnalyzer.tokenTypesEnum.CHARACTER.name())) {
            if (tempList.get(index + 1).lexi.equals(".")) {
                index = index + 2;
                if (!isValidCalledParameterTypeITOALast(tempList, index)) {
                    return false;
                }
            }
            if (tempList.get(index + 1).lexi.equals("(")) {
                if (!tempList.get(index + 2).lexi.equals(")")) {
                    index = index + 2;
                    if (!isValidCalledParameterTypeITOALast(tempList, index)) {
                        return false;
                    }
                } else {
                    index++;
                }
            }
            if (tempList.get(index + 1).type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name())) {
                return true;
            }
        }
        return false;
    }

    private boolean isValidReturnType(String retName, String retType) {
        return (retName.equals("bool") || retName.equals("int") || retName.equals("char") || retName.equals("void") || (retType.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name()) && Character.isUpperCase(retName.toCharArray()[0])));
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
        if (nextLex.equals(null) || nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.EOT.name()) || previousLex.equals(null)) {
            errorList += "Mathematical operators require a right hand value. Line: " + currentLex.lineNum + "\n";
        }

        if ((previousLex.type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name()) || previousLex.type.equals(LexicalAnalyzer.tokenTypesEnum.NUMBER.name()) || previousLex.lexi.equals(")")) && (nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name()) || nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.NUMBER.name()) || nextLex.lexi.equals("("))) {
            return;
        }
        errorList += "Both side of mathematical operation must be either an Identifier or a Number. Line: " + previousLex.lineNum + "\n";
    }

    private void validateAssignmentOpr(Tuple<String, String, Integer> currentLex, Tuple<String, String, Integer> previousLex, Tuple<String, String, Integer> nextLex) throws IllegalArgumentException {
        if (nextLex == null || nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.EOT.name()) || previousLex == null) {
            errorList += "There must be a valid type on both sides of the assignment operator. Line: " + currentLex.lineNum + "\n";
        }

        if (!isPreviousLexiValidAssignment(previousLex)) {
            errorList += "Left hand side of assignment operation must be an Identifier. Line: " + previousLex.lineNum + "\n";
        }

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
        else if (nextLex.lexi.equals("true") || nextLex.lexi.equals("false") || nextLex.lexi.equals("null") || nextLex.lexi.equals("this") || nextLex.lexi.equals("new") || nextLex.lexi.equals("atoi") || nextLex.lexi.equals("itoa"))
            return true;
        else if (nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_OPEN.name())) {
            return true;
        } else if (nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.BLOCK_BEGIN.name())) {
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
        else if (nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name()))
            return true;
        else if (nextLex.type.equals(LexicalAnalyzer.tokenTypesEnum.ARRAY_END.name()))
            return true;

        return false;
    }

    private boolean isPreviousLexiValidAssignment(Tuple<String, String, Integer> previousLex) {
        if (previousLex.type.equals(LexicalAnalyzer.tokenTypesEnum.IDENTIFIER.name()))
            return true;
        else if (previousLex.type.equals(LexicalAnalyzer.tokenTypesEnum.PAREN_CLOSE.name()))
            return true;
        else if (previousLex.type.equals(LexicalAnalyzer.tokenTypesEnum.ARRAY_END.name()))
            return true;

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
}