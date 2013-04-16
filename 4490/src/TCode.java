import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 4/7/13
 * Time: 5:30 PM
 */
public class TCode {
    private LinkedHashMap<String, Symbol> symbolTable = new LinkedHashMap<String, Symbol>();
    private List<ICode> iCodeList = new ArrayList<ICode>();
    private List<String> tCode = new ArrayList<String>();

    public TCode(LinkedHashMap<String, Symbol> symbolTable, List<ICode> iCodeList) {
        this.symbolTable = symbolTable;
        this.iCodeList = iCodeList;
    }

    public void buildCode() {
        for (String key : symbolTable.keySet()) {
            Symbol s = symbolTable.get(key);

            if (s.getSymId().startsWith("L") && Character.isDigit(s.getSymId().toCharArray()[1])) {
                if (s.getData() instanceof VaribleData) {
                    if (((VaribleData) s.getData()).getType().equals("int")) {
                        tCode.add(s.getSymId() + " " + s.getValue().substring(1, s.getValue().length()));
                    } else {
                        tCode.add(s.getSymId() + " " + s.getValue());
                    }
                }
            }
        }

        for (ICode iCode : iCodeList) {
            if (iCode.getOperation().equals("JMP")) {
                if (iCode.getLabel().equals("")) {
                    tCode.add(iCode.getOperation() + " " + iCode.getArg1() + " " + iCode.getComment());
                } else {
                    tCode.add(iCode.getLabel() + " " + iCode.getOperation() + " " + iCode.getArg1() + " " + iCode.getComment());
                }
                continue;
            }

            if (iCode.getOperation().equals("MAINST")) {
                tCode.add(iCode.getLabel() + " " + "MOV" + " " + "R1" + " " + "R1" + " " + "; start of main");
                continue;
            }

            if (iCode.getOperation().equals("TRP")) {
                if (iCode.getArg1().equals("0")) {
                    tCode.add("FINISH " + iCode.getOperation() + " " + iCode.getArg1() + " " + iCode.getComment());
                }
                continue;
            }

            if (iCode.getOperation().equals("RTN")) {
                tCode.add("JMP FINISH");
            }
        }

        for (String j : tCode) {
            System.out.println(j);
        }
    }
}
