import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 12/8/12
 * Time: 1:09 PM
 */
public class RunnableThread extends Thread {
    private Assembler assembler;
    private List<Instruction> instructionList;
    private int instruction;
    private int end;

    public RunnableThread(ThreadGroup tg, String name, Assembler assembler, List<Instruction> instructionList, int instruction, int end) {
        super(tg, name);
        this.assembler = assembler;
        this.instructionList = instructionList;
        this.instruction = instruction;
        this.end = end;
    }

    public void run() {
        assembler.runVM(instructionList, instruction, end);
    }
}
