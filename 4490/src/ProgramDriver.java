/**
 * Created by IntelliJ IDEA.
 * User: nmalloch
 * Date: 1/8/13
 * Time: 11:39 AM
 */
public class ProgramDriver {
    public static void main(String[] args) {
        Compiler compiler = new Compiler();
        compiler.runCompiler("lexicalTest.kxi");
    }
}
