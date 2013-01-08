import java.io.*;
import java.util.StringTokenizer;

/**
 * Created by IntelliJ IDEA.
 * User: nmalloch
 * Date: 1/8/13
 * Time: 12:05 PM
 */
public class Compiler {
    int lineCount = 0;

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

                StringTokenizer tokenizer = new StringTokenizer(line);
                while (tokenizer.hasMoreTokens()) {
                    System.out.println(tokenizer.nextElement());
                }
                //TODO: all the work

                lineCount++;
            }
        } catch (IOException e) {
            System.out.println("There was an error reading in the file");
            System.exit(0);
        }


        System.out.print("success");
    }
}
