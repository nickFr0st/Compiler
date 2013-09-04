package project;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 9/3/13
 * Time: 10:39 PM
 */
public class EAL_SAR extends SAR {
    List<SAR> arguments;

    public EAL_SAR(List<SAR> arguments) {
        this.arguments = arguments;
    }

    public List<SAR> getArguments() {
        return arguments;
    }
}
