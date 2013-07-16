package project;

/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 10/27/12
 * Time: 2:54 PM
 */
public class Memory {
    private String label;
    private String data;

    public Memory(String label, String data) {
        this.label = label;
        this.data = data;
    }

    public String getLabel() {
        return label;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public String getData() {
        return data;
    }

    public void setData(String data) {
        this.data = data;
    }
}
