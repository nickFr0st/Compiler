package project;

/**
 * Created by Malloch on 2/19/14
 */
public class Parameter {
    private String type;
    private String id;

    public Parameter(String type, String id) {
        this.type = type;
        this.id = id;
    }

    public String getType() {
        return type;
    }

    public String getId() {
        return id;
    }
}
