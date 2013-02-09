/**
 * Created with IntelliJ IDEA.
 * User: Nathanael
 * Date: 2/9/13
 * Time: 11:45 AM
 */
public class Symbol {
    private String scope;
    private String symId;       // unique identifier
    private String value;       // lexi
    private String kind;        // type
    private IData data;        // kind specific data

    public Symbol(String scope, String symId, String value, String kind, IData data) {
        this.scope = scope;
        this.symId = symId;
        this.value = value;
        this.kind = kind;
        this.data = data;
    }

    public String getScope() {
        return scope;
    }

    public void setScope(String scope) {
        this.scope = scope;
    }

    public String getSymId() {
        return symId;
    }

    public void setSymId(String symId) {
        this.symId = symId;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    public String getKind() {
        return kind;
    }

    public void setKind(String kind) {
        this.kind = kind;
    }

    public IData getData() {
        return data;
    }

    public void setData(IData data) {
        this.data = data;
    }
}
