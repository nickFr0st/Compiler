package project;

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
    private IData data;         // kind specific data
    private int size;           // size of element

    public Symbol(String scope, String symId, String value, String kind, IData data, int size) {
        this.scope = scope;
        this.symId = symId;
        this.value = value;
        this.kind = kind;
        this.data = data;
        this.size = size;
    }

    /**
     * returns the scope of the symbol
     * @return scope
     */
    public String getScope() {
        return scope;
    }

    /**
     * returns the symbol id (same as the symbolTable key)
     * @return symId
     */
    public String getSymId() {
        return symId;
    }

    /**
     * returns the name or lexi of the symbol element
     * @return value
     */
    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    /**
     * returns the element type "Class, Variable, etc."
     * @return kind
     */
    public String getKind() {
        return kind;
    }

    /**
     * if symbol is a class then returns null, else this
     * returns the symbols data object weather it is a VariableData
     * or a MethodData
     *
     * @return data
     */
    public IData getData() {
        return data;
    }

    /**
     * returns the number of memory blocks used by the symbol element
     * (may not be used)
     *
     * @return size
     */
    public Integer getSize() {
        return size;
    }

    public void setSize(int size) {
        this.size = size;
    }
}
