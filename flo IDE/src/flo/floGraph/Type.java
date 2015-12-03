package flo.floGraph;

/**
 * A type can either be an atomic, raw type, or a function from one type to
 * another.
 */
public class Type {

    @SuppressWarnings("unused")
    private boolean isRawType;

    // Valid if isRawType
    private String name;

    // Valid if !isRawType
    private Type type1;
    private Type type2;

    public Type() {

    }

    public String getName() {
        return name;
    }

    public Type getType1() {
        return type1;
    }

    public Type getType2() {
        return type2;
    }
}
