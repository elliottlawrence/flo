package flo.floGraph;

import java.util.regex.Pattern;

/**
 * Boxes are a generic representation of functions, constructors, and literals.
 */
public enum BoxFlavor {
    Function, Constructor, Literal;

    // Regexes for the supported literals

    private static final Pattern litString = Pattern.compile("^\".*\"$");
    private static final Pattern litChar =
        Pattern.compile("^'[^\\']'$|^'\\\\['trn]'$|^'\\\\\\\\'$");
    private static final Pattern litInt = Pattern.compile("^[0-9]+$");
    private static final Pattern litFloat =
        Pattern.compile("^[0-9]*\\.[0-9]+$");

    /**
     * Given a box name, determine if it matches a regex for a literal. If not,
     * the box represents a function.
     *
     * @param name
     * @return the box flavor associated with this box name
     */
    public static BoxFlavor getBoxFlavor(final String name) {
        if (litString.matcher(name).matches() || litChar.matcher(name).matches()
            || litInt.matcher(name).matches()
            || litFloat.matcher(name).matches())
            return Literal;
        return Function;
    }
}
