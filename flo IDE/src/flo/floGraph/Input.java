package flo.floGraph;

/**
 * An input to a function has a name and a type.
 */
public class Input {

	private final String name;
	private Type type;

	public Input(final String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}

	public Type getType() {
		return type;
	}
}
