package flo.floGraph;

/**
 * An input to a function has a name and a type.
 */
public class Input {

	private String name;
	private Type type;

	/**
	 * The box interface in which this input is defined
	 */
	private final BoxInterface parent;

	/**
	 * The cable attached to this input, if there is one
	 */
	private Cable cable;

	public Input(final String name, final BoxInterface parent) {
		this.name = name;
		this.parent = parent;
	}

	public String getName() {
		return name;
	}

	public void setName(final String name) {
		this.name = name;
	}

	public Type getType() {
		return type;
	}

	public BoxInterface getParent() {
		return parent;
	}

	public boolean hasCable() {
		return cable != null;
	}

	public Cable getCable() {
		return cable;
	}

	public void setCable(final Cable cable) {
		this.cable = cable;
	}
}
