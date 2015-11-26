package flo.floGraph;

/**
 * A cable connects the output of one box to the input of another.
 * Alternatively, a cable can start from a single output, indicating that the
 * other end will be connected to the output of the surrounding function.
 */
public class Cable {

	private final boolean hasStartOnly;

	private final int startID;

	/**
	 * Valid if !hasStartOnly
	 */
	private final int endID;
	private final Input endInput;

	public Cable(final int startID, final int endID, final Input endInput) {
		this.startID = startID;
		this.endID = endID;
		this.endInput = endInput;

		hasStartOnly = false;
	}

	public boolean getHasStartOnly() {
		return hasStartOnly;
	}

	public int getStartID() {
		return startID;
	}

	public int getEndID() {
		return endID;
	}

	public Input getEndInput() {
		return endInput;
	}
}
