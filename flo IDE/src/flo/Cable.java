package flo;

/**
 * A cable connects the output of one box to the input of another.
 * Alternatively, a cable can start from a single output, indicating
 * that the other end will be connected to the output of the
 * surrounding function.
 */
public abstract class Cable {
	
	private boolean hasStartOnly;

	private int startID;
	
	/**
	 * Valid if !hasStartOnly
	 */
	private int endID;
	private Input endInput;
	
	public Cable() {
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
