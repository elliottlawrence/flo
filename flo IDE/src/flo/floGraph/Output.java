package flo.floGraph;

import java.util.ArrayList;

/**
 * Outputs are unnamed but have types nonetheless.
 */
public class Output {

	private Type type;

	private final ArrayList<Cable> cables = new ArrayList<Cable>();

	public Output() {
	}

	public Type getType() {
		return type;
	}

	public void addCable(final Cable cable) {
		cables.add(cable);
	}

	public boolean hasCable() {
		return cables.size() > 0;
	}

	public ArrayList<Cable> getCables() {
		return cables;
	}

	public void removeCable(final Cable cable) {
		cables.remove(cable);
	}
}
