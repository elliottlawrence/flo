package flo.floGraph;

import java.util.ArrayList;

import javax.json.Json;
import javax.json.JsonObjectBuilder;

import flo.Util.Jsonable;

/**
 * Outputs are unnamed but have types nonetheless.
 */
public class Output implements Jsonable {

	private Type type;

	private final ArrayList<Cable> cables = new ArrayList<Cable>();

	/**
	 * The box interface in which this input is defined
	 */
	private final BoxInterface parent;

	public Output(final BoxInterface parent) {
		this.parent = parent;
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

	/**
	 * Convert this output to JSON
	 */
	@Override
	public JsonObjectBuilder toJsonObjectBuilder() {
		return Json.createObjectBuilder().add("parentID", parent.getID());
	}
}
