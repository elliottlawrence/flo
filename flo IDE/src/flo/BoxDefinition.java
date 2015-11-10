package flo;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/**
 * A box definition consists of the box's interface augmented with a
 * set of boxes and cables connecting nodes on the boxes. In addition,
 * a box may have local box definitions, akin to a let/where clause
 * in Haskell.
 */
public class BoxDefinition extends BoxDefinitionContainer {
	
	private BoxInterface boxInterface;
	private Map<Integer, BoxInterface> boxes;
	private ArrayList<Cable> cables;
	
	public BoxDefinition(String name) {
		boxInterface = new BoxInterface(name);
		boxes = new HashMap<Integer, BoxInterface>();
		cables = new ArrayList<Cable>();
		boxDefinitions = new ArrayList<BoxDefinition>();
	}
	
	public BoxInterface getBoxInterface() {
		return boxInterface;
	}
	
	public Map<Integer, BoxInterface> getBoxes() {
		return boxes;
	}
	
	public ArrayList<Cable> getCables() {
		return cables;
	}
}
