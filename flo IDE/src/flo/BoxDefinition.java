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
public class BoxDefinition {
	
	BoxInterface boxInterface;
	Map<Integer, BoxInterface> boxes;
	ArrayList<Cable> cables;
	ArrayList<BoxDefinition> localDefinitions;
	
	public BoxDefinition() {
		boxes = new HashMap<Integer, BoxInterface>();
		cables = new ArrayList<Cable>();
		localDefinitions = new ArrayList<BoxDefinition>();
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
	
	public ArrayList<BoxDefinition> getLocalDefinitions() {
		return localDefinitions;
	}

}
