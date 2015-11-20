package flo.Observable;

import flo.floGraph.BoxDefinition;

public class BoxDefinitionAddedEvent {

	public BoxDefinition boxDefinition;

	public BoxDefinitionAddedEvent(final BoxDefinition bd) {
		boxDefinition = bd;
	}
}
