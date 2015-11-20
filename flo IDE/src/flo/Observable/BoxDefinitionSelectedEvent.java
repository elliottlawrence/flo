package flo.Observable;

import flo.floGraph.BoxDefinition;

public class BoxDefinitionSelectedEvent {

	public BoxDefinition boxDefinition;

	public BoxDefinitionSelectedEvent(final BoxDefinition bd) {
		boxDefinition = bd;
	}
}
