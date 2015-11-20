package flo.Observable;

import flo.floGraph.Module;

public class ModuleAddedEvent {

	public Module module;

	public ModuleAddedEvent(final Module m) {
		module = m;
	}

}
