package flo;

/**
 * Represents the different type of changes that can occur to a Flo Graph so
 * listeners can have more information.
 */
public enum FloGraphChange {
	ModuleAdded, ModuleRemoved, ModuleRenamed, BoxDefinitionAdded, BoxDefinitionRemoved, BoxInterfaceRenamed, BoxDefinitionSelected, BoxAdded
}
