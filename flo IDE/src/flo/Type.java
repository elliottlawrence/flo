package flo;

/**
 * A type can either be an atomic, raw type, or a function from
 * one type to another.
 */
public class Type {
	
	boolean isRawType;
	
	/**
	 * Valid if isRawType
	 */
	String name;
	
	/**
	 * Valid if !isRawType
	 */
	Type type1;
	Type type2;
	
	public Type() {
		
	}
	
	public String getName() {
		return name;
	}
	
	public Type getType1() {
		return type1;
	}
	
	public Type getType2() {
		return type2;
	}
}
