package flo;

/**
 * Since Java is stupid and doesn't have a built in tuple type
 */
public class Triple<T1, T2, T3> {
	public final T1 x;
	public final T2 y;
	public final T3 z;

	public Triple(final T1 x, final T2 y, final T3 z) {
		this.x = x;
		this.y = y;
		this.z = z;
	}
}
