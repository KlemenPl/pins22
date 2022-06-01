package pins.data.mem;

/**
 * An access to a variable relative to an (unspecified) base address.
 */
public class MemRelAccess extends MemAccess {

	/** Offset of a variable relative to a base address. */
	public final long offset;

	/** The variable's static depth (0 for record components). */
	public final int depth;

	/**
	 * Constructs a new relative access.
	 * 
	 * @param size   The size of the variable.
	 * @param offset Offset of a variable relative to a base address.
	 * @param depth  The variable's static depth (0 for record components).
	 */
	public MemRelAccess(long size, long offset, int depth) {
		super(size);
		this.offset = offset;
		this.depth = depth;
	}

	@Override
	public String toString() {
		return "RelAccess(" + size + "," + offset + "," + depth + ")";
	}

	@Override
	public void log(String pfx) {
		System.out.println(pfx + toString());
	}

}
