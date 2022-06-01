package pins.data.typ;

/**
 * Pointer type.
 */
public class SemPtr extends SemType {

	/** Base type. */
	public final SemType baseType;

	/**
	 * Constructs a new pointer type.
	 * 
	 * @param baseType The base type.
	 */
	public SemPtr(SemType baseType) {
		this.baseType = baseType;
	}

	@Override
	public long size() {
		return 8;
	}

	@Override
	public void log(String pfx) {
		System.out.println(pfx + "\033[31mPtr\033[0m"); 
		baseType.log(pfx + "  ");
	}

}
