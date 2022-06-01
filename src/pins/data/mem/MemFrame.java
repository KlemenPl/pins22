package pins.data.mem;

import pins.common.logger.*;
import pins.data.typ.*;

/**
 * A stack frame.
 */
public class MemFrame implements Loggable {

	/** The function's entry label. */
	public final MemLabel label;

	/** The function's static depth. */
	public final int depth;

	/** The size of the frame. */
	public final long size;

	/** The size of the block of local variables within a frame. */
	public final long locsSize;

	/** The size of the block of arguments within a frame. */
	public final long argsSize;

	/** The register to hold the frame pointer. */
	public final MemTemp FP;

	/** The register to hold the return value. */
	public final MemTemp RV;

	/**
	 * Constructs a new frame with no temporary variables and no saved registers.
	 * 
	 * @param label    The function's entry label.
	 * @param depth    The function's static depth.
	 * @param locsSize The size of the block of local variables within a frame.
	 * @param argsSize The size of the block of arguments within a frame.
	 */
	public MemFrame(MemLabel label, int depth, long locsSize, long argsSize) {
		this.label = label;
		this.depth = depth;
		this.locsSize = locsSize;
		this.argsSize = argsSize;
		this.size = this.locsSize + 2 * (new SemPtr(new SemVoid())).size() + this.argsSize;
		this.FP = new MemTemp();
		this.RV = new MemTemp();
	}

	@Override
	public String toString() {
		return "Frame(" + label + "," + size + "=16+" + locsSize + "+" + argsSize + "," + FP + "," + RV + "," + depth + ")";
	}

	@Override
	public void log(String pfx) {
		System.out.println(pfx + toString());
	}

}
