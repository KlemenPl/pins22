package pins.data.imc.code.stmt;

import pins.data.imc.visitor.*;
import pins.data.mem.*;

/**
 * Label.
 * 
 * Does nothing.
 */
public class ImcLABEL extends ImcStmt {

	/** The label. */
	public MemLabel label;

	/**
	 * Constructs a label.
	 * 
	 * @param label The label.
	 */
	public ImcLABEL(MemLabel label) {
		this.label = label;
	}

	@Override
	public <Result, Arg> Result accept(ImcVisitor<Result, Arg> visitor, Arg accArg) {
		return visitor.visit(this, accArg);
	}

	@Override
	public void log(String pfx) {
		System.out.println(pfx + "LABEL(" + label +")");
	}

	@Override
	public String toString() {
		return "LABEL(" + label + ")";
	}

}
