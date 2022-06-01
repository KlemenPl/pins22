package pins.data.imc.code.expr;

import java.util.*;

import pins.data.imc.visitor.*;
import pins.data.mem.*;

/**
 * Function call.
 * 
 * Evaluates arguments (the static link must be included) from left to right,
 * calls the function denoted by the label provided and returns the function's
 * result.
 */
public class ImcCALL extends ImcExpr {

	/** The label of the function. */
	public final MemLabel label;

	/** The offsets of arguments. */
	public final Vector<Long> offs;

	/** The values of arguments. */
	public final Vector<ImcExpr> args;

	/**
	 * Constructs a function call.
	 * 
	 * @param label The label of the function.
	 * @param offs  The offsets of arguments.
	 * @param args  The values of arguments.
	 */
	public ImcCALL(MemLabel label, Vector<Long> offs, Vector<ImcExpr> args) {
		this.label = label;
		this.offs = new Vector<Long>(offs);
		this.args = new Vector<ImcExpr>(args);
	}

	@Override
	public <Result, Arg> Result accept(ImcVisitor<Result, Arg> visitor, Arg accArg) {
		return visitor.visit(this, accArg);
	}

	@Override
	public void log(String pfx) {
		System.out.println(pfx + "CALL(" + label + ")");
		for (int a = 0; a < args.size(); a++)
			args.get(a).log(pfx + "  ");
	}

	@Override
	public String toString() {
		StringBuffer buffer = new StringBuffer();
		buffer.append("CALL(");
		buffer.append(label);
		for (int a = 0; a < args.size(); a++) {
			buffer.append(",");
			buffer.append(offs.get(a).toString());
			buffer.append(":");
			buffer.append(args.get(a).toString());
		}
		buffer.append(")");
		return buffer.toString();
	}

}
