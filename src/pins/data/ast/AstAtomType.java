package pins.data.ast;

import pins.common.report.*;
import pins.data.ast.visitor.AstVisitor;

/**
 * An atom type.
 */
public class AstAtomType extends AstType {

	public enum Kind {
		VOID, CHAR, INT
	};

	public final Kind kind;

	public AstAtomType(Location location, Kind kind) {
		super(location);
		this.kind = kind;
	}

	@Override
	public void log(String pfx) {
		System.out.println(pfx + "\033[1mAstAtomType(" + kind + ")\033[0m @(" + location + ")");
		logAttributes(pfx);
	}

	@Override
	public <Result, Arg> Result accept(AstVisitor<Result, Arg> visitor, Arg arg) {
		return visitor.visit(this, arg);
	}

}
