package pins.data.ast;

import pins.common.report.*;
import pins.data.ast.visitor.AstVisitor;

/**
 * A constant expression
 */
public class AstConstExpr extends AstExpr {

	public enum Kind {
		VOID, CHAR, INT, PTR
	};

	public final Kind kind;

	public final String name;

	public AstConstExpr(Location location, Kind kind, String name) {
		super(location);
		this.kind = kind;
		this.name = name;
	}

	@Override
	public void log(String pfx) {
		System.out.println(pfx + "\033[1mAstConstExpr(" + kind + "," + name + ")\033[0m @(" + location + ")");
		logAttributes(pfx);
	}

	@Override
	public <Result, Arg> Result accept(AstVisitor<Result, Arg> visitor, Arg arg) {
		return visitor.visit(this, arg);
	}

}
