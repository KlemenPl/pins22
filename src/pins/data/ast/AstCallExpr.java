package pins.data.ast;

import pins.common.report.*;
import pins.data.ast.visitor.AstVisitor;

/**
 * A call expression.
 */
public class AstCallExpr extends AstNameExpr implements AstName {
	
	public final ASTs<AstExpr> args;

	public AstCallExpr(Location location, String name, ASTs<AstExpr> args) {
		super(location, name);
		this.args = args;
	}

	@Override
	public void log(String pfx) {
		System.out.println(pfx + "\033[1mAstCallExpr(" + name + ")\033[0m @(" + location + ")");
		logAttributes(pfx);
		System.out.println(pfx + "  {Args}");
		args.log(pfx + "  ");
	}

	@Override
	public <Result, Arg> Result accept(AstVisitor<Result, Arg> visitor, Arg arg) {
		return visitor.visit(this, arg);
	}

}
