package pins.data.ast;

import pins.common.report.*;
import pins.data.ast.visitor.AstVisitor;

/**
 * An expression containing a list of statements.
 */
public class AstStmtExpr extends AstExpr {

	public final ASTs<AstStmt> stmts;

	public AstStmtExpr(Location location, ASTs<AstStmt> stmts) {
		super(location);
		this.stmts = stmts;
	}

	@Override
	public void log(String pfx) {
		System.out.println(pfx + "\033[1mAstStmtExpr\033[0m @(" + location + ")");
		logAttributes(pfx);
		System.out.println(pfx + "  {Stmts}");
		stmts.log(pfx + "    ");
	}

	@Override
	public <Result, Arg> Result accept(AstVisitor<Result, Arg> visitor, Arg arg) {
		return visitor.visit(this, arg);
	}

}
