package pins.data.ast;

import pins.common.report.*;
import pins.data.ast.visitor.AstVisitor;

/*
 * A function declaration.
 */
public class AstFunDecl extends AstDecl {
	
	public final ASTs<AstParDecl> pars;
	
	public final AstExpr expr;

	public AstFunDecl(Location location, String name, ASTs<AstParDecl> pars, AstType type, AstExpr expr) {
		super(location, name, type);
		this.pars = pars;
		this.expr = expr;
	}

	@Override
	public void log(String pfx) {
		System.out.println(pfx + "\033[1mAstFunDecl(" + name + ")\033[0m @(" + location + ")");
		logAttributes(pfx);
		System.out.println(pfx + "  {Pars}");
		pars.log(pfx + "  ");
		type.log(pfx + "  ");
		expr.log(pfx + "  ");
	}

	@Override
	public <Result, Arg> Result accept(AstVisitor<Result, Arg> visitor, Arg arg) {
		return visitor.visit(this, arg);
	}

}
