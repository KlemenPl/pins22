package pins.data.ast;

import pins.common.report.*;
import pins.data.ast.visitor.AstVisitor;

/**
 * A variable declaration.
 */
public class AstVarDecl extends AstDecl {

	public AstVarDecl(Location location, String name, AstType type) {
		super(location, name, type);
	}

	@Override
	public void log(String pfx) {
		System.out.println(pfx + "\033[1mAstVarDecl(" + name + ")\033[0m @(" + location + ")");
		type.log(pfx + "  ");
	}

	@Override
	public <Result, Arg> Result accept(AstVisitor<Result, Arg> visitor, Arg arg) {
		return visitor.visit(this, arg);
	}

}
