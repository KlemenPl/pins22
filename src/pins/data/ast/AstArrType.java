package pins.data.ast;

import pins.common.report.*;
import pins.data.ast.visitor.AstVisitor;

/**
 * An array type.
 */
public class AstArrType extends AstType {

	public final AstType elemType;

	public final AstExpr size;

	public AstArrType(Location location, AstType elemType, AstExpr size) {
		super(location);
		this.elemType = elemType;
		this.size = size;
	}

	@Override
	public void log(String pfx) {
		System.out.println(pfx + "\033[1mAstArrType\033[0m @(" + location + ")");
		logAttributes(pfx);
		elemType.log(pfx + "  ");
		size.log(pfx + "  ");
	}

	@Override
	public <Result, Arg> Result accept(AstVisitor<Result, Arg> visitor, Arg arg) {
		return visitor.visit(this, arg);
	}

}
