package pins.data.ast;

import pins.common.report.*;
import pins.data.ast.visitor.AstVisitor;

/**
 * A pointer type.
 */
public class AstPtrType extends AstType {

	public final AstType subType;

	public AstPtrType(Location location, AstType subType) {
		super(location);
		this.subType = subType;
	}

	@Override
	public void log(String pfx) {
		System.out.println(pfx + "\033[1mAstPtrType\033[0m @(" + location + ")");
		logAttributes(pfx);
		subType.log(pfx + "  ");
	}

	@Override
	public <Result, Arg> Result accept(AstVisitor<Result, Arg> visitor, Arg arg) {
		return visitor.visit(this, arg);
	}

}
