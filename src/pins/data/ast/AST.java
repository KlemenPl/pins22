package pins.data.ast;

import pins.common.logger.*;
import pins.common.report.*;
import pins.data.ast.visitor.*;
import pins.data.imc.code.expr.ImcExpr;
import pins.data.imc.code.stmt.ImcStmt;
import pins.data.typ.*;
import pins.phase.imcgen.ImcGen;
import pins.phase.seman.*;
import pins.data.mem.*;
import pins.phase.memory.*;

/**
 * An abstract syntax tree.
 */
public abstract class AST implements Loggable {

	private static int count = 0;

	public final int id;

	public final Location location;

	public AST(Location location) {
		this.location = location;
		id = count++;
	}

	protected void logAttributes(String pfx) {
		if (this instanceof AstName) {
			AstDecl decl = SemAn.declaredAt.get((AstName) this);
			if (decl != null)
				System.out.println(pfx + "  declaredAt: " + decl.location);
		}
		if (this instanceof AstTypDecl) {
			SemName name = SemAn.declaresType.get((AstTypDecl) this);
			if (name != null) {
				System.out.println(pfx + "  declaresType:");
				name.log(pfx + "    ");
				name.type().log(pfx + "    ");
			}
		}
		if (this instanceof AstType) {
			SemType type = SemAn.describesType.get((AstType) this);
			if (type != null) {
				System.out.println(pfx + "  describesType:");
				type.log(pfx + "    ");
			}
		}
		if (this instanceof AstExpr) {
			SemType type = SemAn.exprOfType.get((AstExpr) this);
			if (type != null) {
				System.out.println(pfx + "  exprOfType:");
				type.log(pfx + "    ");
			}
		}
		if (this instanceof AstStmt) {
			SemType type = SemAn.stmtOfType.get((AstStmt) this);
			if (type != null) {
				System.out.println(pfx + "  stmtOfType:");
				type.log(pfx + "    ");
			}
		}
		if (this instanceof AstFunDecl) {
			MemFrame frame = Memory.frames.get((AstFunDecl) this);
			if (frame != null)
				frame.log(pfx + "    ");
		}
		if (this instanceof AstVarDecl) {
			MemAccess access = Memory.varAccesses.get((AstVarDecl) this);
			if (access != null)
				access.log(pfx + "    ");
		}
		if (this instanceof AstParDecl) {
			MemRelAccess access = Memory.parAccesses.get((AstParDecl) this);
			if (access != null)
				access.log(pfx + "    ");
		}

		if (this instanceof AstExpr) {
			ImcExpr imcExpr = ImcGen.exprImc.get((AstExpr) this);
			if (imcExpr != null)
				imcExpr.log(pfx + "    ");
		}
		if (this instanceof AstStmt) {
			ImcStmt imcStmt = ImcGen.stmtImc.get((AstStmt) this);
			if (imcStmt != null)
				imcStmt.log(pfx + "    ");
		}

	}

	public abstract <Result, Arg> Result accept(AstVisitor<Result, Arg> visitor, Arg arg);

}
