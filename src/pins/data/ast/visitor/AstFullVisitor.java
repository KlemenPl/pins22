package pins.data.ast.visitor;

import pins.data.ast.*;

/**
 * Abstract syntax tree visitor that traverses the abstract syntax tree.
 * 
 * @param <Result> The result type.
 * @param <Arg>    The argument type.
 */
public class AstFullVisitor<Result, Arg> implements AstVisitor<Result, Arg> {

	// GENERAL PURPOSE

	@Override
	public Result visit(ASTs<? extends AST> trees, Arg arg) {
		for (AST t : trees.asts())
			if (t != null)
				t.accept(this, arg);
		return null;
	}

	// DECLARATIONS

	@Override
	public Result visit(AstFunDecl funDecl, Arg arg) {
		if (funDecl.pars != null)
			funDecl.pars.accept(this, arg);
		if (funDecl.type != null)
			funDecl.type.accept(this, arg);
		if (funDecl.expr != null)
			funDecl.expr.accept(this, arg);
		return null;
	}

	@Override
	public Result visit(AstParDecl parDecl, Arg arg) {
		if (parDecl.type != null)
			parDecl.type.accept(this, arg);
		return null;
	}

	@Override
	public Result visit(AstTypDecl typDecl, Arg arg) {
		if (typDecl.type != null)
			typDecl.type.accept(this, arg);
		return null;
	}

	@Override
	public Result visit(AstVarDecl varDecl, Arg arg) {
		if (varDecl.type != null)
			varDecl.type.accept(this, arg);
		return null;
	}

	// EXPRESSIONS

	@Override
	public Result visit(AstBinExpr binExpr, Arg arg) {
		if (binExpr.fstSubExpr != null)
			binExpr.fstSubExpr.accept(this, arg);
		if (binExpr.sndSubExpr != null)
			binExpr.sndSubExpr.accept(this, arg);
		return null;
	}

	@Override
	public Result visit(AstCallExpr callExpr, Arg arg) {
		if (callExpr.args != null)
			callExpr.args.accept(this, arg);
		return null;
	}

	@Override
	public Result visit(AstCastExpr castExpr, Arg arg) {
		if (castExpr.subExpr != null)
			castExpr.subExpr.accept(this, arg);
		if (castExpr.type != null)
			castExpr.type.accept(this, arg);
		return null;
	}

	@Override
	public Result visit(AstConstExpr constExpr, Arg arg) {
		return null;
	}

	@Override
	public Result visit(AstNameExpr nameExpr, Arg arg) {
		return null;
	}

	@Override
	public Result visit(AstPreExpr preExpr, Arg arg) {
		if (preExpr.subExpr != null)
			preExpr.subExpr.accept(this, arg);
		return null;
	}

	@Override
	public Result visit(AstPstExpr pstExpr, Arg arg) {
		if (pstExpr.subExpr != null)
			pstExpr.subExpr.accept(this, arg);
		return null;
	}

	@Override
	public Result visit(AstStmtExpr stmtExpr, Arg arg) {
		if (stmtExpr.stmts != null)
			stmtExpr.stmts.accept(this, arg);
		return null;
	}

	@Override
	public Result visit(AstWhereExpr whereExpr, Arg arg) {
		if (whereExpr.subExpr != null)
			whereExpr.subExpr.accept(this, arg);
		if (whereExpr.decls != null)
			whereExpr.decls.accept(this, arg);
		return null;
	}

	// STATEMENTS

	@Override
	public Result visit(AstAssignStmt assignStmt, Arg arg) {
		if (assignStmt.fstSubExpr != null)
			assignStmt.fstSubExpr.accept(this, arg);
		if (assignStmt.sndSubExpr != null)
			assignStmt.sndSubExpr.accept(this, arg);
		return null;
	}

	@Override
	public Result visit(AstExprStmt exprStmt, Arg arg) {
		if (exprStmt.expr != null)
			exprStmt.expr.accept(this, arg);
		return null;
	}

	@Override
	public Result visit(AstIfStmt ifStmt, Arg arg) {
		if (ifStmt.condExpr != null)
			ifStmt.condExpr.accept(this, arg);
		if (ifStmt.thenBodyStmt != null)
			ifStmt.thenBodyStmt.accept(this, arg);
		if (ifStmt.elseBodyStmt != null)
			ifStmt.elseBodyStmt.accept(this, arg);
		return null;
	}

	@Override
	public Result visit(AstWhileStmt whileStmt, Arg arg) {
		if (whileStmt.condExpr != null)
			whileStmt.condExpr.accept(this, arg);
		if (whileStmt.bodyStmt != null)
			whileStmt.bodyStmt.accept(this, arg);
		return null;
	}

	// TYPES

	@Override
	public Result visit(AstArrType arrType, Arg arg) {
		if (arrType.elemType != null)
			arrType.elemType.accept(this, arg);
		if (arrType.size != null)
			arrType.size.accept(this, arg);
		return null;
	}

	@Override
	public Result visit(AstAtomType atomType, Arg arg) {
		return null;
	}

	@Override
	public Result visit(AstPtrType ptrType, Arg arg) {
		if (ptrType.subType != null)
			ptrType.subType.accept(this, arg);
		return null;
	}

	@Override
	public Result visit(AstTypeName typeName, Arg arg) {
		return null;
	}

}
