package pins.data.ast.visitor;

import pins.common.report.*;
import pins.data.ast.*;

/**
 * Abstract syntax tree visitor.
 * 
 * @param <Result> The result type.
 * @param <Arg>    The argument type.
 */
public interface AstVisitor<Result, Arg> {

	// GENERAL PURPOSE

	public default Result visit(ASTs<? extends AST> trees, Arg arg) {
		throw new Report.InternalError();
	}

	// DECLARATIONS

	public default Result visit(AstFunDecl funDecl, Arg arg) {
		throw new Report.InternalError();
	}

	public default Result visit(AstParDecl parDecl, Arg arg) {
		throw new Report.InternalError();
	}

	public default Result visit(AstTypDecl typDecl, Arg arg) {
		throw new Report.InternalError();
	}

	public default Result visit(AstVarDecl varDecl, Arg arg) {
		throw new Report.InternalError();
	}

	// EXPRESSIONS

	public default Result visit(AstBinExpr binExpr, Arg arg) {
		throw new Report.InternalError();
	}

	public default Result visit(AstCallExpr callExpr, Arg arg) {
		throw new Report.InternalError();
	}

	public default Result visit(AstCastExpr castExpr, Arg arg) {
		throw new Report.InternalError();
	}

	public default Result visit(AstConstExpr constExpr, Arg arg) {
		throw new Report.InternalError();
	}
	
	public default Result visit(AstNameExpr nameExpr, Arg arg) {
		throw new Report.InternalError();
	}

	public default Result visit(AstPreExpr preExpr, Arg arg) {
		throw new Report.InternalError();
	}

	public default Result visit(AstPstExpr pstExpr, Arg arg) {
		throw new Report.InternalError();
	}

	public default Result visit(AstStmtExpr stmtExpr, Arg arg) {
		throw new Report.InternalError();
	}

	public default Result visit(AstWhereExpr whereExpr, Arg arg) {
		throw new Report.InternalError();
	}

	// STATEMENTS

	public default Result visit(AstAssignStmt assignStmt, Arg arg) {
		throw new Report.InternalError();
	}

	public default Result visit(AstExprStmt exprStmt, Arg arg) {
		throw new Report.InternalError();
	}

	public default Result visit(AstIfStmt ifStmt, Arg arg) {
		throw new Report.InternalError();
	}

	public default Result visit(AstWhileStmt whileStmt, Arg arg) {
		throw new Report.InternalError();
	}

	// TYPES

	public default Result visit(AstArrType arrType, Arg arg) {
		throw new Report.InternalError();
	}

	public default Result visit(AstAtomType atomType, Arg arg) {
		throw new Report.InternalError();
	}

	public default Result visit(AstPtrType ptrType, Arg arg) {
		throw new Report.InternalError();
	}

	public default Result visit(AstTypeName typeName, Arg arg) {
		throw new Report.InternalError();
	}

}
