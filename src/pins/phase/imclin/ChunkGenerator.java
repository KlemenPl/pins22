package pins.phase.imclin;

import java.util.*;

import pins.data.lin.*;
import pins.phase.imcgen.ImcGen;
import pins.data.ast.*;
import pins.data.ast.visitor.*;
import pins.data.mem.*;
import pins.data.imc.code.expr.*;
import pins.data.imc.code.stmt.*;
import pins.phase.memory.*;

public class ChunkGenerator extends AstFullVisitor<Object, Object> {

	@Override
	public Object visit(AstFunDecl funDecl, Object arg) {
		funDecl.expr.accept(this, arg);

		MemFrame frame = Memory.frames.get(funDecl);
		MemLabel entryLabel = new MemLabel();
		MemLabel exitLabel = new MemLabel();
		
		Vector<ImcStmt> canonStmts = new Vector<ImcStmt>();
		canonStmts.add(new ImcLABEL(entryLabel));
		ImcExpr bodyExpr = ImcGen.exprImc.get(funDecl.expr);
		//ImcStmt bodyStmt = new ImcMOVE(new ImcTEMP(frame.RV), bodyExpr);
		ImcStmt bodyStmt = ((ImcSEXPR)bodyExpr).stmt;
		canonStmts.addAll(bodyStmt.accept(new StmtCanonizer(), null));
		canonStmts.add(new ImcJUMP(exitLabel));
		
		Vector<ImcStmt> linearStmts = linearize (canonStmts);
		ImcLin.addCodeChunk(new LinCodeChunk(frame, linearStmts, entryLabel, exitLabel));
		
		return null;
	}

	@Override
	public Object visit(AstVarDecl varDecl, Object arg) {
		MemAccess access = Memory.varAccesses.get(varDecl);
		if (access instanceof MemAbsAccess) {
			MemAbsAccess absAccess = (MemAbsAccess) access;
			ImcLin.addDataChunk(new LinDataChunk(absAccess));
		}
		return null;
	}
	
	private Vector<ImcStmt> linearize(Vector<ImcStmt> stmts) {
		Vector<ImcStmt> linearStmts = new Vector<ImcStmt>();
		for (int s = 0; s < stmts.size(); s++) {
			ImcStmt stmt = stmts.get(s);
			if (stmt instanceof ImcCJUMP) {
				ImcCJUMP imcCJump = (ImcCJUMP)stmt;
				MemLabel negLabel = new MemLabel();
				linearStmts.add(new ImcCJUMP(imcCJump.cond, imcCJump.posLabel, negLabel));
				linearStmts.add(new ImcLABEL(negLabel));
				linearStmts.add(new ImcJUMP(imcCJump.negLabel));
			}
			else
				linearStmts.add(stmt);
		}
		return linearStmts;
	}

}
