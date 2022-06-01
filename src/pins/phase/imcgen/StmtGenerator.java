package pins.phase.imcgen;

import java.util.*;

import pins.data.ast.*;
import pins.data.ast.visitor.*;
import pins.data.imc.code.ImcInstr;
import pins.data.imc.code.expr.ImcExpr;
import pins.data.imc.code.stmt.*;
import pins.data.mem.*;

public class StmtGenerator implements AstVisitor<ImcStmt, Stack<MemFrame>> {

    @Override
    public ImcStmt visit(AstAssignStmt assignStmt, Stack<MemFrame> frames) {
        ImcStmt code = new ImcMOVE(assignStmt.fstSubExpr.accept(new ExprGenerator(), frames),
                assignStmt.sndSubExpr.accept(new ExprGenerator(), frames));
        ImcGen.stmtImc.put(assignStmt, code);
        return code;
    }

    @Override
    public ImcStmt visit(AstExprStmt exprStmt, Stack<MemFrame> memFrames) {
        ImcESTMT imcESTMT = new ImcESTMT(exprStmt.expr.accept(new ExprGenerator(), memFrames));
        ImcGen.stmtImc.put(exprStmt, imcESTMT);
        return imcESTMT;
    }


    @Override
    public ImcStmt visit(AstIfStmt ifStmt, Stack<MemFrame> memFrames) {
        // 1. if C then S1 else S2 end;
        //
        // condLabel (not needed):
        //   eval(C)
        //     cond : 0 -> JUMP elseLabel
        //     JUMP thenLabel
        // thenLabel:
        //   exec(S1)
        //   JUMP endLabel
        // elseLabel:
        //   exec(S2)
        //   JUMP endLabel
        // endLabel:
        //
        //
        // 2. if C then S end;
        // condLabel:
        //   eval(C)
        //     cond : 0 -> JUMP endLabel
        //     JUMP thenLabel
        // thenLabel:
        //   exec(S)
        //   JUMP endLabel
        // endLabel:

        ImcExpr cond = ifStmt.condExpr.accept(new ExprGenerator(), memFrames);
        ImcStmt thenBody = ifStmt.thenBodyStmt.accept(this, memFrames);
        ImcStmt elseBody = ifStmt.elseBodyStmt != null ? ifStmt.elseBodyStmt.accept(this, memFrames) : null;
        MemLabel thenLabel = new MemLabel();
        MemLabel elseLabel = new MemLabel();
        MemLabel endLabel = new MemLabel();

        Vector<ImcStmt> stmts = new Vector<>(elseBody == null ? 5 : 8);
        stmts.add(new ImcCJUMP(cond, thenLabel, elseBody == null ? endLabel : elseLabel));
        stmts.add(new ImcLABEL(thenLabel));
        stmts.add(thenBody);
        stmts.add(new ImcJUMP(endLabel));
        if (elseBody != null) {
            stmts.add(new ImcLABEL(elseLabel));
            stmts.add(elseBody);
            stmts.add(new ImcJUMP(endLabel));
        }
        stmts.add(new ImcLABEL(endLabel));

        return new ImcSTMTS(stmts);
    }

    @Override
    public ImcStmt visit(AstWhileStmt whileStmt, Stack<MemFrame> memFrames) {
        // while C do S end;
        //
        // condLabel:
        //   eval(C)
        //     cond : 0 -> JUMP endLabel
        //     JUMP bodyLabel
        // bodyLabel:
        //   exec(S)
        //   JUMP condLabel
        // endLabel: (finish)

        ImcExpr  cond = whileStmt.condExpr.accept(new ExprGenerator(), memFrames);
        ImcStmt  body = whileStmt.bodyStmt.accept(this, memFrames);
        MemLabel bodyLabel = new MemLabel();
        MemLabel condLabel = new MemLabel();
        MemLabel endLabel = new MemLabel();

        Vector<ImcStmt> stmts = new Vector<>(6);
        stmts.add(new ImcLABEL(condLabel));
        stmts.add(new ImcCJUMP(cond, bodyLabel, endLabel));
        stmts.add(new ImcLABEL(bodyLabel));
        stmts.add(body);
        stmts.add(new ImcJUMP(condLabel));
        stmts.add(new ImcLABEL(endLabel));

        ImcSTMTS imcSTMTS = new ImcSTMTS(stmts);
        ImcGen.stmtImc.put(whileStmt, imcSTMTS);
        return imcSTMTS;
    }
}
