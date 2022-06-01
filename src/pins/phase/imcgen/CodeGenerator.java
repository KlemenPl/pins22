package pins.phase.imcgen;

import java.util.*;

import pins.data.ast.AstFunDecl;
import pins.data.ast.visitor.*;
import pins.data.imc.code.expr.ImcCONST;
import pins.data.imc.code.expr.ImcExpr;
import pins.data.imc.code.expr.ImcSEXPR;
import pins.data.imc.code.expr.ImcTEMP;
import pins.data.imc.code.stmt.ImcESTMT;
import pins.data.imc.code.stmt.ImcMOVE;
import pins.data.mem.*;
import pins.phase.memory.Memory;

public class CodeGenerator extends AstFullVisitor<Void, Stack<MemFrame>> {
    // Can directly use new/del as function names, because they are
    // reserved keywords and cannot be used.
    protected static final MemLabel funNewLabel = new MemLabel("new");
    protected static final MemLabel funDelLabel = new MemLabel("del");

    @Override
    public Void visit(AstFunDecl funDecl, Stack<MemFrame> memFrames) {
        if (memFrames == null) memFrames = new Stack<>();

        MemFrame funFrame = Memory.frames.get(funDecl);
        memFrames.push(funFrame);
        //ImcExpr fun = funDecl.expr.accept(new ExprGenerator(), memFrames);
        //ImcGen.exprImc.put(funDecl.expr, fun);

        // TODO: Should this be added here, or in the next (linearization) phase?
        ImcExpr funRV = funDecl.expr.accept(new ExprGenerator(), memFrames);
        ImcTEMP rvTEMP = new ImcTEMP(funFrame.RV);
        ImcMOVE writeRV = new ImcMOVE(rvTEMP, funRV);
        ImcGen.exprImc.put(funDecl.expr, new ImcSEXPR(writeRV, rvTEMP));
        ImcGen.exprImc.put(funDecl.expr, new ImcSEXPR(writeRV, new ImcCONST(0)));
        memFrames.pop();

        return null;
    }
}
