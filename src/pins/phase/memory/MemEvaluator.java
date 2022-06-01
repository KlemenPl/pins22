package pins.phase.memory;

import pins.data.ast.*;
import pins.data.ast.visitor.*;
import pins.data.mem.*;
import pins.data.typ.*;
import pins.phase.seman.*;

/**
 * Computing memory layout: frames and accesses.
 */
public class MemEvaluator extends AstFullVisitor<Object, MemEvaluator.FunContext> {

    /**
     * Functional context, i.e., used when traversing function and building a new
     * frame, parameter accesses and variable accesses.
     */
    protected static class FunContext {
        public int  depth    = 0;
        public long locsSize  = 0;
        public long varOffset = 0;
        public long argsSize  = 0;
        public long parsSize = new SemPtr(new SemVoid()).size();
    }


    public MemEvaluator() {
    }

    @Override
    public Object visit(AstFunDecl funDecl, FunContext funContext) {
        MemLabel label;
        if (funContext == null) {
            // Global
            label = new MemLabel(funDecl.name);
            funContext = new FunContext();
        } else {
            label = new MemLabel();
            int depth = funContext.depth;
            funContext = new FunContext();
            funContext.depth = depth + 1;
        }
        funDecl.pars.accept(this, funContext);
        funDecl.expr.accept(this, funContext);
        MemFrame frame = new MemFrame(label, funContext.depth, funContext.locsSize, funContext.argsSize);
        Memory.frames.put(funDecl, frame);
        return null;
    }

    @Override
    public Object visit(AstVarDecl varDecl, FunContext funContext) {
        if (funContext == null) {
            // Global
            MemAbsAccess access = new MemAbsAccess(SemAn.describesType.get(varDecl.type).size(), new MemLabel(varDecl.name));
            Memory.varAccesses.put(varDecl, access);
        } else {
            long size = SemAn.describesType.get(varDecl.type).size();
            funContext.varOffset -= size;
            funContext.locsSize += size;
            Memory.varAccesses.put(varDecl, new MemRelAccess(size, funContext.varOffset, funContext.depth + 1));
        }
        return null;
    }

    @Override
    public Object visit(AstParDecl parDecl, FunContext funContext) {
        long size = SemAn.describesType.get(parDecl.type).size();
        Memory.parAccesses.put(parDecl, new MemRelAccess(size, funContext.parsSize, funContext.depth + 1));
        funContext.parsSize += size;
        return null;
    }

    @Override
    public Object visit(AstCallExpr callExpr, FunContext funContext) {
        long argSize = 8;
        for (AstExpr arg : callExpr.args.asts()) {
            argSize += SemAn.exprOfType.get(arg).size();
        }
        funContext.argsSize = Math.max(funContext.argsSize, argSize);
        return super.visit(callExpr, funContext);
    }
}
