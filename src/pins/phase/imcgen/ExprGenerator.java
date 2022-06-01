package pins.phase.imcgen;

import java.util.*;

import pins.common.report.Report;
import pins.data.ast.*;
import pins.data.ast.visitor.*;
import pins.data.imc.code.expr.*;
//import pins.data.imc.code.stmt.*;
import pins.data.imc.code.stmt.ImcESTMT;
import pins.data.imc.code.stmt.ImcMOVE;
import pins.data.imc.code.stmt.ImcSTMTS;
import pins.data.imc.code.stmt.ImcStmt;
import pins.data.mem.*;
import pins.data.typ.SemArr;
import pins.data.typ.SemChar;
import pins.data.typ.SemVoid;
import pins.phase.memory.Memory;
import pins.phase.seman.SemAn;
//import pins.data.typ.*;
//import pins.phase.memory.*;
//import pins.phase.seman.*;

public class ExprGenerator implements AstVisitor<ImcExpr, Stack<MemFrame>> {

    @Override
    public ImcExpr visit(AstWhereExpr whereExpr, Stack<MemFrame> frames) {
        whereExpr.decls.accept(new CodeGenerator(), frames);
        ImcExpr code = whereExpr.subExpr.accept(this, frames);
        ImcGen.exprImc.put(whereExpr, code);
        return code;
    }

     @Override
    public ImcExpr visit(AstCallExpr callExpr, Stack<MemFrame> memFrames) {
        Vector<AstExpr> astArgs = callExpr.args.asts();
        Vector<Long> offsets = new Vector<>(astArgs.size() + 1);
        Vector<ImcExpr> args = new Vector<>(astArgs.size() + 1);

        AstFunDecl funDecl = (AstFunDecl) SemAn.declaredAt.get(callExpr);
        Vector<AstParDecl> astFunPars = funDecl.pars.asts();
        MemFrame funFrame = Memory.frames.get(funDecl);

        // Calculate SL
        ImcExpr exprSL;
        if (funFrame.depth == 0) {
            // For globals it can be anything. We do not need SL.
            exprSL = new ImcCONST(-1);
        } else {
            exprSL = new ImcTEMP(memFrames.peek().FP);
            for (int i = funFrame.depth; i <= memFrames.peek().depth; ++i)
                exprSL = new ImcMEM(exprSL);
        }
        offsets.add(0L);
        args.add(exprSL);

        for (int i = 0; i < astArgs.size(); ++i) {
            AstExpr astExpr = astArgs.get(i);
            AstParDecl parDecl = astFunPars.get(i);

            ImcExpr imcExpr = astExpr.accept(this, memFrames);
            long offset = Memory.parAccesses.get(parDecl).offset;

            offsets.add(offset);
            args.add(imcExpr);
        }

        ImcCALL imcCALL = new ImcCALL(funFrame.label, offsets, args);
        ImcGen.exprImc.put(callExpr, imcCALL);
        return imcCALL;
    }

    @Override
    public ImcExpr visit(AstCastExpr castExpr, Stack<MemFrame> memFrames) {
        ImcExpr exp = castExpr.subExpr.accept(this, memFrames);
        if (SemAn.exprOfType.get(castExpr) instanceof SemChar) {
            // We only support 7bit ASCII.
            // So we need to clip value to that range (0..128)
            // NOTE: Casting value outside range ie: (1 : char) is
            // undefined behaviour.
            exp = new ImcBINOP(ImcBINOP.Oper.MOD, exp, new ImcCONST(256L));
        }

        ImcGen.exprImc.put(castExpr, exp);
        return exp;
    }

    @Override
    public ImcExpr visit(AstBinExpr binExpr, Stack<MemFrame> memFrames) {
        ImcExpr lhs     = binExpr.fstSubExpr.accept(this, memFrames);
        ImcExpr rhs     = binExpr.sndSubExpr.accept(this, memFrames);
        ImcExpr imcExpr = null;
        if (binExpr.oper == AstBinExpr.Oper.ARR) {
            // Example: t[n] (t: lhs, n: rhs); at global scope
            // First we need a pointer to the first element in array (NAME(_t)).
            // To get `n`-th element we need to add `n` multiplied by stride of base element.
            //
            // IMCode Representation:
            //
            //           MEM
            //            |
            //         BINOP(+)
            //        /        \
            //       /          \
            //   NAME(_t)     BINOP(*)
            //               /        \
            //              /          \
            //            NAME(_n)   CONST(elementStride)

            lhs = ((ImcMEM) lhs).addr; // Array pointer
            long     stride = SemAn.exprOfType.get(binExpr).size();
            ImcBINOP opMul  = new ImcBINOP(ImcBINOP.Oper.MUL, rhs, new ImcCONST(stride));
            ImcBINOP opAdd  = new ImcBINOP(ImcBINOP.Oper.ADD, lhs, opMul);

            imcExpr = new ImcMEM(opAdd);
        } else {
            ImcBINOP.Oper op = switch (binExpr.oper) {
                case AND -> ImcBINOP.Oper.AND;
                case OR -> ImcBINOP.Oper.OR;
                case EQU -> ImcBINOP.Oper.EQU;
                case NEQ -> ImcBINOP.Oper.NEQ;
                case LTH -> ImcBINOP.Oper.LTH;
                case GTH -> ImcBINOP.Oper.GTH;
                case LEQ -> ImcBINOP.Oper.LEQ;
                case GEQ -> ImcBINOP.Oper.GEQ;
                case MUL -> ImcBINOP.Oper.MUL;
                case DIV -> ImcBINOP.Oper.DIV;
                case MOD -> ImcBINOP.Oper.MOD;
                case ADD -> ImcBINOP.Oper.ADD;
                case SUB -> ImcBINOP.Oper.SUB;
                case ARR -> null;
            };

            imcExpr = new ImcBINOP(op, lhs, rhs);
        }
        ImcGen.exprImc.put(binExpr, imcExpr);
        return imcExpr;
    }

    @Override
    public ImcExpr visit(AstPreExpr preExpr, Stack<MemFrame> memFrames) {
        ImcExpr expr   = preExpr.subExpr.accept(this, memFrames);
        ImcExpr unExpr = null;
        switch (preExpr.oper) {
            case NEW, DEL:
                Vector<Long> offsets = new Vector<>(2);
                offsets.add(0L);
                offsets.add(8L);
                Vector<ImcExpr> args = new Vector<>(2);
                // For globals, SL can be any value.
                args.add(new ImcCONST(0xDEADBEEF));
                args.add(expr);
                MemLabel label = preExpr.oper == AstPreExpr.Oper.NEW ?
                        CodeGenerator.funNewLabel : CodeGenerator.funDelLabel;
                ImcCALL call = new ImcCALL(label, offsets, args);
                unExpr = call;
                break;
            case ADD:
                unExpr = expr;
                break;
            case NOT:
                unExpr = new ImcUNOP(ImcUNOP.Oper.NOT, expr);
                break;
            case SUB:
                unExpr = new ImcUNOP(ImcUNOP.Oper.NEG, expr);
                break;
            case PTR:
                if (expr instanceof ImcMEM imcMEM)
                    expr = imcMEM.addr;
                unExpr = expr;
                break;
        }
        ImcGen.exprImc.put(preExpr, unExpr);
        return unExpr;
    }

    @Override
    public ImcExpr visit(AstPstExpr pstExpr, Stack<MemFrame> memFrames) {
        ImcExpr expr = pstExpr.subExpr.accept(this, memFrames);
        ImcExpr unExpr = null;

        if (pstExpr.oper == AstPstExpr.Oper.PTR) {
            // After we get address ('expr') we just need to
            // dereference it to get the value.
            unExpr = new ImcMEM(expr);
        } else assert false; // Only ptr implemented

        ImcGen.exprImc.put(pstExpr, unExpr);
        return unExpr;
    }


    /*
    private ImcExpr calculateAddress(ImcExpr imcExpr, MemRelAccess access, Stack<MemFrame> memFrames) {
        if (access.depth <= memFrames.peek().depth) {
            MemFrame frame = memFrames.pop();
            imcExpr = new ImcMEM(imcExpr);
            imcExpr = calculateAddress(imcExpr, access, memFrames);
            memFrames.push(frame);
        }
        return imcExpr;
    }
     */

    private ImcExpr calculateAddress(ImcExpr imcExpr, MemRelAccess access, Stack<MemFrame> memFrames) {
        for (int i = access.depth; i <= memFrames.peek().depth; ++i)
            imcExpr = new ImcMEM(imcExpr);
        return imcExpr;
    }

    @Override
    public ImcExpr visit(AstNameExpr nameExpr, Stack<MemFrame> memFrames) {
        // Example: a
        // Once we calculate address of `a` we simply need to dereference it
        // to get the value
        //
        // For each depth we need to dereference static link TEMP(FP).
        //
        // At global scope:
        //         MEM
        //          |
        //       NAME(_a)
        //
        // In global function (depth = 1):
        //          MEM
        //           |
        //        BINOP(+)
        //       /        \
        //      /          \
        //  TEMP(FP)   CONST(offset to a)
        //
        // In nested function (depth = 2):
        //          MEM
        //           |
        //        BINOP(+)
        //       /        \
        //      /          \
        //     MEM   CONST(offset to a)
        //      |
        //    TEMP(FP)

        AstDecl decl = SemAn.declaredAt.get(nameExpr);

        ImcExpr mem;

        if (decl instanceof AstVarDecl varDecl &&
                Memory.varAccesses.get(varDecl) instanceof MemAbsAccess absAccess) {
            // Global variable.
            ImcNAME name = new ImcNAME(absAccess.label);
            mem = new ImcMEM(name);
        } else {
            // Local or a parameter variable.
            MemRelAccess access = null;
            if (decl instanceof AstVarDecl varDecl) {
                access = (MemRelAccess) Memory.varAccesses.get(varDecl);
            } else if (decl instanceof AstParDecl parDecl) {
                access = Memory.parAccesses.get(parDecl);
            } else assert false;

            ImcCONST offset = new ImcCONST(access.offset);
            // First we need to get address to the variable.
            mem = calculateAddress(new ImcTEMP(memFrames.peek().FP),
                    access, memFrames);
            // Now we simply add offset to get the actual value.
            mem = new ImcMEM(new ImcBINOP(ImcBINOP.Oper.ADD, mem, offset));

        }

        ImcGen.exprImc.put(nameExpr, mem);
        return mem;
    }

    @Override
    public ImcExpr visit(AstConstExpr constExpr, Stack<MemFrame> memFrames) {
        ImcCONST imcCONST = switch (constExpr.kind) {
            case INT -> null;
            case PTR, VOID -> new ImcCONST(0);
            case CHAR -> new ImcCONST(constExpr.name.length() == 3 ? constExpr.name.charAt(1) : constExpr.name.charAt(2));
        };
        if (imcCONST == null) {
            try {
                imcCONST = new ImcCONST(Integer.parseInt(constExpr.name));
            } catch (NumberFormatException e) {
                throw new Report.Error("Invalid integer const: " + constExpr.name);
            }
        }

        ImcGen.exprImc.put(constExpr, imcCONST);
        return imcCONST;
    }


    @Override
    public ImcExpr visit(AstStmtExpr stmtExpr, Stack<MemFrame> memFrames) {
        Vector<AstStmt> astStmts = stmtExpr.stmts.asts();
        Vector<ImcStmt> stmts    = new Vector<>(astStmts.size());
        for (AstStmt stmt : astStmts)
            stmts.add(stmt.accept(new StmtGenerator(), memFrames));
        ImcStmt stmt = new ImcSTMTS(stmts);
        ImcExpr returnExpr;
        if (SemAn.exprOfType.get(stmtExpr) instanceof SemVoid) {
            returnExpr = new ImcCONST(0);
        } else {
            ImcStmt last = stmts.get(stmts.size() - 1);
            // If it is not void, it must be ImcESTMT.
            ImcESTMT imcESTMT = (ImcESTMT) last;
            returnExpr = imcESTMT.expr;
        }
        return new ImcSEXPR(stmt, returnExpr);
    }

}
