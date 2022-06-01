package pins.phase.seman;

import pins.common.report.Report;
import pins.data.ast.*;
import pins.data.ast.visitor.AstFullVisitor;
import pins.data.typ.*;

import java.util.HashSet;
import java.util.Set;

public class TypeChecker extends AstFullVisitor<SemType, TypeChecker.Mode> {

    public enum Mode {
        RESOLVE_TYPE,
        CHECK_TYPE,
    }

    @Override
    public SemType visit(AstTypDecl typDecl, Mode mode) {
        if (mode == Mode.RESOLVE_TYPE) {
            SemAn.declaresType.put(typDecl, new SemName(typDecl.name));
        } else {
            SemType type = typDecl.type.accept(this, mode).actualType();
            SemAn.describesType.put(typDecl.type, type);
            SemAn.declaresType.get(typDecl).define(type);
        }
        return null;
    }

    @Override
    public SemType visit(AstFunDecl funDecl, Mode mode) {
        if (mode == Mode.RESOLVE_TYPE) {
            SemType type = funDecl.type.accept(this, Mode.CHECK_TYPE).actualType();
            SemAn.describesType.put(funDecl.type, type);
        } else {
            funDecl.pars.accept(this, mode);
            SemType returnType;
            if (SemAn.describesType.containsKey(funDecl.type))
                returnType = SemAn.describesType.get(funDecl.type).actualType();
            else {
                returnType = funDecl.expr.accept(this, mode);
                SemAn.describesType.put(funDecl.type, returnType);
            }
            SemType actualType = funDecl.expr.accept(this, mode).actualType();
            if (!isSameType(actualType, returnType))
                throw error(funDecl.expr, "Return type does not match function signature");
        }
        return SemAn.describesType.get(funDecl.type);
    }

    @Override
    public SemType visit(AstParDecl parDecl, Mode mode) {
        if (mode == Mode.CHECK_TYPE) {
            SemType type = parDecl.type.accept(this, mode).actualType();
            if (type instanceof SemVoid)
                throw error(parDecl.type, "Function parameter can not be void");
            SemAn.describesType.put(parDecl.type, type);
        }

        return SemAn.describesType.get(parDecl.type);
    }

    @Override
    public SemType visit(AstBinExpr binExpr, Mode mode) {
        if (mode == Mode.CHECK_TYPE) {
            SemType lhs = binExpr.fstSubExpr.accept(this, mode).actualType();
            SemType rhs = binExpr.sndSubExpr.accept(this, mode).actualType();

            switch (binExpr.oper) {
                case ADD:
                case SUB:
                case MUL:
                case DIV:
                case MOD:
                case OR:
                case AND:
                    if (!(lhs instanceof SemInt && rhs instanceof SemInt))
                        throw error(binExpr, "Both operands need to be integer");
                    SemAn.exprOfType.put(binExpr, new SemInt());
                    break;
                case EQU:
                case NEQ:
                case LTH:
                case LEQ:
                case GTH:
                case GEQ:
                    if (!isSameType(lhs, rhs))
                        throw error(binExpr, "Both operands need to be same type");
                    if (isTypeOf(lhs, SemArr.class))
                        throw error(binExpr, "Logical operator cannot be applied to array operands");
                    SemAn.exprOfType.put(binExpr, new SemInt());
                    break;
                case ARR:
                    if (!(lhs instanceof SemArr))
                        throw error(binExpr, "'arr'[expr] arr must be an array");
                    if (!(rhs instanceof SemInt))
                        throw error(binExpr, "arr['expr'] expr must be an integer");
                    SemArr arrType = (SemArr) lhs;
                    SemAn.exprOfType.put(binExpr, arrType.elemType);
                    break;
            }

        }
        return SemAn.exprOfType.get(binExpr);
    }

    @Override
    public SemType visit(AstPreExpr preExpr, Mode mode) {
        if (mode == Mode.CHECK_TYPE) {
            SemType type = preExpr.subExpr.accept(this, mode).actualType();

            switch (preExpr.oper) {
                case ADD:
                case SUB:
                case NOT:
                    if (!(type instanceof SemInt))
                        throw error(preExpr, "Operand needs to be integer");
                    SemAn.exprOfType.put(preExpr, new SemInt());
                    break;
                case NEW:
                    if (!(type instanceof SemInt))
                        throw error(preExpr, "new 'expr' - expr must be integer");
                    SemAn.exprOfType.put(preExpr, new SemPtr(new SemVoid()));
                    break;
                case DEL:
                    if (!((type) instanceof SemPtr))
                        throw error(preExpr, "del 'expr' - expr must be a pointer");
                    SemAn.exprOfType.put(preExpr, new SemVoid());
                    break;
                case PTR:
                    if (preExpr.subExpr instanceof AstConstExpr)
                        throw error(preExpr, "Cannot take address of const expression.");
                    SemAn.exprOfType.put(preExpr, new SemPtr(type));
                    break;
            }

        }
        return SemAn.exprOfType.get(preExpr);
    }

    @Override
    public SemType visit(AstPstExpr pstExpr, Mode mode) {
        if (mode == Mode.CHECK_TYPE) {
            SemType type = pstExpr.subExpr.accept(this, mode).actualType();

            // Only have PTR oper
            switch (pstExpr.oper) {
                case PTR:
                    if (!(type instanceof SemPtr))
                        throw error(pstExpr, "can not dereference a non pointer value");
                    SemAn.exprOfType.put(pstExpr, ((SemPtr) type).baseType);
                    break;
                default:
                    assert false;
            }

        }
        return SemAn.exprOfType.get(pstExpr);
    }

    @Override
    public SemType visit(AstCallExpr callExpr, Mode mode) {
        if (mode == Mode.CHECK_TYPE) {
            var funDecl = (AstFunDecl) SemAn.declaredAt.get(callExpr);
            if (funDecl.pars.asts().size() != callExpr.args.asts().size())
                throw error(callExpr.args, "Expected " + funDecl.pars.asts().size() +
                        "arguments, got: " + callExpr.args.asts().size());
            for (int i = 0; i < funDecl.pars.asts().size(); ++i) {
                var parType    = funDecl.pars.asts().get(i).type.accept(this, mode).actualType();
                var actualType = callExpr.args.asts().get(i).accept(this, mode).actualType();
                if (!isSameType(parType, actualType))
                    throw error(callExpr.args.asts().get(i), "Argument does not match function signature (invalid type).");
            }
            SemAn.exprOfType.put(callExpr, funDecl.type.accept(this, mode));

        }
        return SemAn.exprOfType.get(callExpr);
    }

    @Override
    public SemType visit(AstCastExpr castExpr, Mode mode) {
        if (mode == Mode.CHECK_TYPE) {
            SemType exprType = castExpr.subExpr.accept(this, mode).actualType();
            SemAn.exprOfType.put(castExpr.subExpr, exprType);
            SemType castType = castExpr.type.accept(this, mode).actualType();
            if (isTypeOf(castType, SemVoid.class))
                throw error(castExpr, "Cannot cast to void");
            SemAn.exprOfType.put(castExpr, castType);
        }
        return SemAn.exprOfType.get(castExpr);
    }

    @Override
    public SemType visit(AstConstExpr constExpr, Mode mode) {
        if (mode == Mode.CHECK_TYPE) {
            SemType type = null;
            switch (constExpr.kind) {
                case INT:
                    type = new SemInt();
                    break;
                case CHAR:
                    type = new SemChar();
                    break;
                case VOID:
                    type = new SemVoid();
                    break;
                case PTR:
                    if (constExpr.name.equals("nil")) type = new SemPtr(new SemVoid());
                    else type = new SemPtr(constExpr.accept(this, mode));
                    break;
                }
            SemAn.exprOfType.put(constExpr, type);
        }
        return SemAn.exprOfType.get(constExpr);
    }

    @Override
    public SemType visit(AstNameExpr nameExpr, Mode mode) {
        if (mode == Mode.CHECK_TYPE) {
            var decl = SemAn.declaredAt.get(nameExpr);
            var type = decl.type.accept(this, mode).actualType();
            SemAn.exprOfType.put(nameExpr, type);
        }
        return SemAn.exprOfType.get(nameExpr);
    }


    @Override
    public SemType visit(AstStmtExpr stmtExpr, Mode mode) {
        super.visit(stmtExpr, mode);
        if (mode == Mode.CHECK_TYPE) {
            SemType type = new SemVoid();
            if (stmtExpr.stmts.asts().size() > 0)
                type = stmtExpr.stmts.asts().get(stmtExpr.stmts.asts().size() - 1).accept(this, mode).actualType();
            SemAn.exprOfType.put(stmtExpr, type);
        }
        return SemAn.exprOfType.get(stmtExpr);
    }

    @Override
    public SemType visit(AstWhereExpr whereExpr, Mode mode) {
        super.visit(whereExpr, mode);
        if (mode == Mode.CHECK_TYPE) {
            SemType type = whereExpr.subExpr.accept(this, mode).actualType();
            SemAn.exprOfType.put(whereExpr, type);
        }
        return SemAn.exprOfType.get(whereExpr);
    }

    @Override
    public SemType visit(AstAssignStmt assignStmt, Mode mode) {
        super.visit(assignStmt, mode);
        if (mode == Mode.CHECK_TYPE) {
            var lvalueType = assignStmt.fstSubExpr.accept(this, mode).actualType();
            var rvalueType = assignStmt.sndSubExpr.accept(this, mode).actualType();
            if (!isSameType(lvalueType, rvalueType))
                throw error(assignStmt, "Type mismatch (cannot assign value to different type)");
            SemAn.stmtOfType.put(assignStmt, new SemVoid());
        }
        return SemAn.stmtOfType.get(assignStmt);
    }

    @Override
    public SemType visit(AstExprStmt exprStmt, Mode mode) {
        if (mode == Mode.CHECK_TYPE) {
            SemType type = exprStmt.expr.accept(this, mode).actualType();
            SemAn.stmtOfType.put(exprStmt, type);
        }
        return SemAn.stmtOfType.get(exprStmt);
    }

    @Override
    public SemType visit(AstIfStmt ifStmt, Mode mode) {
        super.visit(ifStmt, mode);
        if (mode == Mode.CHECK_TYPE) {
            SemType exprType = ifStmt.condExpr.accept(this, mode).actualType();
            if (!(exprType instanceof SemInt))
                throw error(ifStmt.condExpr, "Condition expression is not an integer");

            SemType thenType = ifStmt.thenBodyStmt.accept(this, mode).actualType();
            SemType elseType = ifStmt.elseBodyStmt != null ? ifStmt.elseBodyStmt.accept(this, mode).actualType() : null;

            if (!(thenType instanceof SemVoid))
                throw error(ifStmt.thenBodyStmt, "Then body is not void (returns non void value)");
            if (elseType != null && (!(elseType instanceof SemVoid)))
                throw error(ifStmt.elseBodyStmt, "Else body is not void (returns non void value)");
            SemAn.stmtOfType.put(ifStmt, new SemVoid());
        }
        return SemAn.stmtOfType.get(ifStmt);
    }

    @Override
    public SemType visit(AstWhileStmt whileStmt, Mode mode) {
        super.visit(whileStmt, mode);
        if (mode == Mode.CHECK_TYPE) {
            SemType exprType = whileStmt.condExpr.accept(this, mode).actualType();
            if (!(exprType instanceof SemInt))
                throw error(whileStmt.condExpr, "Condition expression is not an integer");

            SemType bodyType = whileStmt.bodyStmt.accept(this, mode).actualType();
            if (!(bodyType instanceof SemVoid))
                throw error(whileStmt.bodyStmt, "The body is not void (returns non void value)");
            SemAn.stmtOfType.put(whileStmt, new SemVoid());
        }
        return SemAn.stmtOfType.get(whileStmt);
    }

    @Override
    public SemType visit(AstArrType arrType, Mode mode) {
        if (mode == Mode.CHECK_TYPE) {
            SemType base = arrType.elemType.accept(this, mode);
            try {
                SemType sizeType = arrType.size.accept(this, mode).actualType();
                if (!(sizeType instanceof SemInt)) throw new Exception("");
                long numEl = Long.parseLong(((AstConstExpr) arrType.size).name);
                if (numEl <= 0) throw new Exception("");
                SemType type = new SemArr(base, numEl).actualType();
                SemAn.describesType.put(arrType, type);
            } catch (Exception e) {
                throw error(arrType.size, "Invalid const int expression");
            }
        }
        return SemAn.describesType.get(arrType);
    }

    @Override
    public SemType visit(AstAtomType atomType, Mode mode) {
        SemType type = switch (atomType.kind) {
            case INT -> new SemInt();
            case CHAR -> new SemChar();
            case VOID -> new SemVoid();
        };
        SemAn.describesType.put(atomType, type);
        return SemAn.describesType.get(atomType);
    }

    @Override
    public SemType visit(AstPtrType ptrType, Mode mode) {
        if (mode == Mode.CHECK_TYPE) {
            SemType base = ptrType.subType.accept(this, mode);
            SemType type = new SemPtr(base);
            SemAn.describesType.put(ptrType, type);
        }
        return SemAn.describesType.get(ptrType);
    }

    private final Set<AstTypDecl> currDecls = new HashSet<>();

    @Override
    public SemType visit(AstTypeName typeName, Mode mode) {
        if (mode == Mode.CHECK_TYPE) {
            if (!(SemAn.declaredAt.get(typeName) instanceof AstTypDecl decl))
                throw error(typeName, "'" + SemAn.declaredAt.get(typeName).name + "' Is not a valid type.");
            if (currDecls.contains(decl))
                throw error(typeName, "Recursive type definition detected");

            SemType type;
            currDecls.add(decl);
            if (decl.type instanceof AstPtrType)
                type = SemAn.declaresType.get(decl);
            else
                type = decl.type.accept(this, mode);
            type = type.actualType();
            currDecls.clear();
            SemAn.describesType.put(typeName, type);
        }
        return SemAn.describesType.get(typeName);
    }

    private boolean isSameType(SemType a, SemType b) {
        if (a.getClass() != b.getClass()) return false;

        if (a instanceof SemPtr typeA && b instanceof SemPtr typeB)
            return isSameType(typeA.baseType, typeB.baseType);
        else if (a instanceof SemArr typeA && b instanceof SemArr typeB)
            return typeA.numElems == typeB.numElems && isSameType(typeA.elemType, typeB.elemType);

        return true;
    }

    private boolean isTypeOf(SemType a, Class<?>... types) {
        for (var type : types) if (a.getClass() == type) return true;
        return false;
    }

    private Report.Error error(AST node, String error) {
        return new Report.Error("Semantic error: " + error + " (" + node.location + ").");
    }
}
