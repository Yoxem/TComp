module TComp
include("./parser.jl")
using .Parser
using Match

# For pass 2
struct SimpleExp
    binds
    body
end

inp = ARGS
f = open(ARGS[1], "r")
prog = read(f, String)       

print(prog)
parsed = Parser.totalParse(prog)
print(parsed)
tmp_var_no = 0



# Pass 1: Duplicated varname uniquified
function uniquifyVar(parsed, env)
    @match parsed begin
        #  letrec is not considered
        #[("%let", "id"), [ty, var], val, [("%lambda", "id"), args, body]] => nothing 

        [("%let", "id"), [ty, var], val, body] => 
            begin
                envNew = env
                push!(envNew, var[1]) # push x of var = ("x", "id") in newEnv
                res = [("%let", "id"),
                    [ty, uniquifyVar(var, envNew)],
                    uniquifyVar(val, env),
                    uniquifyVar(body, envNew)]
                return res
            end
        (var, "id") =>
            begin
                reversedEnv = reverse(env)
                index = length(env) - findfirst(e -> e == var, reversedEnv) + 1
                newVar = var * string(index)
                return (newVar, "id")
            end
        [("%prime", "id"), op, [lhs, rhs]] =>
            begin
                lhs_new = uniquifyVar(lhs, env)
                rhs_new = uniquifyVar(rhs, env)
                return [("%prime", "id"), op, [lhs_new, rhs_new]]
            end
        [("%call", "id"), callee, args...] =>
            begin
                unifiedCallee =  uniquifyVar(callee, env)
                unifiedArgs = map(x ->uniquifyVar(x, env), args[1])

                return vcat([("%call", "id"), unifiedCallee],  [unifiedArgs])
            end
        (c, "int") => return parsed
        _ => "Error"
    end
end

# PASS2 explicit Control and Remove Complex
function explicitControlRemoveComplex(prog)
    function rmComplex(exp)
        return rmComplexAux1(exp, 0)
    end
    function rmComplexAux1(exp, varNo)
        if exp[1] == ("%let", "id")
            res = splitLet([], exp, varNo)
            tup = rmComplexAux2(SimpleExp(res[1], res[2]), res[3])
        else
            tup = rmComplexAux2(SimpleExp([], exp), varNo) 
        end
        return tup
    end


    function rmComplexAux2(exp, varNo)

        return @match exp.body begin
            (c, "int") => return (exp, varNo)
            (v, "id") => return (exp, varNo)
            [(id, "id"), caller, callee] where (id == "%prime" || id == "%call") => 
            begin
                newResList = exp.binds
                new_exp_body = Any[(id, "id"), caller]
                new_exp_args = []

                for i in callee
                    res = rmComplexAux1(i, varNo)
                    varNo = res[2]
                    newBind = res[1].binds
                    if newBind != []
                        println("NEW_BIND:", newBind)
                        newResList = vcat(newResList, newBind)
                        push!(new_exp_args, last(newBind)[2][2])
                    else
                        push!(new_exp_args, i)
                    end
        
                end
                push!(new_exp_body, new_exp_args)
    
                println(newResList)
                newBindVar = [("int", "id"), ("tmp" * string(varNo) , "id")]
                varNo += 1 
                newBind = [("%let", "id"), newBindVar, new_exp_body]
                push!(newResList, newBind)
                return (SimpleExp(newResList, newBindVar), varNo)

            end


            _ => "Error"
        end
    end
    
    function splitLet(binds, exp, varNo)
        if exp[1] == ("%let", "id")
            res = rmComplexAux1(exp[3], varNo)
            binds = vcat(binds, res[1].binds)
            new_exp = res[1].body
            new_bind = [("%let", "id"), exp[2], new_exp]
            push!(binds, new_bind)
    
    
            varNo = res[2]
            return splitLet(binds, exp[4], varNo)
        else
            return (binds, exp, varNo)
        end
    end


    raw_res =  rmComplex(prog)[1]
    res = push!(raw_res.binds, raw_res.body)
    return res
end

### PASS 3 assign x86 instruction
function assignInstruction(inp)
    resList = []
    for i in inp
        println(i)
        @match i begin
            [("%let", "id"), [_ty, (id, "id")],
            [("%prime", "id"), (op, _), [(rhs, _), (lhs, _)]]] =>
            begin
                instr = ""
                ops = ["+", "-", "*", "/"]
                instrs = ["addq", "subq", "mulq", "divq"]
                opIndex = findfirst(x -> x == op, ops)
                instr = instrs[opIndex]

                if rhs == id
                    line1 = [instr, lhs, id]
                    push!(resList, line1)
                else
                    line1 = ["movq", lhs, id]
                    line2 = [instr, rhs, id]

                    push!(resList, line1)
                    push!(resList, line2)
                end
                

                #TODO　[("%call", "id"), (op, _), args] => ...
            end

            [("%let", "id"), [_ty, (id, "id")], (val, _)] =>
            begin
                line = ["movq", val, id]
                push!(resList, line)
            end

            (c, "int") => push!(resList, [c])

            (v, "id") => push!(resList, [v])
            _ => println("Error")
        end
    end

    return resList

end


emptyEnv = []
res = uniquifyVar(parsed, emptyEnv)
println("PASS1", res)
res2 = explicitControlRemoveComplex(res)

println("PASS2", Parser.prettyStringLisp(res2))
res3 = assignInstruction(res2)
println("PASS3", res3)


close(f)



end # module
