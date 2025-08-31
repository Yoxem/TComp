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

#(prog)
parsed = Parser.totalParse(prog)
#print(parsed)
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
                        newResList = vcat(newResList, newBind)
                        push!(new_exp_args, last(newBind)[2][2])
                    else
                        push!(new_exp_args, i)
                    end
        
                end
                push!(new_exp_body, new_exp_args)

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

            #fix bug[("int", "id"), ("tmp1", "id")] => ("tmp1", "id")
            if new_exp[1] == ("int", "id")
                new_exp = new_exp[2]
            end
            new_bind = [("%let", "id"), exp[2], new_exp]
            push!(binds, new_bind)
    
    
            varNo = res[2]
            return splitLet(binds, exp[4], varNo)
        else
            return (binds, exp, varNo)
        end
    end


    raw_res =  rmComplex(prog)[1]
    raw_res_body = raw_res.body
    #fix bug[("int", "id"), ("tmp1", "id")] => ("tmp1", "id")
    if  raw_res_body[1] == ("int", "id")
        raw_res_body =  [("%return", "id"), raw_res_body[2]]
    end

    if raw_res_body[2] == "int" # ("$8", "int")
        raw_res_body =  [("%return", "id"), raw_res_body]
    end

    res = push!(raw_res.binds, raw_res_body)
    return res
end

### PASS 3 assign x86 instruction
function assignInstruction(inp)
    resList = []
    for i in inp
        @match i begin
            [("%return", "id"), (val, t_val)] => begin
                if t_val == "int"
                    val = "\$" * val
                end
                push!(resList, ["movq", val, "%rax"])
            end
            [("%let", "id"), [_ty, (id, "id")],
               [("%prime", "id"), (op, _), [(lhs, lhs_t), (rhs, rhs_t)]]] =>
            begin
                instr = ""
                ops = ["+", "-", "*", "/"]
                instrs = ["addq", "subq", "imulq", "divq"]
                opIndex = findfirst(x -> x == op, ops)
                instr = instrs[opIndex]

                if lhs_t == "int"
                    lhs = "\$" * lhs
                end

                if rhs_t == "int"
                    rhs = "\$" * rhs
                end

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

            [("%let", "id"), [_ty, (id, "id")], (val, t_val)] =>
            begin
                if t_val == "int"
                    val = "\$" * val
                end
                line = ["movq", val, id]
                push!(resList, line)
            end

            (c, "int") => begin
                c_modified = "\$" * c
                push!(resList, [c_modified])
            end

            (v, "id") => push!(resList, [v])
            _ => println("Error")
        end
    end

    return resList

end


emptyEnv = []
res = uniquifyVar(parsed, emptyEnv)
#println("PASS1", res)
res2 = explicitControlRemoveComplex(res)

#println("PASS2", Parser.prettyStringLisp(res2))
res3 = assignInstruction(res2)
#println("PASS3", res3)


# PASS4 assign home
function assignHomes(inp)
    varRegex = r"(^[^\$%].*)"
    res = []
    vars = []
    for i in inp
        orig = i[2]
        dest = i[3]
        if match(varRegex, orig) != nothing # i.e. orig is a var and not a reg.
            if !(orig in vars)
                push!(vars, orig)
            end
        end 
        if match(varRegex, dest) != nothing # i.e. dest is a var and not a reg.
            if !(dest in vars)
                push!(vars, dest)
            end
        end 
    end
    #println("ALL_VAR", vars)


    varsLength = length(vars)
    for i in inp
        instr = i[1]
        orig = i[2]
        dest = i[3]

        origIdx = findfirst(x -> x == orig,vars)
        if origIdx != nothing
            realAddressIdx = varsLength - origIdx + 1
            realAddress = "-$(realAddressIdx * 8)(%rbp)"
            orig = realAddress
        end

        destIdx = findfirst(x -> x == dest,vars)
        if destIdx != nothing
            realAddressIdx = varsLength - destIdx + 1
            realAddress = "-$(realAddressIdx * 8)(%rbp)"
            dest = realAddress
        end

        push!(res, [instr, orig, dest])
    end
    return (res, varsLength)
end


# PASS5 patch instruction (ensure "instr x(rbp) y(rbp)" not happened)
function patchInstruction(inp)
    memoryRegex = r".+[(]%rbp[)]$"
    res = []

    for i in inp
        inst = i[1]
        orig = i[2]
        dest = i[3]
        if (match(memoryRegex, orig) != nothing) & (match(memoryRegex, dest) != nothing)
            cmd1 = ["movq", orig, "%rax"]
            push!(res, cmd1)

            cmd2 = [inst, "%rax", dest]
            push!(res, cmd2)
        elseif (inst == "imulq") & (match(r"^%.+", dest) == nothing)
            cmd1 = ["movq", dest, "%rax"]
            cmd2 = ["imulq", orig, "%rax"]
            cmd3 = ["movq", "%rax", dest]
            push!(res, cmd1)
            push!(res, cmd2)
            push!(res, cmd3)

        else
            push!(res, i)
        end
    end
    return res
end

res4 = assignHomes(res3)
res4_prog = res4[1]
varNumber = res4[2]
res5 = patchInstruction(res4_prog)
#println("PASS5",res5)


## PASS6 add prelude and conclude
function preludeConclude(prog, varNumber)
    rspSubqMax = varNumber * 8

    body = "start:\n"

    for i in prog
        ln_cmd = ""
        if length(i) == 3
            ln_cmd = "\t$(i[1])\t$(i[2]), $(i[3])\n"
            body = body * ln_cmd
        end
    end
    body *= "\tjmp\tconclusion\n\n\n"

    prelude = """
    .globl	main
    main:
    	pushq %rbp
    	movq	 %rsp, %rbp\n""" * "\tsubq	 \$$rspSubqMax, %rsp\n\tjmp start\n\n"

    conclude = """\nconclusion:\n""" * "\taddq	 \$$rspSubqMax, %rsp\n\tpopq	%rbp\n\tretq"

    assemblyProg = prelude * body * conclude
    return assemblyProg
end

res6 = preludeConclude(res5, varNumber)
# println("PASS6",res6) # emit assembly code
f2 = open("./a.s", "w")
write(f2, res6) #write the assembly code


close(f)
close(f2)


end # module
