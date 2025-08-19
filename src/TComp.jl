module TComp
include("./parser.jl")
using .Parser
using Match


inp = ARGS
f = open(ARGS[1], "r")
prog = read(f, String)       

print(prog)
parsed = Parser.totalParse(prog)
print(parsed)
tmp_var_no = 0



# Pass 1 duplicated varname unified
function unifyVar(parsed, env)
    @match parsed begin
        #  letrec is not considered
        #[("%let", "id"), [ty, var], val, [("%lambda", "id"), args, body]] => nothing 

        [("%let", "id"), [ty, var], val, body] => 
            begin
                envNew = env
                push!(envNew, var[1]) # push x of var = ("x", "id") in newEnv
                res = [("%let", "id"),
                    [ty, unifyVar(var, envNew)],
                    unifyVar(val, env),
                    unifyVar(body, envNew)]
                return res
            end
        (var, "id") =>
            begin
                reversedEnv = reverse(env)
                index = length(env) - findfirst(e -> e == var, reversedEnv) + 1
                return (index, "id")
            end
        [(plus, "plus"), lhs, rhs] =>
            begin
                lhs_new = unifyVar(lhs, env)
                rhs_new = unifyVar(rhs, env)
                return [(plus, "plus"), lhs_new, rhs_new] 

            end
        [(minus, "minus"), lhs, rhs] =>
            begin
                lhs_new = unifyVar(lhs, env)
                rhs_new = unifyVar(rhs, env)
                return [(minus, "minus"), lhs_new, rhs_new] 

            end
        [("%call", "id"), callee, args...] =>
            begin
                unifiedCallee =  unifyVar(callee, env)
                unifiedArgs = map(x ->unifyVar(x, env), args[1])

                return vcat([("%call", "id"), unifiedCallee],  [unifiedArgs])
            end
        (c, "int") => return parsed
        _ => "Error"
    end
end

emptyEnv = []
res = unifyVar(parsed, emptyEnv)
print(res)


close(f)



end # module
