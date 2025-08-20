prog1 = ["prm", "+", [7, ["+", [5, 4]]]]
prog2 = ["prm","+", [["prm", "+", [5, 4]], ["prm","+", [7, 8]]]]

prog3 = ["prm","+", [["prm","+", [["cal", "foo", 999], 4]], 7]]
prog4 = ["prm","+", [1, 2]]

prog5 = 5

for i in prog4[2]
    println(i)
end

struct SimpleExp
    binds
    body
end


function rmComplex(exp)
    return rmComplexAux1(exp, 0)
end




function rmComplexAux1(exp, varNo)
    if exp[1] == "let"
        res = splitLet([], exp, varNo)
        println("RES=~=~", res)
        tup = rmComplexAux2(SimpleExp(res[1], res[2]), res[3])
    
    else
        tup = rmComplexAux2(SimpleExp([], exp), varNo)
        
    end
    return tup
end

function rmComplexAux2(exp, varNo)
    if isa(exp.body, Int) || isa(exp.body, String)
        return (exp, varNo)
    elseif isa(exp.body, Array) & (exp.body[1] == "prm" || exp.body[1] == "cal")
        newResList = exp.binds
        new_exp_body = Any[exp.body[1], exp.body[2]]
        new_exp_args = []
        println("exp_body", exp.body)
        for i in exp.body[3]
            res = rmComplexAux1(i, varNo)
            println("res", res)
            varNo = res[2]
            newBind = res[1].binds
            if newBind != []
                println("new~~", newBind)
                newResList = vcat(newResList, newBind)
                push!(new_exp_args, last(newBind)[2])
            else
                push!(new_exp_args, i)
            end

        end
        push!(new_exp_body, new_exp_args)

        println(newResList)
        newBindVar = "tmp" * string(varNo)
        varNo += 1 
        newBind = ["%let", newBindVar, new_exp_body]
        push!(newResList, newBind)
        return (SimpleExp(newResList, newBindVar), varNo)
    else
        return (exp, varNo)
    end
end


function splitLet(binds, exp, varNo)
    if exp[1] == "let"
        res = rmComplexAux1(exp[3], varNo)
        binds = vcat(binds, res[1].binds)
        new_exp = res[1].body
        new_bind = ["%let", exp[2], new_exp]
        push!(binds, new_bind)


        varNo = res[2]
        return splitLet(binds, exp[4], varNo)
    else
        return (binds, exp, varNo)
    end
end

prog6 = ["let", "x", ["prm", "-", [10, 3]], ["let", "y", ["cal", "foo", [12]], ["prm", "+", [33, "x"]]]]
println("SPLITLET: ", splitLet([], prog6, 0))

println(rmComplex(prog1), "\n\n")

println(rmComplex(prog2), "\n\n")
println(rmComplex(prog3), "\n\n")
println(rmComplex(prog4), "\n\n")
println(rmComplex(prog5), "\n\n")
println(rmComplex(prog6), "\n\n")
