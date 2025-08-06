module Parser

struct ParserResult
    matched
    remained
end

OptParserResult = Union{Nothing,ParserResult}

struct Psr
    fun
end

function strng(c)
    return Psr((x)-> length(x) >= 1 ?
        (x[1][1] == c ?
            ParserResult([x[1]], x[2:end])
            : nothing)
        : nothing)
end

function typ(t)
    return Psr((x)-> length(x) >= 1 ?
        (x[1][2] == t ?
            ParserResult([x[1]], x[2:end])
            : nothing)
        : nothing)
end

(>>)(a::OptParserResult, b::Psr) = then(a, b)

function then(a, b)
    if a == nothing
        return a
    else
        return b.fun(a.remained)
    end
end

(|)(a::Psr, b::Psr) = choice(a, b)

function choice(a, b)
    return Psr((x)-> (a.fun(x) == nothing ? b.fun(x) : a.fun(x)))
end



function seq(parserLst)
    function seqAux(s)
        result  = []
        isNothing = false
        tmp = nothing

        for p in parserLst
            println("s+=+=", s)
            tmp = p.fun(s)
            println("tmp%%%%", tmp)
            if tmp == nothing
                return nothing
            else
                s = tmp.remained
                push!(result, tmp.matched[1])
                
            end
        end

        return ParserResult(result, s)
    end

    return Psr(seqAux)
end



patternList = [("int", "\\d+"),
               ("id", "[_a-zA-Z][_0-9a-zA-Z]*"),
               ("l_paren", "[\\(]"),
               ("r_paren", "[\\)]"),
               ("plus", "[+]"),
               ("minus", "[\\-]"),
               ("mul", "[\\*]"),
               ("div", "[\\\\]"),
               ("sp", "\\s+"),
               ("comma", "[,]"),
               ("amp", "[&]"),
               ("assign", "[=]"),
               ("lambda", "[=][>]"),
               ]

function combineUnit(tup)
    retString = "(?P<" * tup[1] * ">" * tup[2] * ")"
    #retStringRegex = Regex(retString)
    return retString
end

tmp = join(map(combineUnit, patternList), "|")
matchEachItem = Regex(tmp)

matchAll = "^(" * tmp * ")+\$"
matchEntirely = Regex(matchAll)

print(matchEntirely)


inp = "123 + 345 - 456 * a"
print("~~~\n")
isEntirelyMatched = match(matchEntirely, inp)

if isEntirelyMatched == false
    print("illegal tokens contained.")

end


function prettyString(ele)
    if isa(ele, String)
        return "\"" * ele * "\""
    elseif isa(ele, Tuple)
        mappedEle = map(prettyString, ele)
        mappedString = "(" * join(mappedEle, ", ") * ")"
        return mappedString
    elseif isa(ele, Array)
        mappedEle = map(prettyString, ele)
        mappedString = "[" * join(mappedEle, ", ") * "]"
        return mappedString
    elseif isa(ele, ParserResult)
        res = "ParserResult(" * prettyString(ele.matched) * "," * prettyString(ele.remained) * ")"
        return res
    else #number
        return string(ele)
    end

end

mI = eachmatch(matchEachItem, inp)

function processKeys(x)
    keys_ = keys(x)
    return filter((i) -> x[i] != nothing, keys_)[1]
end

matchedList = map((x)->x.match, collect(mI))

groupNameList = map(processKeys, collect(mI))

zippedTokenList = collect(zip(matchedList, groupNameList))

print(zippedTokenList)

withoutSpaces = filter((x)-> x[2] != "sp", zippedTokenList)
initWrapped = ParserResult([], withoutSpaces)


test1 = initWrapped >> strng("123") >> (strng("+")|strng("-"))
test2 = initWrapped >> seq([strng("123"),  strng("+")])

println(prettyString(test1))
println(prettyString(test2))

"""
term = int
exp = (term (+|-) term) | term
"""
term = typ("int")

function longExpAux(input)
    rawFunc = seq([term,  (strng("+") | strng("-")), term])
    rawRes = rawFunc.fun(input)
    if rawRes != nothing
        matched = [rawRes.matched[2], rawRes.matched[1], rawRes.matched[3]]
        res = ParserResult(matched, rawRes.remained)
        return res
    else
        return rawRes
    end
end
    
function expAux(input)
    longExp = Psr(longExpAux)
    rawFunc = longExp | term
    res = rawFunc.fun(input)

    return res

end

exp = Psr(expAux)



test3 = initWrapped >> exp
println(prettyString(test3))


end

