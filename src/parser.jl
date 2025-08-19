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
            ParserResult(x[1], x[2:end])
            : nothing)
        : nothing)
end

function typ(t)
    return Psr((x)-> length(x) >= 1 ?
        (x[1][2] == t ?
            ParserResult(x[1], x[2:end])
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


function many0(parser)
    function many0Aux(s)
        result = []
        tmp = parser.fun(s)
        while tmp != nothing
            s = tmp.remained
            result = push!(result, tmp.matched) 
            tmp = parser.fun(s)           
        end
        return ParserResult(result, s)
    end

    return Psr(many0Aux)

end

function seq(parserLst)
    function seqAux(s)
        result  = []
        isNothing = false
        tmp = nothing

        for p in parserLst
            tmp = p.fun(s)
            if tmp == nothing
                return nothing
            else
                s = tmp.remained
                result = push!(result, tmp.matched)
                
            end
        end

        return ParserResult(result, s)
    end

    return Psr(seqAux)
end



patternList = [("int", "\\d+"),
               ("id", "[_a-zA-Z][_0-9a-zA-Z]*"),
               ("lParen", "[\\(]"),
               ("rParen", "[\\)]"),
               ("plus", "[+]"),
               ("funType", "[-][\\>]"),
               ("minus", "[\\-]"),
               ("mul", "[\\*]"),
               ("div", "[\\/]"),
               ("sp", "[\\n\\r ]+"),
               ("comma", "[,]"),
               ("semicolon", "[;]"),
               ("lambda", "[=][\\>]"),
               ("assign", "[=]"),
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

#println(matchEntirely)

"""
((int -> int) -> int)
 add = (x , y) => x + y;

 int a = 8; 
12 + foo(13*a,14,15)
"""


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

function prettyStringLisp(ele)
    if isa(ele, String)
        return ele
    elseif isa(ele, Tuple)
        res = prettyStringLisp(ele[1])
        return res
    elseif isa(ele, Array)
        mappedEle = map(prettyStringLisp, ele)
        mappedString = "(" * join(mappedEle, " ") * ")"
        return mappedString
    elseif isa(ele, ParserResult)
        res = prettyStringLisp(ele.matched)
        return res
    else #number
        return string(ele)
    end
end




function processKeys(x)
    keys_ = keys(x)
    return filter((i) -> x[i] != nothing, keys_)[1]
end




#test1 = initWrapped >> strng("123") >> (strng("+")|strng("-"))
#test2 = initWrapped >> seq([strng("123"),  strng("+")])

#println(prettyString(test1))
#println(prettyString(test2))

"""
atom = int | id
func = "(" fn_args ")" "=>" body
unit = func | "(" exp ")" | atom
args = unit ("," unit)*
factor = unit "(" args ")"
term = (factor (*|/) factor) | factor
exp = (term (+|-) term) | term

letexp = ty id "=" exp ";" body
body = exp | letexp
"""
atom = typ("int") | typ("id")



function fnArgItemAux(input)
    rawFunc = seq([typ("comma"),  typ("id")])
    rawRes = rawFunc.fun(input)
    if rawRes != nothing
        matched = rawRes.matched[2]
        res = ParserResult(matched, rawRes.remained)
        return res
    else
        return nothing
    end
end
fnArgItem = Psr(fnArgItemAux)

function fnArgsAux(input)
    rawFunc = seq([typ("id"), many0(fnArgItem)])
    res = rawFunc.fun(input)
    if res != nothing
        matched = vcat([res.matched[1]], res.matched[2])
        res = ParserResult(matched, res.remained)
        return res
    else
        return nothing
    end
end
fnArgs = Psr(fnArgsAux)

function funcAux(input)
    rawFunc = seq([typ("lParen"),  fnArgs, typ("rParen"), typ("lambda"), body])
    rawRes = rawFunc.fun(input)
    if rawRes != nothing
        matched = [("%lambda", "id"), rawRes.matched[2], rawRes.matched[5]]
        res = ParserResult(matched, rawRes.remained)
        return res
    else
        return nothing
    end
end

function longUnitAux(input)
    rawFunc = seq([typ("lParen"),  exp, typ("rParen")])
    rawRes = rawFunc.fun(input)
    if rawRes != nothing
        matched = rawRes.matched[2]
        res = ParserResult(matched, rawRes.remained)
        return res
    else
        return nothing
    end
end
function unitAux(input)
    fun = Psr(funcAux)
    longUnit = Psr(longUnitAux)
    rawFunc = fun | longUnit | atom
    res = rawFunc.fun(input)
    return res
end
unit = Psr(unitAux)


function argItemAux(input)
    rawFunc = seq([typ("comma"), exp])
    rawRes = rawFunc.fun(input)
    if rawRes != nothing
        matched = rawRes.matched[2]
        res = ParserResult(matched, rawRes.remained)
        return res
    else
        return nothing
    end
end
argItem = Psr(argItemAux)

function argsAux(input)
    rawFunc = seq([exp, many0(argItem)])
    res = rawFunc.fun(input)
    if res != nothing
        matched = vcat([res.matched[1]], res.matched[2])
        res = ParserResult(matched, res.remained)
    end
    return res
end
args = Psr(argsAux)


function longFactorAux(input)
    rawFunc = seq([unit, typ("lParen"),  args, typ("rParen")])
    rawRes = rawFunc.fun(input)
    if rawRes != nothing
        matched = [("%call", "id"), rawRes.matched[1], rawRes.matched[3]]
        res = ParserResult(matched, rawRes.remained)
        return res
    else
        return nothing
    end
end

function factorAux(input)
    longFactor = Psr(longFactorAux)
    rawFunc = longFactor | unit
    res = rawFunc.fun(input)
    return res
end

factor = Psr(factorAux)

function longTermAux(input)
    rawFunc = seq([factor,  (typ("mul") | typ("div")), factor])
    rawRes = rawFunc.fun(input)
    if rawRes != nothing
        matched = [rawRes.matched[2], rawRes.matched[1], rawRes.matched[3]]
        res = ParserResult(matched, rawRes.remained)
        return res
    else
        return nothing
    end
end

function termAux(input)
    longTerm = Psr(longTermAux)
    rawFunc = longTerm | factor
    res = rawFunc.fun(input)
    return res
end

term = Psr(termAux)


function longExpAux(input)
    rawFunc = seq([term,  (typ("plus") | typ("minus")), term])
    rawRes = rawFunc.fun(input)
    if rawRes != nothing
        matched = [rawRes.matched[2], rawRes.matched[1], rawRes.matched[3]]
        res = ParserResult(matched, rawRes.remained)
        return res
    else
        return nothing
    end
end
    
function expAux(input)
    longExp = Psr(longExpAux)
    rawFunc = longExp | term
    res = rawFunc.fun(input)
    return res
end
exp = Psr(expAux)


"""tyOfArgs = "(" ty ("," ty)* ")"
tyHead = tyOfArgs | tyOfFn | id
tyOfFn = "(" tyHead -> ty ")"
ty = id | tyOfFn """

function tyArgItemAux(input)
    rawFunc = seq([typ("comma"), ty])
    rawRes = rawFunc.fun(input)
    if rawRes != nothing
        matched = rawRes.matched[2]
        res = ParserResult(matched, rawRes.remained)
        return res
    else
        return nothing
    end
end


function tyOfArgsAux(input)
    tyArgItem = Psr(tyArgItemAux)
    rawFunc = seq([typ("lParen"), ty, many0(tyArgItem), typ("rParen")])
    res = rawFunc.fun(input)
    if res != nothing
        matched = vcat([("%argType")], vcat([res.matched[2]], res.matched[3]))
        res = ParserResult(matched, res.remained)
        return res
    else
        return nothing
    end
end

function tyHeadAux(input)
    tyOfArgs  = Psr(tyOfArgsAux)
    tyOfFn = Psr(tyOfFnAux)
    rawFunc = tyOfArgs | tyOfFn | typ("id")
    res = rawFunc.fun(input)
    return res
end

function tyOfFnAux(input)
    tyHead = Psr(tyHeadAux)
    rawFunc = seq([typ("lParen"), tyHead, typ("funType"), ty, typ("rParen")])
    rawRes = rawFunc.fun(input)
    if rawRes != nothing
        matched = [("%funType", "id"), rawRes.matched[2], rawRes.matched[4]]
        res = ParserResult(matched, rawRes.remained)
        return res
    else
        return nothing
    end
end



function tyAux(input)
    tyOfFn= Psr(tyOfFnAux)
    rawFunc = tyOfFn | typ("id")
    res = rawFunc.fun(input)
    return res
end
ty = Psr(tyAux)

function letExpAux(input)
    #id id "=" exp ";" body
    rawFunc = seq([ty, typ("id"), typ("assign"), exp, typ("semicolon"), body])
    rawRes = rawFunc.fun(input)
    if rawRes != nothing
        typ_matched = rawRes.matched[1]
        var_matched = rawRes.matched[2]
        val_matched = rawRes.matched[4]
        body_matched = rawRes.matched[6]

        matched = [("%let", "id"), [typ_matched, var_matched], val_matched, body_matched]
        res = ParserResult(matched, rawRes.remained)
        return res
    else
        return nothing
    end
end

letExp = Psr(letExpAux)

body = letExp | exp

function totalParse(prog)
    isEntirelyMatched = match(matchEntirely, prog)
    if isEntirelyMatched == false
        throw("illegal tokens contained.")
    end

    mI = eachmatch(matchEachItem, prog)
    matchedList = map((x)->x.match, collect(mI))
    groupNameList = map(processKeys, collect(mI))
    zippedTokenList = collect(zip(matchedList, groupNameList))
    print(zippedTokenList)

    withoutSpaces = filter((x)-> x[2] != "sp", zippedTokenList)
    initWrapped = ParserResult([], withoutSpaces)
    res = initWrapped >> body
    println(prettyStringLisp(res))
    return res.matched
end


end

