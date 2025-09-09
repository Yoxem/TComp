module Parser
using Match

struct ParserResult
    matched
    pos
    txt
end

OptParserResult = Union{Nothing,ParserResult}

struct Psr
    fun
end

parserCache = Dict()

function strng(c)
    return Psr((txt, pos)-> length(txt) >= pos ?
        (txt[pos][1] == c ?
            ParserResult(txt[pos], pos+1, txt)
            : nothing)
        : nothing)
end

function typ(t)
    return Psr((txt, pos)-> length(txt) >= pos ?
        (txt[pos][2] == t ?
            ParserResult(txt[pos], pos+1, txt)
            : nothing)
        : nothing)
end

(>>)(a::OptParserResult, b::Psr) = then(a, b)

function then(a, b)
    if a == nothing
        return a
    else
        key = (objectid(b), a.pos)
        if !(key in keys(parserCache))
            res = b.fun(a.txt, a.pos)
            global parserCache[key] = res
            return res
        else
            return parserCache[key]
        end
    end
end

(|)(a::Psr, b::Psr) = choice(a, b)

function choice(a, b)
    function inner(txt, pos)
        aKey = (objectid(a), pos)
        if !(aKey in keys(parserCache))
            aRes = a.fun(txt, pos)
            global parserCache[aKey] = aRes
        else
            aRes = parserCache[aKey]
        end
        if a.fun(txt, pos) == nothing
            bKey = (objectid(b), pos)
            if !(bKey in keys(parserCache))
                bRes = b.fun(txt, pos)
                global parserCache[bKey] = bRes
            else
                bRes = parserCache[bKey]
            end
            return bRes
        else
            return aRes
        end
    end
    return Psr(inner)
end


function many0(parser)
    function many0Aux(txt, pos)
        result = []
        tmp = parser.fun(txt, pos)
        p = pos
        while tmp != nothing
            p = tmp.pos
            result = push!(result, tmp.matched) 
            tmp = parser.fun(txt, p)      
        end
        return ParserResult(result, p, txt)
    end

    return Psr(many0Aux)

end

function seq(parserLst)
    function seqAux(txt, pos)
        result  = []
        isNothing = false
        tmp = nothing

        for p in parserLst
            tmp = p.fun(txt, pos)
            if tmp == nothing
                return nothing
            else
                pos = tmp.pos
                result = push!(result, tmp.matched)
                
            end
        end

        return ParserResult(result, pos, txt)
    end

    return Psr(seqAux)
end



patternList = [
               ("if", "if"),
               ("then", "then"),
               ("else", "else"),
               ("True", "bool"),
               ("False", "bool"),
               ("cmt", "[#][^\\r\\n]+"),
               ("int", "\\d+"),
               ("id", "[_a-zA-Z][_0-9a-zA-Z]*"),
               ("lParen", "[\\(]"),
               ("rParen", "[\\)]"),
               ("eq", "[=][=]"),
               ("ne", "[!][=]"),
               ("lt", "[<]"),
               ("le", "[<][=]"),
               ("gt", "[>]"),
               ("ge", "[>][=]"),
               ("and", "[&]"),
               ("or", "[\\|]"),
               ("not", "[!]"),
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
        res = string(prettyStringLisp(ele[1]))
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
atom = int | id | bool
func = "(" fn_args ")" "=>" body
unit = func | "(" exp ")" | atom
args = unit ("," unit)*
factor = unit "(" args ")"
unary = (-|!) factor | factor
term = (unary (*|/) term) | unary
eqcheckee = (term [(+|-) exp]) | term
logical = eqcheckee ("=="|"<"|"<="|">"|">=") eqcheckee | eqcheckee
subexp = logical ("&&"|"||") logical | logical
exp = if subexp then subexp else subexp | subexp
letexp = ty id "=" exp ";" body
body = exp | letexp
"""
atom = typ("int") | typ("id") | typ("bool")



function fnArgItemAux(input, pos)
    rawFunc = seq([typ("comma"),  typ("id")])
    rawRes = rawFunc.fun(input, pos)
    if rawRes != nothing
        matched = rawRes.matched[2]
        res = ParserResult(matched, rawRes.pos, rawRes.txt)
        return res
    else
        return nothing
    end
end
fnArgItem = Psr(fnArgItemAux)

function fnArgsAux(input, pos)
    rawFunc = seq([typ("id"), many0(fnArgItem)])
    res = rawFunc.fun(input, pos)
    if res != nothing
        matched = vcat([res.matched[1]], res.matched[2])
        res = ParserResult(matched, res.pos, res.txt)
        return res
    else
        return nothing
    end
end
fnArgs = Psr(fnArgsAux)

function funcAux(input, pos)
    rawFunc = seq([typ("lParen"),  fnArgs, typ("rParen"), typ("lambda"), body])
    rawRes = rawFunc.fun(input, pos)
    if rawRes != nothing
        matched = [("%lambda", "id"), rawRes.matched[2], rawRes.matched[5]]
        res = ParserResult(matched, rawRes.pos, rawRes.txt)
        return res
    else
        return nothing
    end
end

function longUnitAux(input, pos)
    rawFunc = seq([typ("lParen"), exp, typ("rParen")])
    rawRes = rawFunc.fun(input, pos)
    if rawRes != nothing
        #fix for tree fix problem
        matched = [("%wrapper", "id"), rawRes.matched[2]]
        
        res = ParserResult(matched, rawRes.pos, rawRes.txt)
        return res
    else
        return nothing
    end
end
function unitAux(input, pos)
    fun = Psr(funcAux)
    longUnit = Psr(longUnitAux)
    rawFunc = fun | longUnit | atom
    res = rawFunc.fun(input, pos)
    return res
end
unit = Psr(unitAux)


function argItemAux(input, pos)
    rawFunc = seq([typ("comma"), exp])
    rawRes = rawFunc.fun(input, pos)
    if rawRes != nothing
        matched = rawRes.matched[2]
        res = ParserResult(matched, rawRes.pos, rawRes.txt)
        return res
    else
        return nothing
    end
end
argItem = Psr(argItemAux)

function argsAux(input, pos)
    rawFunc = seq([exp, many0(argItem)])
    res = rawFunc.fun(input, pos)
    if res != nothing
        matched = vcat([res.matched[1]], res.matched[2])
        res = ParserResult(matched, res.pos, res.txt)
    end
    return res
end
args = Psr(argsAux)


function longFactorAux(input, pos)
    rawFunc = seq([unit, typ("lParen"),  args, typ("rParen")])
    rawRes = rawFunc.fun(input, pos)
    if rawRes != nothing
        matched = [("%call", "id"), rawRes.matched[1], rawRes.matched[3]]
        res = ParserResult(matched, rawRes.pos, rawRes.txt)
        return res
    else
        return nothing
    end
end

function factorAux(input, pos)
    longFactor = Psr(longFactorAux)
    rawFunc = longFactor | unit
    res = rawFunc.fun(input, pos)
    return res
end

factor = Psr(factorAux)

function longUnaryAux(input, pos)
    rawFunc = seq([(typ("minus") | typ("not")), factor])
    rawRes = rawFunc.fun(input, pos)
    if rawRes != nothing
        rator = rawRes.matched[1]
        rand = rawRes.matched[2]
        if rator[2] == "minus" # -a -> 0 - a
            matched = [("%prime", "id"), rator, [("0", "int"), rand]]
        else
            matched = [rator, rand]
        end
            res = ParserResult(matched, rawRes.pos, rawRes.txt)
        return res
    else
        return nothing
    end
end

function unaryAux(input, pos)
    longUnary = Psr(longUnaryAux)
    rawFunc = longUnary | factor
    res = rawFunc.fun(input, pos)
    return res
end

unary = Psr(unaryAux)

function longTermAux(input, pos)
    rawFunc = seq([unary,  (typ("mul") | typ("div")), term])
    rawRes = rawFunc.fun(input, pos)
    if rawRes != nothing
        #correct the tree a /(b / c) -> (a / b) / c
        leftRator = rawRes.matched[2]
        a = rawRes.matched[1]
        bc = rawRes.matched[3]
        matched = @match bc begin
            [("%prime", "id"), (rightRator, tRightRator), [b, c]] where ((rightRator == "*") || (rightRator == "/")) =>
            begin
                [("%prime", "id"), (rightRator, tRightRator), [[("%prime", "id"), leftRator, [a, b]], c]]
            end
            _ => begin
                 [("%prime", "id"), leftRator, [a, bc]]

            end

        end

        #matched = [("%prime", "id"), leftRator, [a, bc]]
        res = ParserResult(matched, rawRes.pos, rawRes.txt)
        return res
    else
        return nothing
    end
end

function termAux(input, pos)
    longTerm = Psr(longTermAux)
    rawFunc = longTerm | unary
    res = rawFunc.fun(input, pos)
    return res
end

term = Psr(termAux)


function longEqCheckeeAux(input, pos)
    rawFunc = seq([term,  (typ("plus") | typ("minus")), eqCheckee])
    rawRes = rawFunc.fun(input, pos)
    if rawRes != nothing
        #correct the tree a -(b - c) -> (a - b) - c
        leftRator = rawRes.matched[2]
        a = rawRes.matched[1]
        bc = rawRes.matched[3]
        matched = @match bc begin
            [("%prime", "id"), (rightRator, tRightRator), [b, c]] where ((rightRator == "+") || (rightRator == "-")) =>
            begin
                [("%prime", "id"), (rightRator, tRightRator), [[("%prime", "id"), leftRator, [a, b]], c]]
            end
            _ => begin
                 [("%prime", "id"), leftRator, [a, bc]]

            end

        end

        #matched = [("%prime", "id"), leftRator, [a, bc]]
        res = ParserResult(matched, rawRes.pos, rawRes.txt)
        return res
    else
        return nothing
    end
end
    
function eqCheckeeAux(input, pos)
    longEqCheckee = Psr(longEqCheckeeAux)
    rawFunc = longEqCheckee | term
    res = rawFunc.fun(input, pos)
    return res
end
eqCheckee = Psr(eqCheckeeAux)


function longlogicalAux(input, pos)
    rawFunc = seq([eqCheckee,  (typ("eq") | typ("ne")|typ("lt") | typ("gt")|typ("le") | typ("ge")), eqCheckee])
    rawRes = rawFunc.fun(input, pos)
    if rawRes != nothing
        rator = rawRes.matched[2]
        l = rawRes.matched[1]
        r = rawRes.matched[3]
        matched = [rator, l, r]
        res = ParserResult(matched, rawRes.pos, rawRes.txt)
        return res
    else
        return nothing
    end
end
    
function logicalAux(input, pos)
    longLogical = Psr(longlogicalAux)
    rawFunc = longLogical | eqCheckee
    res = rawFunc.fun(input, pos)
    return res
end
logical = Psr(logicalAux)

function longSubExpAux(input, pos)
    rawFunc = seq([logical,  (typ("and") | typ("or")), logical])
    rawRes = rawFunc.fun(input, pos)
    if rawRes != nothing
        rator = rawRes.matched[2]
        l = rawRes.matched[1]
        r = rawRes.matched[3]
        matched = [rator, l, r]
        res = ParserResult(matched,  rawRes.pos, rawRes.txt)
        return res
    else
        return nothing
    end
end
    
function subExpAux(input, pos)
    longSubExp = Psr(longSubExpAux)
    rawFunc = longSubExp | logical
    res = rawFunc.fun(input, pos)
    return res
end
subExp = Psr(subExpAux)

function longExpAux(input, pos)
    rawFunc = seq([typ("if"), exp,
                    typ("then"), body,
                    typ("else"), body])
    rawRes = rawFunc.fun(input, pos)
    if rawRes != nothing
        cond = rawRes.matched[2]
        branch1 = rawRes.matched[4]
        branch2 = rawRes.matched[6]
        matched = [("%if", "id"), cond, branch1, branch2]
        res = ParserResult(matched, rawRes.pos, rawRes.txt)
        return res
    else
        return nothing
    end
end
    
function expAux(input, pos)
    longExp = Psr(longExpAux)
    rawFunc = longExp | subExp
    res = rawFunc.fun(input, pos)
    return res
end
exp = Psr(expAux)



"""tyOfArgs = "(" ty ("," ty)* ")"
tyHead = tyOfArgs | tyOfFn | id
tyOfFn = "(" tyHead -> ty ")"
ty = id | tyOfFn """

function tyArgItemAux(input, pos)
    rawFunc = seq([typ("comma"), ty])
    rawRes = rawFunc.fun(input, pos)
    if rawRes != nothing
        matched = rawRes.matched[2]
        res = ParserResult(matched, rawRes.pos, rawRes.txt)
        return res
    else
        return nothing
    end
end


function tyOfArgsAux(input, pos)
    tyArgItem = Psr(tyArgItemAux)
    rawFunc = seq([typ("lParen"), ty, many0(tyArgItem), typ("rParen")])
    res = rawFunc.fun(input, pos)
    if res != nothing
        matched = vcat([("%argType")], vcat([res.matched[2]], res.matched[3]))
        res = ParserResult(matched, res.remained)
        return res
    else
        return nothing
    end
end

function tyHeadAux(input, pos)
    tyOfArgs  = Psr(tyOfArgsAux)
    tyOfFn = Psr(tyOfFnAux)
    rawFunc = tyOfArgs | tyOfFn | typ("id")
    res = rawFunc.fun(input, pos)
    return res
end

function tyOfFnAux(input, pos)
    tyHead = Psr(tyHeadAux)
    rawFunc = seq([typ("lParen"), tyHead, typ("funType"), ty, typ("rParen")])
    rawRes = rawFunc.fun(input, pos)
    if rawRes != nothing
        matched = [("%funType", "id"), rawRes.matched[2], rawRes.matched[4]]
        res = ParserResult(matched, rawRes.pos, rawRes.txt)
        return res
    else
        return nothing
    end
end



function tyAux(input, pos)
    tyOfFn= Psr(tyOfFnAux)
    rawFunc = tyOfFn | typ("id")
    res = rawFunc.fun(input, pos)
    return res
end
ty = Psr(tyAux)

function letExpAux(input, pos)
    #id id "=" exp ";" body
    rawFunc = seq([ty, typ("id"), typ("assign"), exp, typ("semicolon"), body])
    rawRes = rawFunc.fun(input, pos)
    if rawRes != nothing
        typ_matched = rawRes.matched[1]
        var_matched = rawRes.matched[2]
        val_matched = rawRes.matched[4]
        body_matched = rawRes.matched[6]

        matched = [("%let", "id"), [typ_matched, var_matched], val_matched, body_matched]
        res = ParserResult(matched, rawRes.pos, rawRes.txt)
        return res
    else
        return nothing
    end
end

letExp = Psr(letExpAux)

function bodyAux(input, pos)
    rawFunc = letExp | exp

    key = (objectid(rawFunc), pos)
    if !(key in keys(parserCache))
        res = rawFunc.fun(input, pos)
        global parserCache[key] = res
    else
        res = parserCache[key]
    end
    return res
end

body = Psr(bodyAux)


function fixTree(item)
    return @match item begin
        (val, t) => item
        [("%wrapper", "id"), inner] => fixTree(inner)
        [vars...] => map(fixTree, item)
        _ => println("parse Error in fixTree")
    end
end


function totalParse(prog)
    isEntirelyMatched = match(matchEntirely, prog)
    if isEntirelyMatched == false
        throw("illegal tokens contained.")
    end

    mI = eachmatch(matchEachItem, prog)
    matchedList = map((x)->x.match, collect(mI))
    groupNameList = map(processKeys, collect(mI))
    zippedTokenList = collect(zip(matchedList, groupNameList))

    withoutSpaceCmt = filter((x)-> (x[2] != "sp") & (x[2] != "cmt"), zippedTokenList)
    initWrapped = ParserResult([], 1, withoutSpaceCmt)
    res = initWrapped >> body

    res2 = fixTree(res.matched)

    return res2
end


end
