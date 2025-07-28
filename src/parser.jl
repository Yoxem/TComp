module Parser

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


inp = "123 + 345 - 456 * a ^^^"

print("~~~\n")
isEntirelyMatched = match(matchEntirely, inp)

if isEntirelyMatched == false
    print("illegal tokens contained.")

end



mI = eachmatch(matchEachItem, inp)

function processKeys(x)
    keys_ = keys(x)
    return filter((i) -> x[i] != nothing, keys_)[1]
end

matchedList = map((x)->x.match, collect(mI))

groupNameList = map(processKeys, collect(mI))

list1 = [1, 2, 3]
list2 = ["a", "b", "c"]

zippedTokenList = collect(zip(matchedList, groupNameList))

print(zippedTokenList)

end

