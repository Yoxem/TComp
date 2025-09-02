module VertexColoring

#graph = [['a', 'b'], ['b', 'c'], ['e', 'd'], ['e', 'a'], ['a', 'c'], ['b','e'], ['e','c']]


function vertexColoring(graph)
    notDefined = -1

    function getColor(v, color)
        if !(v in keys(color))
            return -1
        else
            return color[v]
        end
    end

    vertices = Set(vcat(graph...))
    verticesList = collect(vertices)

    verticesMapping = map(x -> [x, Set()], verticesList)

    adjacentNodes = Dict(verticesMapping)

    for link in graph
        a = link[1]
        b = link[2]
        push!(adjacentNodes[a], b)
        push!(adjacentNodes[b], a)
    end

    sort!(verticesList, by=x -> length(adjacentNodes[x]), rev=true)

    color = Dict()



    tmpId = -1
    for i in verticesList
        i_adjacents = adjacentNodes[i]
        i_adjacents_color_set = Set(map(x -> getColor(x, color), collect(i_adjacents)))
        i_adjacents_color_list = sort(collect(i_adjacents_color_set))

        if i_adjacents_color_list == [notDefined]
            color[i] = 2
        else
            tmpId = 2
            for i in i_adjacents_color_list
                if tmpId == i
                    tmpId += 1
                end
            end
            color[i] = tmpId
        end
    end

    maxColorId = tmpId

    #force gDict["%rax"] = 1
    color["%rax"] = 1
    return (color, maxColorId)

end

#println(vertexColoring(graph))

# Disabled = force the color id for %rax set to 1
"""function correctGraph(gDict)
    raxOrigId = gDict["%rax"]
    for node in keys(gDict)
        if gDict[node] == raxOrigId
            gDict[node] = 1
        elseif gDict[node] == 1
            gDict[node] = raxOrigId
        end
    end
    return gDict
end"""
end