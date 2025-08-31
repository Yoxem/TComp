graph = [['a', 'b'], ['b', 'c'], ['e', 'd'], ['e', 'a'], ['a', 'c'], ['b','e'], ['e','c']]


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

    println(verticesList)


    for i in verticesList
        i_adjacents = adjacentNodes[i]
        println(i_adjacents)
        i_adjacents_color_set = Set(map(x -> getColor(x, color), collect(i_adjacents)))
        i_adjacents_color_list = sort(collect(i_adjacents_color_set))

        if i_adjacents_color_list == [notDefined]
            color[i] = 0
        else
            tmpId = 0
            for i in i_adjacents_color_list
                if tmpId == i
                    tmpId += 1
                end
            end
            color[i] = tmpId
        end
    end

    return color

end

println(vertexColoring(graph))