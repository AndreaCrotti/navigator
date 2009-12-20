#!/usr/bin/env python

from sys import maxint
from sys import argv
from getopt import getopt

def parse_stradario(stradario):
    """parses the file containing the map"""
    toparse = open(stradario).readline().split('.')
    idx = 0
    ncities = int(toparse[idx])
    cities = toparse[1 : ncities + 1]
    idx = ncities + 1
    # main data structure used
    dst = {}
    for jdx in range(0, ncities * ncities, ncities):
        for kdx in range(ncities):
            val = int(toparse[idx + jdx + kdx])
            if val < 0:
                # FIXME other ways to handle the no link?
                val = maxint
            dst[(cities[jdx / ncities], cities[kdx])] = val
    return ncities, cities, dst

def parse_percorso(percorso):
    """parses the path"""
    return file.readline(open(percorso,'r')).split('.')[:-1]

def floyd_warshall(cities, dist):
    """returns a dictionary containing the minumum distances between cities"""
    dim = len(cities)
    old = new = {}
    # first cycle to set the initial configuration
    for c1 in cities:
        for c2 in cities:
            old[(c1, c2)] = [dist[(c1, c2)], [c2]]
    # ranging over the distance between nodes
    for k in range(1, dim):
        for c1 in cities:
            for c2 in cities:
                diretto = old[(c1, c2)]
                before = old[(c1, cities[k-1])]
                after = old[(cities[k-1], c2)]
                if diretto[0] <= (before[0] + after[0]):
                    new[(c1, c2)] = diretto
                else:
                    new[(c1, c2)] = [before[0] + after[0], before[1]+after[1]]
        old = new
    return new
    
def draw(cities, dist, path):
    """draws the graphs"""
    try:
        import pydot
    except ImportError:
        print "pydot not present, install it if you want to graph"
        return
    graph = pydot.Dot(graph_type='graph')
    nodes = []
    for c in cities:
        n = pydot.Node(c, label=c, color='black')
        if c == path[0]:
            n.set_color('red')
        elif c == path[-1]:
            n.set_color('yellow')
        elif c in path[1 : len(path)-1]:
            n.set_color('green')
        nodes.append(n)
        graph.add_node(n)
    for i in range(len(cities)):
        for j in range(i + 1, len(cities)):
            d = dist[(cities[i], cities[j])]
            if d < maxint:
                e = pydot.Edge(nodes[i], nodes[j], weight=str(d), label=str(d))
                graph.add_edge(e)
    
    pathGraph = pydot.Dot(graph_type='digraph')
    # very nice way to enumerate couples of a list
    i = 1
    for t1, t2 in zip(path, path[1:]):
        e = graph.get_edge(t1, t2)
        w = 0
        # I could have more than one edge from get_edge
        if isinstance(e, list):
            w = e[0].get_weight()
        else:
            w = e.get_weight()
        e1 = pydot.Edge(pydot.Node(t1, label=t1), pydot.Node(t2, label=t2), label=str(i)+"(" + w + ")")
        pathGraph.add_edge(e1)
        i += 1
    pathGraph.write_png('pathgraph.png')
    graph.write_png('path.png')

def usage():
    """docstring for usage"""
    print """
    ./nav.py [-s stradario] [-p path] [-d]
    nav.py gets as input (optional) a map and a path chosen and outputs the shortest path,
    optionally with option [-d] it also creates two nice graphs with pydot and open them.
    """

def main():
    """Main function"""
    strad = "stradario.txt"
    path = "percorso.txt"
    isdraw = False
    
    opts, args = getopt(argv[1:], "s:p:d")
    for o, a in opts:
        if o in '-s':
            strad = a
        if o in '-p':
            path = a
        if o in '-d':
            isdraw = True

    n, cities, dist = parse_stradario(strad)
    min_dist = floyd_warshall(cities, dist)
    percorso = parse_percorso(path)
    final_dist = 0
    tappe = [percorso[0]]
    for i in range(len(percorso)-1):
        dst = min_dist[(percorso[i],percorso[i+1])]
        final_dist += dst[0]
        tappe += dst[1]
    print '.'.join(tappe)
    print final_dist
    if isdraw:
        draw(cities, dist, tappe)

if __name__ == '__main__':
    main()
