#!/usr/bin/env ruby

STRADARIO, PERCORSO = "stradario.txt", "percorso.txt"

def parseInput
  strad, path = [open(STRADARIO), open(PERCORSO)].map { |x| x.read.split('.') }
  ncit = Integer(strad[0])
  cities = strad[1...(ncit + 1)]
  dists = Hash.new
  start = ncit + 1
  for c in cities
    for c1 in cities
      dists[[c, c1]] = Integer(strad[start])
      start += 1
    end
  end
  [cities, dists, path]
end


def floyd_warshall(cities, dist)
  dim = cities.length
  floyd = old = {}
  for c1 in cities
    for c2 in cities
      old[[c1,c2]] = [dist[[c1, c2]], [c2]]
    end
  end
  for k in (1...dim)
    for c1 in cities
      for c2 in cities
        direct = old[[c1, c2]]
        before = old[[c1, cities[k-1]]]
        after = old[[cities[k-1], c2]]
        if direct[0] > 0 && direct[0] <= (before[0] + after[0])
          floyd[[c1, c2]] = direct
        elsif (before[0] + after[0]) > 0
          floyd[[c1, c2]] = [before[0] + after[0], before[1] + after[1]]
        elsif
          floyd[[c1, c2]] = -1
        end
      end
    end
    old = floyd
  end
  floyd
end

cities, dist, path = parseInput
min_dist = floyd_warshall(cities, dist)
for p in path
