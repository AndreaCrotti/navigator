#!/usr/bin/env python

# This script generates the input file for the real program navigator,
# given a connection degree, the number of cities and the length
# of the path to calculate

import getopt
from random import randrange, choice
from sys import exit, argv, stdout

MAP_FILE = 'stradario.txt'
PATH_FILE = 'percorso.txt'
BADARGS = 3
DST_MIN = 1
DST_MAX = 10

def gen_cities(n_cit, fname='cities.txt'):
    """Returns n cities picked casually from file fname"""
    cities = open(fname).readlines()
    # check if less cities than possible
    if n_cit > len(cities):
        raise Exception("not enough cities")
    else:
        rand_c = []
        jump_max = len(cities) / n_cit
        # FIXME: any smarter way to add many cities randomically picked from a list?
        start = randrange(jump_max)
        rand_c.append(cities[start].strip())
        for _ in range(n_cit - 1):
            start += randrange(jump_max)
            rand_c.append(cities[start].strip())
        return rand_c


def gen_table(n_cit, conn_grade):
    """Generates the table of distances between cities,
    it basically works on the lower triangular matrix
    automatically setting the upper part
    """
    from operator import add
    def gen_distance():
        """generates a random distance if threshold reached
        Otherwise no connection => -1"""
        if randrange(0, 11) <= conn_grade:
            return randrange(DST_MIN, DST_MAX)
        else:
            return -1
    res = [None] * n_cit
    for i in range(n_cit):
        # setting to 0 we make sure the diagonal is filled of 0
        res[i] = [0] * n_cit
        for j in range(i):
            res[i][j] = res[j][i] = gen_distance()
    # FIXME heavy use of lambdas
    flat = reduce(lambda x, y: add(x, y), res)
    return '.'.join(map(lambda x: str(x), flat)) + '.'


def gen_map(cities, n_cit, conn_grade):
    """creates the random map file"""
    st = '.'.join([str(n_cit), '.'.join(cities), gen_table(n_cit, conn_grade)])
    return st


def random_path(cities, num):
    """creates a random path of length num using the array cities"""
    if len(cities) == 1:
        print "no path possible"
        return cities[0]
    i = 0
    path = []
    while i < num:
        # FIXME if num >> len(cities) very inefficient
        new = choice(cities)
        # we just generate another instance if we get the same value
        if path and new == path[-1]:
            continue
        path.append(new)
        i += 1
    return path

def gen_path(cities, lenpath):
    """writes the path obtained"""
    return '.'.join(random_path(cities, lenpath)) + '.'


def generate(n_cit, lenpath, grade, output):
    """Generations of input and writing on the output files"""
    cities = gen_cities(n_cit)
    mappa, path = gen_map(cities, n_cit, grade), gen_path(cities, lenpath)
    output[0].write(mappa)
    output[1].write(path)
    

def usage():
    """docstring for usage"""
    print """ ./gen_input.py [-n n_cities] [-r lenpath] [-g connection_grade] [stradario] [percorso] -o
        If -o is given it will print output to standard output
        All the parameters have already a default value, which is
        n_cities   => 10
        lenpath     => 10
        grade       => 5
        stradario   => "stradario.txt"
        percorso    => "percorso.txt"""
    exit(BADARGS)

if __name__ == '__main__':
    opts, args = getopt.getopt(argv[1:], 'ohr:n:g:')
    # default value
    n_cities = npath = 10
    grade = 5                   # middle level of connection grade
    for o, a in opts:
        if o in ('-h'):
            usage()
        if o in ('-r'):
            npath = int(a)
        if o == '-n':
            n_cities = int(a)
        if o == '-g':
            grade = int(a)
            if grade < 0 or grade > 10:
                usage()
        if o == '-o':
            args = [stdout, stdout]
    if not args:
        args = [open(MAP_FILE, "w"), open(PATH_FILE, "w")]
    if len(args) == 1:
        usage()
    generate(n_cities, npath, grade, args)