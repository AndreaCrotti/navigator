#!/usr/bin/env python
# encoding: utf-8

import sys
import os
import unittest
import gen_input


class TestNav(unittest.TestCase):
    def setUp(self):
        pass    
    
    def testMapOutput(self):
        """generates some files and checks if they have the right size"""
        for n in range(2,100):
            gen_input.generate(n, 10, 5)
            currLen = len(open("stradario.txt").read().split('.')) - 1
            shouldLen = 1 + n + (n * n)
            self.assertEqual(currLen, shouldLen)
    
    def testPathOutput(self):
        """Checks the right length of the path"""
        for l in range(1,100):
            gen_input.generate(10, l, 5)
            currLen = len(open("percorso.txt").read().split('.')) - 1
            self.assertEqual(l, currLen)
    
    def correctness(self):
        """Checks that every city in the path is present"""
        pass
            

if __name__ == '__main__':
	unittest.main()