# -*- coding: utf-8 -*-
import numpy as np
inputs = np.array(input().split(), dtype = 'float')
A = int(inputs[0])
B = int(inputs[1])
N = int(inputs[2])
X = input()


def sellShortCake():
    global A
    if A > 0:
        A = A - 1

def sellCheeseCake():
    global B
    if B > 0:
        B = B - 1
    

for x in X:
    if x == 'S':
        sellShortCake()
    elif x == 'C':
        sellCheeseCake()
    else :# 'E'
        if A >= B:
            sellShortCake()
        else:
            sellCheeseCake()
print(A)
print(B)