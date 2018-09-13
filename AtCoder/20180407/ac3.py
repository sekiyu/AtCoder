# -*- coding: utf-8 -*-
import numpy as np
Q = int(input())


def numOfBetter(A, B):
    x = np.arange(1, A * B)
    As = x[x != A]
    Bs = x[x != B]

    counter = 0
    indices = np.ones(Bs.shape)
    for a in As:
        i = np.where((a * Bs < A * B) * indices)
        indices[i] = 0
        counter += 1
    return counter

for i in range(Q):
    A, B = input().split()
    print(numOfBetter(int(A), int(B)))

