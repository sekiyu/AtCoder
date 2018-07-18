# -*- coding: utf-8 -*-
import numpy as np
inputs0 = np.array(input().split(), dtype = 'int')
H = int(inputs0[0])
W = int(inputs0[1])

inputs1 = np.array(input().split(), dtype = 'int')
N = int(inputs1[0])
M = int(inputs1[1])
As = []
for i in range(N):
    As.append(input())

unpainted = 0
for Ai in As:
    None

print(H * W - unpainted)