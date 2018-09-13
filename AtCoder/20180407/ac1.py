# -*- coding: utf-8 -*-
import numpy as np
inputs = input().split()
A = int(inputs[0])
B = int(inputs[1])
K = int(inputs[2])


if A + K < B - K:
    candidates = np.concatenate([np.arange(A, A + K), np.arange(B - K + 1, B + 1)])
else:
    candidates = np.arange(A, B + 1)
    candidates = candidates[np.logical_or(candidates < A + K, candidates > B - K)]

for s in candidates:
    print(s)
