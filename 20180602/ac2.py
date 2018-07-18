# -*- coding: utf-8 -*-
import numpy as np
N, D = list(map(int, input().split()))
X = np.array(input().split(), dtype = 'int')

ans = 0

for i in range(N - 2):
    x = X[i:]
    x = x - x[0]
    x = x[1:]
    x = x[x <= 2 * D]
    js = x[x <= D] # candidates of j
    ks = x[x > D] # candidates of k
    ans = ans + np.sum((js + D).reshape(-1, 1) >= ks)

print(ans)
