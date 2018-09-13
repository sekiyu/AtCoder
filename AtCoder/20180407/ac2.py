# -*- coding: utf-8 -*-
import numpy as np
inputs = np.array(input().split(), dtype = 'float')

diff = inputs.max() * 3 - inputs.sum()
if diff % 2 == 0:
    print(int(diff / 2.0))
else:
    print(int(np.floor(diff / 2.0)) + 2)
