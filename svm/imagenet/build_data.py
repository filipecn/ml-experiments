import numpy as np
import mahotas
import mahotas.features
import os
import random
dirr = os.listdir('.')
dirs = []
for d in dirr:
    if os.path.isdir(d):
        dirs.append(d)
classes = random.sample(set(dirs), 2)
with open('data.txt', 'w') as f:
    for i in range(1, 53):
        f.write(str(i) + ' ')
    f.write('Y\n')
    classId = 0
    for c in classes:
        classId += 1
        images = os.listdir(c)
        for i in images:
            print(c + '/' + i)
            try:
                img = mahotas.imread(c + '/' + i)
                img = mahotas.colors.rgb2grey(img).astype(int)
                features = np.reshape(mahotas.features.haralick(img), 13 * 4)
                f.write(' '.join([str(x) for x in features]) + ' ' +
                        str(classId) + '\n')
            except Exception as e:
                print('bad image!')
