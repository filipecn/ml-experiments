import xml.etree.ElementTree
import os

files = os.listdir('feature_points')
categories = {}
types = {}
cn = 0
t = 0
m = []
classes = {}
cl = 0
y = []
indices = {}
ind = 0
for f in files:
    name = f
    if name[0] not in classes:
        classes[name[0]] = cl
        cl = cl + 1
    y.append(classes[name[0]])
    root = xml.etree.ElementTree.parse('feature_points/' + name).getroot()
    attributes = [x.attrib for x in root.iter('FeaturePoint')]
    l = []
    for a in attributes:
        if a['category'] + a['type'] not in indices:
            indices[a['category'] + a['type']] = ind
            ind = ind + 1
        if a['category'] not in categories:
            categories[a['category']] = cn
            cn = cn + 1
        if a['type'] not in types:
            types[a['type']] = t
            t = t + 1
matrix = []
for f in files:
    name = f
    root = xml.etree.ElementTree.parse('feature_points/' + name).getroot()
    attributes = [x.attrib for x in root.iter('FeaturePoint')]
    l = 100 * [0]
    for a in attributes:
        j = indices[a['category'] + a['type']]
        l[4 * j + 0] = a['x']
        l[4 * j + 1] = a['y']
        l[4 * j + 2] = categories[a['category']]
        l[4 * j + 2] = types[a['type']]
    matrix.append(l)
with open('data.txt', 'w') as f:
    for i in range(len(matrix)):
        f.write(' '.join([str(x) for x in matrix[i]]) + ' ' + str(y[i]) + '\n')
