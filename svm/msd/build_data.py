classes = {}
classes['1996'] = 0
classes['2006'] = 0
n = 500
o = open("data.txt", 'w')
with open("YearPredictionMSD.txt", 'r') as f:
    lines = f.readlines()
    for l in lines:
        features = l.split(',')
        if features[0] in classes:
            if classes[features[0]] > n:
                continue
            classes[features[0]] = classes[features[0]] + 1
            o.write(' '.join([x for x in features]))
o.close()
