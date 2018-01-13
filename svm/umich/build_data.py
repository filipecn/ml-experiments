import string
with open('training.txt', 'r') as f:
    lines = f.readlines()
    words = {}
    textwords = []
    index = 0
    y = []
    for l in lines:
        l = list(l.upper())
        pl = []
        for c in l:
            if c not in string.punctuation:
                pl.append(c)
        pl = ''.join([c for c in pl]).split()
        y.append(int(pl[0]))
        tw = {}
        for i in range(1, len(pl)):
            w = pl[i]
            if w not in words:
                words[w] = index
                index = index + 1
            if w not in tw:
                tw[w] = 0
            tw[w] = tw[w] + 1
        for i in range(1, len(pl) - 2):
            w = pl[i] + "_" + pl[i + 1] + "_" + pl[i + 2]
            if w not in words:
                words[w] = index
                index = index + 1
            if w not in tw:
                tw[w] = 0
            tw[w] = tw[w] + 1
        textwords.append(tw)
    # buid matrix
    matrix = []
    for tw in textwords:
        l = len(words) * [0]
        for w in tw:
            l[words[w]] = tw[w]
        matrix.append(l)
    with open('data.txt', 'w') as o:
        for i in range(len(y)):
            o.write(' '.join([str(j) for j in matrix[i]]))
            o.write(' ' + str(y[i]) + '\n')
