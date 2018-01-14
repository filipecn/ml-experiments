import sys
import time
import urllib.request
import os
import random
from PIL import Image
import subprocess


def reporthook(count, block_size, total_size):
    global start_time
    if count == 0:
        start_time = time.time()
        return
    duration = time.time() - start_time
    progress_size = int(count * block_size)
    speed = int(progress_size / (1024 * duration))
    percent = int(count * block_size * 100 / total_size)
    sys.stdout.write("\r...%d%%, %d MB, %d KB/s, %d seconds passed" %
                     (percent, progress_size / (1024 * 1024), speed, duration))
    sys.stdout.flush()


# get categories
last_cat = ''
categories = []
with open('ilsvrc12_urls.txt', 'r') as f:
    lines = f.readlines()
    for l in lines:
        c = l.split('_')[0]
        if last_cat != c:
            last_cat = c
            categories.append(c)
ncategories = 1
rcategories = random.sample(set(categories), ncategories)
n = 1000
with open('ilsvrc12_urls.txt', 'r') as f:
    lines = f.readlines()
    k = n
    for l in lines:
        c = l.split('_')[0]
        if c not in rcategories:
            continue
        if c != last_cat:
            ncategories -= 1
            if ncategories < 0:
                break
            os.mkdir(c)
            last_cat = c
            k = n
        elif k <= 0:
            continue
        k -= 1
        print(l)
        try:
            req = urllib.request.Request(url=l.split()[1])
            resp = urllib.request.urlopen(req, timeout=3)
            redirected = resp.geturl() != l.split()[1]
            if redirected:
                raise Exception('Image no longer exists!')
            image = c + '/' + l.split()[0].split('_')[1] + '.jpg'
            if not os.path.exists(image):
                urllib.request.urlretrieve(l.split()[1], image, reporthook)
            Image.open(image).verify()
            result = subprocess.run(
                ['jpeginfo', '-c', image], stdout=subprocess.PIPE)
            print(result.stdout)
            if b'ERROR' in result.stdout or b'WARNING' in result.stdout:
                raise Exception('Bad Image!')
            print('')
        except Exception as e:
            k += 1
            print(e)
