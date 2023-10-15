#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import random

def gen_int():
    return random.randint(-256, 256)

def gen_read():
    global n
    n += 1
    return "(read)"

def combine_add(e1, e2):
    return "(+ {exp1} {exp2})".format(exp1=e1, exp2=e2)

def combine_minus(e):
    return "(- {exp})".format(exp=e)

def gen_reduce_element():
    func = random.choice([gen_int, gen_int, gen_int, gen_int,
                          gen_read, gen_read, gen_read])
    return func()

def gen_reduce_element_list():
    l = random.randint(2, 10)
    return [gen_reduce_element() for i in range(l)]

def gen_one_test_case():
    global n
    es = gen_reduce_element_list()
    while len(es) > 1:
        random.shuffle(es)
        epair = [es[0], es[1]]
        es = es[2:]
        epair = map(lambda e:
                    combine_minus(e) if random.randint(0, 1) else e,
                    epair)
        new_e = combine_add(*epair)
        es.append(new_e)
    result = (es[0], n)
    return result


if __name__  == "__main__":
    import sys, os
    num_of_test_case, path = int(sys.argv[1]), sys.argv[2]

    if not os.path.exists(path):
        os.mkdir(path)

    for i in range(num_of_test_case):
        n = 0
        code, num_of_read = gen_one_test_case()
        with open(os.path.join(path, "code-%s" % i), "w") as f:
            print(code, file=f)
        with open(os.path.join(path, "code-input-%s" % i), "w") as f:
            for j in range(num_of_read):
                print(gen_int(), file=f)
