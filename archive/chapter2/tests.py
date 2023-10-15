#!/usr/bin/env python3

import re
import uuid
import os
import subprocess
import random


def gen_input_test_file(code_path, input_path):
    for src in os.listdir(code_path):
        fn = os.path.join(code_path, src)
        if not fn.endswith(".ss"): continue
        input_test = os.path.join(input_path, src+".in")
        with open(fn) as f:
            match = re.findall(r"read", f.read())
            with open(input_test, "w") as g:
                for i in match:
                    print(random.randint(-255, 255), file=g)

def run_r1_using_scheme(code_path, input_path, out_path):
    fns = os.listdir(code_path)
    srcs = [fn for fn in fns if fn.endswith(".ss")]
    input_files = [fn + ".in" for fn in srcs]
    for src, input_file in zip(srcs, input_files):
        test(os.path.join(code_path, src),
             os.path.join(input_path, input_file),
             out_path)

def test(src, input_file, out_path):
    tmp = "/tmp/%s.ss" % str(uuid.uuid1())
    with open(tmp, "w") as f:
        print("(define (program x) x)", file=f)
        print("(define result ", file=f)
        with open(src) as s:
            print(s.read(), file=f)
        print(")", file=f)
        print('(printf "~a" result)', file=f)
    f_in = open(input_file)
    proc = subprocess.Popen(["/usr/bin/scheme", "--script", tmp],
                            stdin=f_in,
                            stdout=subprocess.PIPE)
    result = proc.communicate()[0]
    out_fn = os.path.basename(input_file).split('.')[0]
    with open(os.path.join(out_path, out_fn), "w") as f:
        print(result.decode("utf8"), file=f)

def cmp_dir(d1, d2):
    fns = os.listdir(d1)
    for fn in fns:
        f1 = os.path.join(d1, fn)
        f2 = os.path.join(d2, fn)
        with open(f1) as g1:
            with open(f2) as g2:
                if g1.read() != g2.read():
                    return False
    return True


if __name__ == "__main__":
    run_r1_using_scheme("test/code", "test/input", "test/out1")
    run_r1_using_scheme("test/after_uniq", "test/input", "test/out2")
    print(cmp_dir("test/out1", "test/out2"))
