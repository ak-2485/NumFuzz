from util import *

def gen_sum(n):
    with open(f"../sum/Sum{n}.be", "w") as f:
        f.write(linear_ctx([("v", vec_type(n))]))
        f.write(discrete_ctx([]))

        f.write(unpack_vec("v", n, "y"))

        f.write(sum_vec("v", n, "x"))
        f.write(f"x_{n}\n")

gen_sum(2)
gen_sum(3)
gen_sum(20)
gen_sum(50)
gen_sum(100)
gen_sum(200)
gen_sum(500)
gen_sum(1000)
gen_sum(2000)
