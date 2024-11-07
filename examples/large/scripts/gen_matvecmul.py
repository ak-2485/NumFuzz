from util import *

def gen_matvecmul(n):
    # M is an n x n matrix and v is a vector of length n
    with open(f"../matvecmul/MatVecMul{n}.be", "w") as f:
        f.write(linear_ctx([("M", mat_type(n))]))
        f.write(discrete_ctx([("v", dvec_type(n))]))

        f.write(unpack_matrix("M", n, "Y"))
        f.write(unpack_vec("v", n, "w", True))

        for i in range(1, n + 1):
            for j in range(1, n + 1):
                f.write(f"let x_{i}_{j} = dmul v_{j} M_{i}_{j};\n")
            f.write(sum_vec(f"x_{i}", n, f"u_{i}") + "\n")

        f.write(vec(lambda i: f"u_{i}_{n}", n))

gen_matvecmul(2)
gen_matvecmul(3)
gen_matvecmul(10)
gen_matvecmul(20)
