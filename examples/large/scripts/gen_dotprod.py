from util import *

def gen_dotprod(n):
    with open(f"../dotprod/DotProd{n}.be", "w") as f:
        f.write(linear_ctx([("v", vec_type(n))]))
        f.write(discrete_ctx([("w", dvec_type(n))]))

        f.write(unpack_vec("v", n, "v'"))
        f.write(unpack_vec("w", n, "w'", True))

        for i in range(1, n + 1):
            f.write(f"let x_{i} = dmul w_{i} v_{i};\n")
        f.write(sum_vec("x", n, "y"))
        f.write(f"y_{n}\n")

gen_dotprod(2)
gen_dotprod(5)
gen_dotprod(50)
gen_dotprod(100)
gen_dotprod(500)
