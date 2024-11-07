from util import *

def gen_polyval(n):
    with open(f"../polyval/PolyVal{n}.be", "w") as f:
        f.write(linear_ctx([("a", vec_type(n + 1))]))
        f.write(discrete_ctx([("z", "dnum")]))

        f.write(unpack_vec("a", n + 1, "b"))

        for i in range(1, n + 2):
            f.write(f"let x_{i}_{1} = a_{i};\n")
            for j in range(2, i + 1):
                f.write(f"let x_{i}_{j} = dmul z x_{i}_{j - 1};\n")
            f.write(f"let x_{i} = x_{i}_{i};\n\n")

        f.write(sum_vec("x", n + 1, "y"))
        f.write(f"y_{n + 1}\n")

gen_polyval(2)
gen_polyval(3)
gen_polyval(4)
gen_polyval(10)
gen_polyval(50)