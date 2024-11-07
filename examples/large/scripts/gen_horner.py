from util import *

def gen_horner(n):
    with open(f"../horner/Horner{n}.be", "w") as f:
        f.write(linear_ctx([("a", vec_type(n + 1))]))
        f.write(discrete_ctx([("z", "dnum")]))

        f.write(unpack_vec("a", n + 1, "b"))

        f.write(f"let y_1 = a_{n + 1};\n")
        for i in range(1, n + 1):
            f.write(f"let x_{i} = dmul z y_{i};\n")
            f.write(f"let y_{i + 1} = add a_{n + 1 - i} x_{i};\n")
        f.write(f"y_{n + 1}")

gen_horner(1)
gen_horner(2)
gen_horner(3)
gen_horner(50)
gen_horner(100)
