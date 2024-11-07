''' 
Assume n > 1
'''

# Linear vector of length n
def vec_type(n, ty: str = "num"):
    return "(" + ", ".join([ty for i in range(n)]) + ")"

# Discrete vector of length n
def dvec_type(n):
    return vec_type(n, "dnum")

# Vector of length n
# name_fn has type int -> str
def vec(name_fn, n):
    return "(" + ", ".join([f"{name_fn(i)}" for i in range(1, n + 1)]) + ")"

# Linear matrix of size nxn
def mat_type(n):
    return vec_type(n, vec_type(n))

# Discrete matrix of size nxn
def dmat_type(n):
    return vec_type(n, vec_type(n, "dnum"))

# Linear context
def linear_ctx(vars: list[str, str]):
    fmt = [f"({v} : {t})" for (v, t) in vars]
    return "{" + " ".join(fmt) + "}\n"

# Discrete context
def discrete_ctx(vars: list[(str, str)]):
    return "`" + linear_ctx(vars) + "\n"

# Unpack a vector
def unpack_vec(vec: str, n: int, temp: str, discrete: bool = False):
    let = "dlet" if discrete else "let"
    if n == 2:
        return f"{let} ({vec}_1, {vec}_2) = {vec};\n\n"
    
    s = f"{let} ({vec}_1, {temp}_1) = {vec};\n"
    for i in range(2, n - 1):
        s += f"{let} ({vec}_{i}, {temp}_{i}) = {temp}_{i - 1};\n"
    return s + f"{let} ({vec}_{n - 1}, {vec}_{n}) = {temp}_{n - 2};\n\n"

# Unpack an nxn matrix
def unpack_matrix(matrix: str, n: int, temp: str):
    s = unpack_vec(matrix, n, temp)
    for i in range(1, n + 1):
        s += unpack_vec(f"{matrix}_{i}", n, f"{temp}_{i}")[:-1]
    return s + "\n"

# Sum elements of a vector of length n
def sum_vec(vec: str, n: int, temp: str):
    s = f"let {temp}_1 = {vec}_1;\n"
    for i in range(1, n):
        s += f"let {temp}_{i + 1} = add {temp}_{i} {vec}_{i + 1};\n"
    return s