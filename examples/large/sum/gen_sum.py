def gen_type(n):
    return "num" if n == 1 else f"(num, {gen_type(n - 1)})"

def gen_sum(n):
    with open(f"Sum{n}.be", "w") as f:
        f.write(f"{{(v : {gen_type(n)})}}\n")
        f.write(f"`{{}}\n\n")

        f.write(f"let (v1, y1) = v;\n")
        for i in range(2, n - 1):
            f.write(f"let (v{i}, y{i}) = y{i - 1};\n")
        f.write(f"let (v{n - 1}, v{n}) = y{n - 2};\n")

        f.write("\nlet x1 = v1;\n")
        for i in range(1, n):
            f.write(f"let x{i + 1} = add x{i} v{i + 1};\n")
        
        f.write(f"x{n}\n")

gen_sum(500)