def generate_polynomial_code(n):
    # Start with the input structure and variable `z`
    code = "{"
    for i in range(0,n+1):
        code += f"(a{i}:num)"
    code += "}"
    code += " `{(z:dnum)}\n\n"


    # Generate polynomial terms with each coefficient and power of z
    term_count = 1
    for k in range(n, 0, -1):
        code += f"let x{term_count} = dmul z a{k};  // a{k} * z\n"
        term_count += 1
        for i in range(1, k):
            code += f"let x{term_count} = dmul z x{term_count - 1};  // a{k} * z^{i+1}\n"
            term_count += 1

    # Sum up all terms
    term_count = term_count - 1
    code += f"let sum1 = add a0 x{term_count};\n"
    sum_count = 2
    j = term_count
    for i in range(1, n-1):
        j = j - i 
        code += f"let sum{sum_count} = add sum{sum_count - 1} x{j};\n"
        sum_count += 1
    code += f"add sum{sum_count-1} x{j-(n-1)}"
    return code

# Example usage:
n = 50 # Degree of the polynomial
with open(f"PolyVal_check{n}.be", "w") as f:
    f.write(f"{generate_polynomial_code(n)}")
f.close()
