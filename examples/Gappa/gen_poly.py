def generate_straightforward_script(order):
    script = "@rnd = float<ieee_64, up>;\n\n"
    
    # Generate terms without rounding
    for i in range(order + 1):
        if i == 0:
            script += f"r{i} = a{i};\n"
        else:
            term = " * ".join(["x"] * i)  # Creates x^i by multiplying x i times
            script += f"r{i} = (a{i} * {term});  // a{i} * x^{i}\n"

    # Sum up all terms for the exact polynomial
    exact_sum = " + ".join([f"r{i}" for i in range(order + 1)])
    script += f"\nr = ({exact_sum});\n\n"

    # Generate terms with rounding
    for i in range(order + 1):
        if i == 0:
            script += f"z{i} = rnd(a{i});\n"
        else:
            term = " * ".join(["x"] * i)
            script += f"z{i} = rnd(a{i} * {term});  // a{i} * x^{i}\n"

    # Sum up all terms for the rounded polynomial
    rounded_sum = " + ".join([f"z{i}" for i in range(order + 1)])
    script += f"\nz = rnd({rounded_sum});\n\n"

    # Add the logical formula for Gappa to prove
    script += "# the logical formula that Gappa will try (and succeed) to prove\n"
    script += "{ x in [0.1,1000]"
    for i in range(order + 1):
        script += f" /\\ a{i} in [0.1,1000]"
    script += " -> |(z - r) / r| in ? }"

    return script

# Generate the script for a 500th-order polynomial
order = 500
straightforward_script = generate_straightforward_script(order)
print(straightforward_script)

