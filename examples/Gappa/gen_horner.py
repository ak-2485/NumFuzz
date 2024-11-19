def generate_horner_script(order):
    script = "@rnd = float<ieee_64, up>;\n\n"
    # Generate the exact computation without rounding
    for i in range(order - 1, -1, -1):
        if i == order - 1:
            script += f"r{i}  = (a{order} * x + a{i}); \n"
        else:
            script += f"r{i}  = (r{i+1} * x + a{i}); \n"
    script += "\n"
    # Generate the rounded computation
    for i in range(order - 1, -1, -1):
        if i == order - 1:
            script += f"z{i}  = rnd(a{order} * x + a{i});\n"
        else:
            script += f"z{i}  = rnd(z{i+1} * x + a{i});\n"
    # Add the logical formula for Gappa to prove
    script += "\n# the logical formula that Gappa will try (and succeed) to prove\n"
    script += "{ x in [0.1,1000]"
    for i in range(order + 1):
        script += f" /\\ a{i} in [0.1,1000]"
    script += " -> |(z0 - r0) / r0| in ? }"
    return script
# Generate the script for a 500th-order polynomial
order = 200
with open(f"Horner{order}.g","w") as f:
    f.write(f"{generate_horner_script(order)}")
