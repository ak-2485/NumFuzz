def generate_dp_script(n):
    script = "@rnd = float<ieee_64, up>;\n\n"
    
    # Generate exact terms for the dot product
    for i in range(n):
        script += f"r{i} = (a{i} * b{i});\n"
    
    # Sum all terms for the exact dot product
    exact_sum = " + ".join([f"r{i}" for i in range(n)])
    script += f"\nr = ({exact_sum});  \n\n"
    
    # Generate rounded terms for the dot product
    for i in range(n):
        script += f"z{i} = rnd(a{i} * b{i});\n"
    
    # Sum all terms for the rounded dot product
    rounded_sum = " + ".join([f"z{i}" for i in range(n)])
    script += f"\nz rnd= ({rounded_sum});\n\n"
    
    # Add the logical formula for Gappa to prove
    script += "# the logical formula that Gappa will try (and succeed) to prove\n"
    script += "{"
    for i in range(n):
        script += f" a{i} in [0.1,1000] /\\ b{i} in [0.1,1000] /\\"
    script += " x in [0.1,1000] -> |(z - r) / r| in ? }"

    return script

# Generate the script for a dot product of two vectors with 500 elements
n = 500
with open(f"DotProd{n}.g","w") as f:
    f.write(f"{generate_dp_script(n)}")
