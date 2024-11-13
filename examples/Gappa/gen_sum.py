def generate_sum_script(n):
    script = "@rnd = float<ieee_64, up>;\n\n"
    
    # exact sum
    exact_sum = " + ".join([f"r{i}" for i in range(n)])
    script += f"\nr = ({exact_sum});  \n\n"
    
    # rounded sum
    script += f"\nz rnd= ({exact_sum});\n\n"
    
    # Add the logical formula for Gappa to prove
    script += "# the logical formula that Gappa will try (and succeed) to prove\n"
    script += "{"
    for i in range(n-1):
        script += f" r{i} in [0.1,1000] /\\ "
    script += f" r{n-1} in [0.1,1000] -> |(z - r) / r| in ?"
    script += "}"

    return script

# Generate the script for a sum with 500 elements
n = 500
with open(f"Sum{n}.g","w") as f:
    f.write(f"{generate_sum_script(n)}")
