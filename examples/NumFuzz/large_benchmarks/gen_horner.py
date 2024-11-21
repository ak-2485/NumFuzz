def generate_horner_code(order):
    # Include directives at the top
    code = '#include "../float_ops/addfp64.fz"\n'
    code += '#include "../float_ops/mulfp64.fz"\n\n'
    
    # Function signature
    inputs = " ".join([f"(a{i} : num)" for i in range(order + 1)])
    code += f"function Horner{order} {inputs} (x : ![{order}.0]num)\n{{\n"
    
    # Unpack the input x
    code += "  let [x'] = x;\n"
    
    # Generate the Horner's scheme steps
    previous_z = None
    for i in range(order, 0, -1):
        if previous_z is None:  # First iteration (highest degree term)
            code += f"  s{i} = mulfp64 (a{i}, x');       // Compute a{i} * x\n"
            code += f"  let z{i} = s{i};\n"
        else:
            code += f"  s_add{i} = addfp64 (|a{i}, {previous_z}|);     // Add a{i} to the result\n"
            code += f"  let z_add{i} = s_add{i};\n"
            code += f"  s_mul{i} = mulfp64 (z_add{i}, x');       // Multiply by x\n"
            code += f"  let z_mul{i} = s_mul{i};\n"
            previous_z = f"z_mul{i}"
        previous_z = f"z{i}" if i == order else f"z_mul{i}"
    
    # Final addition for a0
    code += f"  addfp64 (|{previous_z}, a0|)           // Add a0 to the result\n"
    
    # Close function body
    code += "}\n\n"
    
    # Add the function call
    code += f"Horner{order}"
    
    return code

def save_horner_code_to_file(order):
    code = generate_horner_code(order)
    filename = f"Horner{order}_nofma.py"
    with open(filename, "w") as file:
        file.write(code)
    print(f"Code saved to {filename}")

# Example usage
order = 400  # Specify the polynomial order
save_horner_code_to_file(order)

