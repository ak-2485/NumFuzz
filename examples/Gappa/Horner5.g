@rnd = float<ieee_64, up>;
r   = a5*x*x*x*x*x + a4*x*x*x*x + a3*x*x*x + a2*x*x + a1*x + a0;
z4  = rnd(a5 * x + a4);
z3  = rnd(z4 * x + a3);
z2  = rnd(z3 * x + a2); 
z1  = rnd(z2 * x + a1);
z   = rnd(z1 * x + a0); 

# the logical formula that Gappa will try (and succeed) to prove
{ x in [0.1,1000] /\ a0 in [0.1,1000]
  /\ a1 in [0.1,1000] /\ a2 in [0.1,1000]
  /\ a3 in [0.1,1000] /\ a4 in [0.1,1000] /\ a5 in [0.1,1000] -> |(r - z) / r| in ? }
