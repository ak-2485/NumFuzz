@rnd = float<ieee_64, up>;
r   = 
a20*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x + 
a19*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x + 
a18*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x + 
a17*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x + 
a16*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x + 
a15*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x + 
a14*x*x*x*x*x*x*x*x*x*x*x*x*x*x + 
a13*x*x*x*x*x*x*x*x*x*x*x*x*x + 
a12*x*x*x*x*x*x*x*x*x*x*x*x + 
a11*x*x*x*x*x*x*x*x*x*x*x + 
a10*x*x*x*x*x*x*x*x*x*x + 
a9 *x*x*x*x*x*x*x*x*x + 
a8 *x*x*x*x*x*x*x*x + 
a7 *x*x*x*x*x*x*x + 
a6 *x*x*x*x*x*x + 
a5 *x*x*x*x*x + 
a4 *x*x*x*x + 
a3 *x*x*x + a2*x*x + a1*x + a0;
z19 = rnd(a20 *x + a19);
z18 = rnd(z19 * x + a18);
z17 = rnd(z18 * x + a17); 
z16 = rnd(z17 * x + a16);
z15 = rnd(z16 * x + a15);
z14 = rnd(z15 * x + a14);
z13 = rnd(z14 * x + a13);
z12 = rnd(z13 * x + a12); 
z11 = rnd(z12 * x + a11);
z10 = rnd(z11 * x + a10); 
z9  = rnd(z10 *x + a9);
z8  = rnd(z9 * x + a8);
z7  = rnd(z8 * x + a7); 
z6  = rnd(z7 * x + a6);
z5  = rnd(z6 * x + a5);
z4  = rnd(z5 * x + a4);
z3  = rnd(z4 * x + a3);
z2  = rnd(z3 * x + a2); 
z1  = rnd(z2 * x + a1);
z   = rnd(z1 * x + a0); 

# the logical formula that Gappa will try (and succeed) to prove
{ x in [0.1,1000] /\ a0 in [0.1,1000]
  /\ a1 in [0.1,1000] /\ a2 in [0.1,1000] /\ a3 in [0.1,1000] /\ a4 in [0.1,1000] /\ a5 in [0.1,1000] /\ a6 in [0.1,1000]
  /\ a7 in [0.1,1000] /\ a8 in [0.1,1000] /\ a9 in [0.1,1000] /\ a10 in [0.1,1000] 
  /\ a11 in [0.1,1000] /\ a12 in [0.1,1000] /\ a13 in [0.1,1000] /\ a14 in [0.1,1000] /\ a15 in [0.1,1000] /\ a16 in [0.1,1000]
  /\ a17 in [0.1,1000] /\ a18 in [0.1,1000] /\ a19 in [0.1,1000] /\ a20 in [0.1,1000] -> |(r - z) / r| in ? }