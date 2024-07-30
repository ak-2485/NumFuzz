@rnd = float<ieee_64, up>;
r9  = (a10 *x + a9);
r8  = (r9 * x + a8);
r7  = (r8 * x + a7);
r6  = (r7 * x + a6);
r5  = (r6 * x + a5);
r4  = (r5 * x + a4);
r3  = (r4 * x + a3);
r2  = (r3 * x + a2);
r1  = (r2 * x + a1);
r   = (r1 * x + a0);
z9  = rnd(a10 *x + a9);
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
  /\ a7 in [0.1,1000] /\ a8 in [0.1,1000] /\ a9 in [0.1,1000] /\ a10 in [0.1,1000] -> |(z - r) / r| in ? }
