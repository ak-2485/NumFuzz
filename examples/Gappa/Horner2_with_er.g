@rnd = float<ieee_64, up>;
r  = (a2 * x + a1) * x + a0;
z  = rnd(rnd(rnd(a2) * rnd(x) + rnd(a1)) * rnd(x) + rnd(a0));

# the logical formula that Gappa will try (and succeed) to prove
{ x in [0.1,1000] /\ a0 in [0.1,1000]
  /\ a1 in [0.1,1000] /\ a2 in [0.1,1000] -> |(z - r) / r| in ? }
