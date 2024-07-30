
@rnd = float<ieee_64, up>;
r  = ((x0 + x1) + (x2 + x3));
z  rnd= (((x0 + x1) + (x2 + x3)));

# the logical formula that Gappa will try (and succeed) to prove
{ x0 in [0.1,1000] /\ x1 in [0.1,1000] /\
 x2 in [0.1,1000] /\ x3 in [0.1,1000] -> |(z - r) / r| in ? }
