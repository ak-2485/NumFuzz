# some notations for making the script readable
@rnd = float<ieee_64, up>;
r  = 1.0/(sqrt(x+1)+sqrt(x));
r1 = rnd(sqrt(x));
r2 = rnd(x+1);
r3 = rnd(sqrt(r2));
r4 = rnd(r3 + r1);
z  = rnd(1.0/r4);

# the logical formula that Gappa will try (and succeed) to prove
{ x in [0.1,1000] /\ y in [0.1,1000] -> |(z - r) / r| in ? }
