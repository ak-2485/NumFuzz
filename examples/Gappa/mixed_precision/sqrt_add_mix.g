# some notations for making the script readable
@rnd32 = float<ieee_32, up>;
@rnd64 = float<ieee_64, up>;
r  = 1.0/(sqrt(x+1)+sqrt(x));
r1 = rnd64(sqrt(x));
r2 = rnd64(x+1);
r3 = rnd64(sqrt(r2));
r4 = rnd32(r3 + r1);
z  = rnd64(1.0/r3);

# the logical formula that Gappa will try (and succeed) to prove
{ x in [0.1,1000] /\ y in [0.1,1000] -> |(r - z) / r| in ? }
