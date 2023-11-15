# some notations for making the script readable
@rnd = float<ieee_64, up>;
r  = ((x*x) + (y*y));
r1 = rnd(x * x);
r2 = rnd(y * y);
z  = rnd (r1 + r2);

# the logical formula that Gappa will try (and succeed) to prove
{ x in [0.1,1000] /\ y in [0.1,1000] -> |(r - z) / r| in ? }
