@rnd = float<ieee_64, up>;
r  = sqrt((x*x) + (y*y));
r1 = rnd(x * x);
r2 = rnd(y * y);
r3 = rnd(r1 + r2);
z  = rnd(sqrt(r3));

# the logical formula that Gappa will try (and succeed) to prove
{ x in [0.1,1000] /\ y in [0.1,1000] -> |(z - r) / r| in ? }
