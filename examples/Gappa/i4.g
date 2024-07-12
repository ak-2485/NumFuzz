@rnd = float<ieee_64, up>;
r  = sqrt(x + (y*y));
z rnd= sqrt(x + (y*y));

# the logical formula that Gappa will try (and succeed) to prove
{ x in [0.1,1000] /\ y in [0.1,1000] -> |(z - r) / r| in ? }
