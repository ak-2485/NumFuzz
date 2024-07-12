# some notations for making the script readable
@rnd = float<ieee_64, up>;
r = (1/(sqrt(x*x)));
r0 = rnd(x*x);
r1 = rnd(sqrt(r0));
z = rnd(1/r1);

# the logical formula that Gappa will try (and succeed) to prove
{ x in [0.1,1000]  -> |(z - r) / r| in ? }

