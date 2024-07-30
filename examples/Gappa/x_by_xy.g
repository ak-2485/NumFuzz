# some notations for making the script readable
@rnd = float<ieee_64, up>;
r = (x/(x+y));
r1 = rnd(x+y);
z = rnd(x/r1);

# the logical formula that Gappa will try (and succeed) to prove
{ x in [0.1,1000] /\ y in [0.1,1000]  ->  |(z - r)/r| in ? }

