# some notations for making the script readable
@rnd = float<ieee_64, up>;
r = (1/(x+1));
y = rnd(x+1);
z = rnd(1/y);

# the logical formula that Gappa will try (and succeed) to prove
{ x in [0.1,1000]  -> |(z - r) / r| in ? }
