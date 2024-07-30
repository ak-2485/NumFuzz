# some notations for making the script readable
@rnd = float<ieee_64, up>;
r = (t/(1+t));
y = rnd(1+t);
z = rnd(t/y);

# the logical formula that Gappa will try (and succeed) to prove
{ t in [0.1,1000]  -> |(z - r) / r| in ? }
