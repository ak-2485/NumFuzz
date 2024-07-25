# some notations for making the script readable
@rnd32 = float<ieee_32, up>;
@rnd64 = float<ieee_64, up>;

r = 4;
K = 111e-2;

dr = (1 + (x / K));
df rnd32 = (1 + (rnd32(x / K)) );
nr = (r * x);
nf rnd32 = (r * x);

R = nr/dr;
Z rnd64 = (nf/df);

# the logical formula that Gappa will try (and succeed) to prove
{ x in [0.1,1000]  -> |(R - Z) / R| in ? }

