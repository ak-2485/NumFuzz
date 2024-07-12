# some notations for making the script readable
@rnd = float<ieee_64, up>;

r = 4;
K = 111e-2;
dr = (1 + ((x / K) * (x / K)));
df rnd= (1 + ((x / K) * (x / K)));
nr = ((r * x) * x);
nf rnd= ((r * x) * x);

R = nr/dr;
Z rnd= (nf/df);

# the logical formula that Gappa will try (and succeed) to prove
{ x in [0.1,1000]  -> |(Z- R) / R| in ? }

