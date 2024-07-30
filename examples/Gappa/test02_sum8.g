@rnd = float<ieee_64, up>;
r = x0 + x1 + x2 + x3 + x4 + x5 + x6 + x7;
a = rnd(rnd(x0 + x1) + x2);
b = rnd(rnd(a + x3) + x4);
c = rnd(rnd(b + x5) + x6);
z = rnd(c + x7) ;

# the logical formula that Gappa will try (and succeed) to prove
{ x0 in [0.1,1000] /\ x1 in [0.1,1000] /\
  x2 in [0.1,1000] /\ x3 in [0.1,1000] /\
  x4 in [0.1,1000] /\ x5 in [0.1,1000] /\
  x6 in [0.1,1000] /\ x7 in [0.1,1000]  -> |(z - r) / r| in ? }
