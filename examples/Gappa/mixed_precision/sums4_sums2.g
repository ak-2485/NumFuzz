@rnd32 = float<ieee_32, up>;
@rnd64 = float<ieee_64, up>;
r  = (x0 + x1) + (x2 + x3);
z = rnd32 ( (rnd64 (x0 + x1)) +  (rnd32( x2 + x3) ) );

# the logical formula that Gappa will try (and succeed) to prove
{ x0 in [0.1,1000] /\ x1 in [0.1,1000] /\
 x2 in [0.1,1000] /\ x3 in [0.1,1000] -> |(r - z) / r| in ? }
