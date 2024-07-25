@rnd32 = float<ieee_32, up>;
@rnd64 = float<ieee_64, up>;
r = x / (1+x);
z = rnd64 (x / rnd32 (1 + x) );

{ x in [0.1,1000] /\ y in [0.1,1000] -> |(r - z) / r| in ? }