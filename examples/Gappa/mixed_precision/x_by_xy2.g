@rnd32 = float<ieee_32, up>;
@rnd64 = float<ieee_64, up>;
r = x / (x+y);
z = rnd64 ( x / rnd32(x+y)  );

{ x in [0.1,1000] /\ y in [0.1,1000]  ->  |(z - r)/r| in ? }