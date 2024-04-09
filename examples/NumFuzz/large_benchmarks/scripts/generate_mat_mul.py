
M=128
N=128

l1=[]
l2=[]
l3=[]

def prodify (a) :
    args_t = [a]*N
    s= ",(".join(args_t[:M-1])
    s= "(" + s + "," + a
    for i in range(0,M-1) :
        s= s + ")"
    return s

#includes
print('#include "dotprod'+str(M)+'.fz"')
#function name
print('function mat_mul'+str(M))
#arguments
print("(A : !["+str(float(M))+"]")
a = prodify("num")
print("( " + prodify(a) + " ))")
print("(B : !["+str(float(M))+"]")
b = prodify("num")
print("( " + prodify(b) + " ))")

#start function body
print("{")

print("/*rows of A*/")
print("let [A'] = A;")
print("let (A0, AS1) = A';")
# pairs let
for i in range(1,N-2):
    print("let (A"+ str(i)+", AS" +str(i+1)+")"+ "= AS" + str(i)+ ";")
print("let (A"+ str(N-2)+", A" +str(N-1)+")"+ "= AS" + str(N-2)+ ";")
    
print("/*columns of B*/")
print("let [B'] = B;")
print("let (B0, BS1) = B';")
# pairs let
for i in range(1,N-2):
    print("let (B"+ str(i)+", BS" +str(i+1)+")"+ "= BS" + str(i)+ ";")
print("let (B"+ str(N-2)+", B" +str(N-1)+")"+ "= BS" + str(N-2)+ ";")


print("/* cij is the dot product of the ith row of A and the jth column of B */")
for i in range(0,M) :
    for j in range(0,N):
        n = str(i)
        m = str(j)
        if len(n) == 1:
            n = "0"+n
        if len(m) == 1:
            m = "0"+m
        l1.append("c"+n+'_'+m)
        l3.append("c"+n+'_'+m)
# pure let
for j in range(0,M) :
    for i in range(0,N):
        print(str(l1[i+j*M])+" = dotprod"+str(N)+
                    " A" + str(i) + " B" + str(j)+";")

for i in range(0,M) :
    l2.append(",".join(l3[i*M:(i+1)*M]))

s="),((".join(l2)
s= "((" + s

for i in range(0,M) :
    s= s + ")"

s= s + ")"

print(s)

print("}")
#end function body
print('mat_mul'+str(M))

        


        
