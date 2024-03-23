
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

def prodify_list (l) :
    s= ",(".join(l[:M-1])
    s= "(" + s + "," + l[M-1]
    for i in range(0,M-1) :
        s= s + ")"
    return s

#includes

#function name
print('function fold'+str(M))
#arguments
print("(c' : !["+str(float(M-1))+"](num -o num -o M[eps64_up]num))")
print("(a :") 
print(prodify("num")+")")

#start function body
print("{")

print("let [c] = c'; ")

# pairs let
print("let (a0, as1) = a;")
for i in range(1,N-2):
    print("let (a"+ str(i)+", as" +str(i+1)+")"+ "= as" + str(i)+ ";")
print("let (a"+ str(N-2)+", a" +str(N-1)+")"+ "= as" + str(N-2)+ ";")

# pure let
print("s"+ str(M-2)+ " = c"+
    " a" + str(M-2) + " a" + str(M-1) +";")
print("let z"+ str(M-2)+" = s" + str(M-2)+ ";")
for i in range(M-3,0,-1) :
    print("s"+ str(i)+ " = c"+
    " a" + str(i) + " z" + str(i+1) +";")
    print("let z"+ str(i)+" = s" + str(i)+ ";")
print("c a0 z1")

#end function body
print("}")



