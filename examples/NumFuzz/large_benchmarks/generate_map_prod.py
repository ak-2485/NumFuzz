
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
print('function map'+str(M)+"_prod")
#arguments
print("(a :") 
print(prodify("(num,num)")+")")
print("(c' : !["+str(float(M))+"]((num,num) -o M[eps64_up]num))")

#start function body
print("{")

print("let [c] = c'; ")

# pairs let
print("let (a0, as1) = a;")
for i in range(1,N-2):
    print("let (a"+ str(i)+", as" +str(i+1)+")"+ "= as" + str(i)+ ";")
print("let (a"+ str(N-2)+", a" +str(N-1)+")"+ "= as" + str(N-2)+ ";")

# pure let
for i in range(0,M) :
    print("s"+ str(i)+ " = c"+
    " a" + str(i) +";")

l=[]
for i in range(0,N):
    l.append("s"+str(i))  
v1 = prodify_list(l)
print(v1)  

#end function body
print("}")



