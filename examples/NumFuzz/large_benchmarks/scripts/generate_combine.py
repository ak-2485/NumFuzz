
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
print('function combine'+str(M))
#arguments
print("(a :") 
print(prodify("num")+")")
print("(b : ")
print(prodify("num")+")")

#start function body
print("{")

print("let (a0, as1) = a;")
# pairs let
for i in range(1,N-2):
    print("let (a"+ str(i)+", as" +str(i+1)+")"+ "= as" + str(i)+ ";")
print("let (a"+ str(N-2)+", a" +str(N-1)+")"+ "= as" + str(N-2)+ ";")

print("let (b0, bs1) = b;")
# pairs let
for i in range(1,N-2):
    print("let (b"+ str(i)+", bs" +str(i+1)+")"+ "= bs" + str(i)+ ";")
print("let (b"+ str(N-2)+", b" +str(N-1)+")"+ "= bs" + str(N-2)+ ";")

l=[]
for i in range(0,N):
    l.append("(a"+str(i)+", b"+str(i)+")")  
v1 = prodify_list(l)
print(v1)    

#end function body
print("}")



