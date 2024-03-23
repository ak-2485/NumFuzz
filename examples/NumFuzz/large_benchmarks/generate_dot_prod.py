
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
print('#include "combine'+str(M)+'.fz"')
print('#include "map'+str(M)+'_prod.fz"')
print('#include "mulfp.fz"')
print('#include "addfp.fz"')
print('#include "fold'+str(M)+'.fz"')

#function name
print('function dotprod'+str(M))
#arguments
print("(a :") 
print(prodify("num")+")")
print("(b : ")
print(prodify("num")+")")

#start function body
print("{")

print("v = combine"+str(M)+ " a b  ;")
print("v' = map"+str(M)+"_prod v [mulfp{"+str(float(M))+"}];")

print("let (a0', as1) = v';")
# pairs let
for i in range(1,N-2):
    print("let (a"+ str(i)+"', as" +str(i+1)+")"+ "= as" + str(i)+ ";")
print("let (a"+ str(N-2)+"', a" +str(N-1)+"')"+ "= as" + str(N-2)+ ";")

# monadic let
for i in range(0,N):
    print("let a"+ str(i)+" = a" + str(i)+ "';")

l=[]
for i in range(0,N):
    l.append("a"+str(i))  
v1 = prodify_list(l)
print("v1 = " + v1+";")    
for i in range(0,M) :
    l2.append(",".join(l3[i*M:(i+1)*M]))

print("g = fun (x: num) {fun (y: num) {addfp (|x,y|)}};")
print("fold"+str((M))+ "[g{"+str(float(M-1))+"}] v1") 

print("}")
#end function body



        
