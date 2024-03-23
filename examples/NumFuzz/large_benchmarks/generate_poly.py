import sys
M = int(sys.argv[1])

l1=[]
l2=[]
l3=[]

def prodify (a,n) :
    args_t = [a]*n
    s= ",(".join(args_t[:n-1])
    s= "(" + s + "," + a
    for i in range(0,n-1) :
        s= s + ")"
    return s

def prodify_list (l,n) :
    s= ",(".join(l[:n-1])
    s= "(" + s + "," + l[n-1]
    for i in range(0,n-1) :
        s= s + ")"
    return s

#includes
print('#include "mulfp.fz"')
print('#include "addfp.fz"')
print('#include "fold'+str(M+1)+'.fz"')


#function name
print('function poly'+str(M))
#arguments
print("(x' : !["+str(float(M*(M+1)/2))+"]num)")
print("(a :") 
print(prodify("num",M+1)+")")

#start function body
print("{")

print("g = fun (x2:num) {fun (y2:num) {addfp (|x2,y2|)}};")

# pairs let
print("let (a0, as1) = a;")
for i in range(1,M-1):
    print("let (a"+ str(i)+", as" +str(i+1)+")"+ "= as" + str(i)+ ";")
print("let (a"+ str(M-1)+", a" +str(M)+")"+ "= as" + str(M-1)+ ";")

# vars let
print("let [x] = x'; ")
c = 0
for i in range(M,1,-1):
    print("q"+str(c)+"= mulfp (x,x);")
    print("let q"+str(c)+"' = q"+str(c)+";")
    c+=1
    for j in range(i-1,1,-1):
        print("q"+str(c)+" = mulfp (q"+str(c-1)+"',x);")
        print("let q"+str(c)+"' = q"+str(c)+";")
        c+=1
    print("x"+str(i)+"' = mulfp (a"+str(i)+",q"+str(c-1)+"');")
    print("let x"+str(i)+" = x"+str(i)+"';")
print("x1' = mulfp (a1,x);")
print("let x1 = x1';")
    

l=[]
l.append("a0")
for i in range(1,M+1):
    l.append("x"+str(i))  
v1 = prodify_list(l,M+1)

print("fold" + str(M+1)+" [g{"+str(float(M))+"}] "+v1)


#end function body
print("}")
print('poly'+str(M))



