#P1.a

double = lambda m : m*2
double(2)

def doublesom(x, f):
    return f(x)*2
    
doublesom(2,double)
    
#Si no se cae son de primer orde, y debería retornar 8    

#P1.b

lis=[1,2,3]

def printing(x,y,z):
    return 1

printing(print(lis[0]), print(lis[1]), print(lis[2]))

#De esta forma imprimirá el orden en que se evalúan los argumentos 

#P1.c    

def add3(x):
    n=3
    return x+n

def add1(x):
    return x+n

add1(1)

#Si retorna un free identifier es alcance léxico, si retorna un 4 es dinámico 

#P1.d

def short_circuit(x):
    if True or print("b"):
        return True
    else: return False

print(short_circuit(11))

#Si imprime en consola el "b" esto quiere decir que no toma atajos, 
# (en caso de or nada más evalua la primera si es verdadera)

#P1.e

def identify_eval(x):
    return 1

identify_eval(1+None)

#Si retorna error de operando es evaluación temprana, si retorna 1 es perezosa

#P1.f    

def fact(x):
    def fact_acc(x, acc):
        if x == 0:
            return acc
        else:
            return fact_acc(x-1, acc*x)
    return fact_acc(x,1)
    
fact(1000000)

#Si se cae con el número grande, no soporta la optimización TRO

# P1.g  

def even(n):
    if n==0:
        return True 
    if n==1:
        return False
    return odd(n-1)

def odd(n):
    if n==0:
        return False
    if n==1:
        return True 
    return even(n-1)    

print(even(10000))

# Si se cae con el número grande no soporta la optimización TCO

