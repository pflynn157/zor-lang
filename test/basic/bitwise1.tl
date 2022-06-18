
#OUTPUT
#X1: 0
#X2: 30
#X3: 30
#END

#RET 0

extern printf(ln : string, x : i32);

func main -> i32 is
    var y : i32 := 20;
    var x1 : i32 := y & 10;
    var x2 : i32 := y | 10;
    var x3 : i32 := y ^ 10;
    
    println("X1: %d", x1);
    println("X2: %d", x2);
    println("X3: %d", x3);
    
    return 0;
end
