
#OUTPUT
#X1: 30
#X2: 10
#X3: 200
#X4: 2
#END

#RET 0

func main -> i32 is
    var y : i32 := 20;
    var x1 : i32 := y + 10;
    var x2 : i32 := y - 10;
    var x3 : i32 := y * 10;
    var x4 : i32 := y / 10;
    
    println("X1: %d", x1);
    println("X2: %d", x2);
    println("X3: %d", x3);
    println("X4: %d", x4);
    
    return 0;
end

