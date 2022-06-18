
#OUTPUT
#CONST: 20, 30
#END

#RET 0

const Top : i32 := 30;

func main -> i32 is
    const x : i32 := 20;
    
    println("CONST: %d, %d", x, Top);
    
    return 0;
end
