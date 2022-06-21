func main -> i32 is
    var x : i32 := 0;
    var y : i32 := 0;
    
    if x < 10 then
        println("Hello!");
    elif x < 20 then
        println("Hello 2");
        if y < 30 then
            println("HELLO");
        else
            println("HI");
        end
    elif x < 30 then
        println("Hello 3");
    else
        println("Hello 4");
    end
    println("Done!");
    
    return 0;
end
