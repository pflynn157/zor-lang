func print0() is
    return;
end
func print1(x:i32) is
    return;
end
func print2(x:i32, y:i32) is
    return;
end

func main -> i32 is
    var x : i32 := 0;
    print0();
    print1(1);
    print2(1, 2);
    print1(x + 10);
    print2(x + 20, x + 30);
    return 0;
end
