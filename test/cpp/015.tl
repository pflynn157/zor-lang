func print is
    return;
end

func get_x0 -> i32 is
    return 1;
end
func get_x1(x:i32) -> i32 is
    return x;
end
func get_x3(x:i32, y:i32, z:i32) -> i32 is
    return x + y + z;
end

func main -> i32 is
    var x : i32 := get_x0();
    var y : i32 := get_x1(1);
    var z : i32 := get_x3(1, 2, 3);
    print();
    return 0;
end
